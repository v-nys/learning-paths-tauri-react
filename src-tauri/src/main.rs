// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use graphviz_rust::{cmd::Format, exec, printer::PrinterContext};
use petgraph::{
    algo::{
        toposort,
        tred::{dag_to_toposorted_adjacency_list, dag_transitive_reduction_closure},
    },
    dot::Config,
    dot::Dot,
    graph::NodeIndex,
    visit::{EdgeRef, IntoEdgeReferences},
    Graph,
};
use serde::Deserialize;
use std::{fmt, collections::HashMap, path::Path, error::Error};
use anyhow;

/* Maybe more use of references would be more idiomatic here. */

/// A single unit of learning material.
///
/// A `Node` represents knowledge that can be processed as one whole.
/// It does not need to be entirely standalone, as it can have dependencies in the form of `Edge` values.
#[derive(Deserialize, Clone)]
struct Node {
    /// An ID should be unique and is used to refer to it inside its `Cluster`.
    ///
    /// The ID also be used to refer to the node from outside its `Cluster` if namespacing is applied.
    id: String,
    /// Human-readable title for this unit of knowledge.
    ///
    /// This is not required to be unique at any level.
    title: String,
}

// TODO: check this
// is there currently no distinction between push and pull edges?
#[derive(Deserialize, Clone)]
struct Edge {
    start_id: String,
    end_id: String,
}

/// An namespaced collection of `Node`s which may link to `Node`s in different namespaces.
///
/// A `Cluster` can represent a thematic clustering (nodes are related to the same topic such as a common technology).
/// It can also represent a practical clustering (nodes make up a single project).
/// A `Cluster` has a (non-nested) namespace prefix, which can be used to refer to nodes in the `Cluster`.
/// E.g. if a `Cluster's` namespace prefix is `"foo"` and the `Cluster` contains a `Node` whose ID is `bar`, this node can be referred to as `foo__bar`.
/// The namespace and node ID are always separated by `"__"`.
#[derive(Deserialize, Clone)]
struct Cluster {
    namespace_prefix: String,
    nodes: Vec<Node>,
    edges: Vec<Edge>,
}

/// An error related to the internal structure of a (syntactically valid, semantically invalid) `Cluster`.
#[derive(Debug)]
enum StructuralError {
    // TODO: consider making making edges whose sink is an external node structural errors, as well
    DoubleNode(String), // creating two nodes with same ID
    MissingInternalEndpoint(String, String, String), // referring to non-existent node
    NodeMultipleNamespace(String), // creating a node with explicit namespace
    EdgeMultipleNamespace(String, String, String), // edge from / to internal node with
                        // explicit namespace
}

/*enum Dependency {
    Requirement(Directed),
    Motivation(Directed),
}*/

fn node_dot_attributes(
    graph: &Graph<(String, String), &str>,
    node_ref: (NodeIndex, &(String, String)),
) -> String {
    // label specified last is used, so this overrides the auto-generated one
    format!("label=\"{}\"", node_ref.1 .1)
}

/// Given a sequence of filesystem paths, deserialize the cluster represented by each path and optionally run additional validation.
/// 
/// # Parameters
/// - `paths`: A sequence of filesystem paths, represented as a single string.
/// - `check_redundant_edges`: Whether to consider redundant, i.e. implied, edges as an error.
/// - `check_cluster_boundaries`: Whether to consider unresolvable references to other clusters as an error.
/// - `check_missing_files`: Whether to consider files (associated with nodes) which are not acessible as an error.
/// 
/// # Returns
/// 
/// An association list from each component path to ...?
/// 
/// # Errors
/// 
/// The function always produces an association list, but the associated values may be errors. This is because each cluster can be analyzed in isolation.
/// 
#[tauri::command]
fn read_contents(
    paths: &str,
    check_redundant_edges: bool,
    check_cluster_boundaries: bool,
    check_missing_files: bool,
) -> Vec<(
    &str, // these are paths
    // the following is the type of svgs
    // the "outer" String is a serialized representation of a ReadError
    // the "middle" Vec<String> is a sequence of structural errors
    // the "inner" Vec<String> is e sequence of comments
    Result<Result<(String, Vec<String>), Vec<String>>, String>,
    
)> {
    eprintln!("read_contents was invoked!");
    let paths = paths.split(";");
    let read_results = paths.clone().map(std::fs::read_to_string);
    let deserialized_graphs: Vec<Result<Cluster, anyhow::Error>> = read_results
        .map(|r| match r {
            Ok(ref text) => serde_yaml::from_str(text).map_err(anyhow::Error::new),
            Err(e) => Err(anyhow::Error::new(e)),
        })
        .collect();
    eprintln!("Graphs have been deserialized.");
    // each cluster is associated with a petgraph Graph
    // so we get a vector of results
    // for every element, we could get a read error or a structural error,
    // in which case there is no association
    // REFACTOR: type of deserialized_graphs is:
    // Vec<Result<Result<(Cluster, Graph<(String, String), &str>), Vec<StructuralError>>, ReadError>>
    // Would probably be nicer if it was:
    // Vec<Result<(Cluster, Graph<(String, String), &str>), CombinedError>>
    // and nicer still if it was something like
    // Vec<Result<(Cluster, Graph<NodeData, EdgeData>), CombinedError>>
    let deserialized_graphs: Vec<_> = deserialized_graphs
        .into_iter()
        .map(|r| {
            r.map(|c| {
                let mut map = std::collections::HashMap::new();
                let mut graph = Graph::new();
                let mut structural_errors: Vec<StructuralError> = vec![];
                // these should only be internal nodes
                for node in &c.nodes {
                    let maybe_namespaced_key = node.id.clone();
                    if maybe_namespaced_key.contains("__") {
                        structural_errors
                            .push(StructuralError::NodeMultipleNamespace(maybe_namespaced_key));
                    } else {
                        let definitely_namespaced_key =
                            format!("{}__{maybe_namespaced_key}", c.namespace_prefix);
                        if !map.contains_key(&definitely_namespaced_key) {
                            let idx = graph.add_node((node.id.to_owned(), node.title.to_owned()));
                            map.insert(definitely_namespaced_key, idx);
                        } else {
                            structural_errors
                                .push(StructuralError::DoubleNode(definitely_namespaced_key));
                        }
                    }
                }

                for Edge { start_id, end_id } in &c.edges {
                    let mut can_add = true;
                    if start_id.starts_with(&format!("{}__", &c.namespace_prefix)) {
                        structural_errors.push(StructuralError::EdgeMultipleNamespace(
                            start_id.to_owned(),
                            end_id.to_owned(),
                            start_id.to_owned(),
                        ));
                        can_add = false;
                    }
                    if end_id.starts_with(&format!("{}__", &c.namespace_prefix)) {
                        structural_errors.push(StructuralError::EdgeMultipleNamespace(
                            start_id.to_owned(),
                            end_id.to_owned(),
                            start_id.to_owned(),
                        ));
                        can_add = false;
                    }
                    if can_add {
                        let mut start_id = start_id.to_owned();
                        let mut end_id = end_id.to_owned();
                        if start_id.contains("__") {
                            if !map.contains_key(&start_id) {
                                let idx = graph.add_node((start_id.clone(), start_id.clone()));
                                map.insert(start_id.clone(), idx);
                            }
                        } else {
                            start_id = format!("{}__{start_id}", c.namespace_prefix);
                        }
                        if end_id.contains("__") {
                            if !map.contains_key(&end_id) {
                                let idx = graph.add_node((end_id.clone(), end_id.clone()));
                                map.insert(end_id.clone(), idx);
                            }
                        } else {
                            end_id = format!("{}__{end_id}", c.namespace_prefix);
                        }
                        match (map.get(&start_id), map.get(&end_id)) {
                            (Some(idx1), Some(idx2)) => {
                                // don't need edge labels, so just using ""
                                graph.add_edge(*idx1, *idx2, "");
                            }
                            (Some(_), None) => {
                                structural_errors.push(StructuralError::MissingInternalEndpoint(
                                    start_id.to_owned(),
                                    end_id.to_owned(),
                                    end_id.to_owned(),
                                ));
                            }
                            (None, Some(_)) => {
                                structural_errors.push(StructuralError::MissingInternalEndpoint(
                                    start_id.to_owned(),
                                    end_id.to_owned(),
                                    start_id.to_owned(),
                                ));
                            }
                            (None, None) => {
                                structural_errors.push(StructuralError::MissingInternalEndpoint(
                                    start_id.to_owned(),
                                    end_id.to_owned(),
                                    start_id.to_owned(),
                                ));
                                structural_errors.push(StructuralError::MissingInternalEndpoint(
                                    start_id.to_owned(),
                                    end_id.to_owned(),
                                    end_id.to_owned(),
                                ));
                            }
                        }
                    }
                }
                if structural_errors.is_empty() {
                    Ok((c, graph))
                } else {
                    Err(structural_errors)
                }
            })
        })
        .collect();

    // to enable serialization and cloning: error types are converted to owned strings
    // also, references to the associations are introduced rather than clones
    // REFACTOR: serializable is only used in the next step, so make one long chain instead?
    let serializable =
        deserialized_graphs
            .iter()
            .map(|graph_with_remarks| match graph_with_remarks {
                Ok(readable) => match readable {
                    Ok(structurally_sound) => Ok(Ok(structurally_sound)),
                    Err(structural_errors) => Ok(Err(structural_errors
                        .into_iter()
                        .map(|se| format!("{:#?}", se))
                        .collect::<Vec<String>>())),
                },
                Err(read_error) => Err(format!("{:#?}", read_error)),
            });
    eprintln!("Errors have been converted into strings.");

    // cluster-graph pairs are converted into dot source code + additional comments
    // i.e. the first owned String in the Ok-values is dot source code, the accompanying vector contains remarks
    let dots: Vec<_> = serializable
        .map(|lvl1res| match lvl1res {
            Ok(lvl2res) => Ok(match lvl2res {
                Ok((_cluster, graph)) => {
                    let mut remarks: Vec<String> = vec![];
                    if check_missing_files {
                        remarks.push("Can't check for missing files yet.".to_owned());
                    }
                    Ok((
                        format!(
                            "{:?}",
                            Dot::with_attr_getters(
                                graph,
                                &[Config::EdgeNoLabel],
                                &|_g, _g_edge_ref| "".to_owned(),
                                &node_dot_attributes
                            )
                        ),
                        remarks,
                    ))
                }
                Err(e) => Err(e.clone()),
            }),
            Err(e) => Err(e.to_owned()),
        })
        .collect();
    eprintln!("Dots have been generated and remarks have been added.");

    let graph_result: Result<Vec<_>, _> = dots.clone().into_iter().collect();
    let mut paths: Vec<&str> = paths.collect();
    let mut boundary_errors = vec![];
    let complete_graph_result;
    match graph_result {
        Ok(v) => {
            let graph_result: Result<Vec<_>, _> = v.into_iter().collect();
            match graph_result {
                Ok(v) => {
                    let mut complete_graph: Graph<(String, String), &str> = Graph::new();
                    let mut complete_graph_map = HashMap::new();
                    // wait, I don't get this
                    // why does it look like I can get at cluster / graph?
                    // deserialized is a reference...
                    deserialized_graphs.iter().for_each(|deserialized| {
                        match deserialized {
                            Ok(Ok((cluster, graph))) => {
                                for (id, title) in graph.node_weights() {
                                    // only add the internal ones to the map
                                    if !id.contains("__") {
                                        let node_idx = complete_graph.add_node((
                                            format!("{}__{id}", cluster.namespace_prefix),
                                            title.to_owned(),
                                        ));
                                        complete_graph_map.insert(
                                            format!("{}__{id}", cluster.namespace_prefix),
                                            node_idx,
                                        );
                                    }
                                }
                            }
                            _ => panic!("Coding error."),
                        }
                    });
                    for key in complete_graph_map.keys() {
                        eprintln!("Key: {}", key);
                    }
                    deserialized_graphs.iter().for_each(|deserialized| {
                        match deserialized {
                            Ok(Ok((cluster, _graph))) => {
                                for Edge { start_id, end_id } in cluster.edges.iter() {
                                    // add the dependencies
                                    let namespaced_start_id = if start_id.contains("__") {
                                        start_id.to_owned()
                                    } else {
                                        format!("{}__{start_id}", cluster.namespace_prefix)
                                    };
                                    let namespaced_end_id = if end_id.contains("__") {
                                        end_id.to_owned()
                                    } else {
                                        format!("{}__{end_id}", cluster.namespace_prefix)
                                    };
                                    match (
                                        complete_graph_map.get(&namespaced_start_id),
                                        complete_graph_map.get(&namespaced_end_id),
                                    ) {
                                        (Some(start_idx), Some(end_idx)) => {
                                            complete_graph.add_edge(*start_idx, *end_idx, "");
                                        }
                                        (Some(_), None) => {
                                            boundary_errors.push(format!(
                                                "Cluster {} refers to missing external node {}",
                                                cluster.namespace_prefix, namespaced_end_id
                                            ));
                                        }
                                        (None, Some(_)) => {
                                            boundary_errors.push(format!(
                                                "Cluster {} refers to missing external node {}",
                                                cluster.namespace_prefix, namespaced_start_id
                                            ));
                                        }
                                        (None, None) => {
                                            boundary_errors.push(format!(
                                                "Cluster {} refers to missing external node {}",
                                                cluster.namespace_prefix, namespaced_end_id
                                            ));
                                            boundary_errors.push(format!(
                                                "Cluster {} refers to missing external node {}",
                                                cluster.namespace_prefix, namespaced_start_id
                                            ));
                                        }
                                    }
                                }
                            }
                            _ => panic!("Coding error."),
                        }
                    });
                    if boundary_errors.is_empty() {
                        complete_graph_result = Ok(Ok(complete_graph));
                    } else {
                        complete_graph_result = Ok(Err(boundary_errors));
                    }
                }
                Err(e) => {
                    complete_graph_result = Ok(Err(e));
                }
            }
        }
        Err(e) => {
            complete_graph_result = Err(e);
        }
    }

    eprintln!("Global graph has been represented.");
    // issue: structural issues are represented as a &Vec<String>, rather than a Vec<String>
    let mut svgs: Vec<_> = dots
        .into_iter()
        .map(|dot_result| {
            dot_result.map(|dot_with_remarks| {
                dot_with_remarks.map(|(ref dot_src, remarks)| {
                    let g = graphviz_rust::parse(dot_src)
                        .expect("Assuming petgraph generated valid dot syntax.");
                    (
                        exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])
                            .expect("Assuming valid graph can be rendered into SVG."),
                        remarks,
                    )
                })
            })
        })
        .collect();

    eprintln!("SVGs have been rendered.");
    paths.push("complete graph (currently only hard dependencies)");
    let mut comments = vec![];
    svgs.push(complete_graph_result.map(|lvl1| {
        lvl1.map(|lvl2| {
            let toposort_result = toposort(&lvl2, None)
                .expect("Currently just assuming cycle-free graph. Could take this into account.");
            let (res, _revmap): (_, Vec<NodeIndex>) =
                dag_to_toposorted_adjacency_list(&lvl2, &toposort_result);
            let (tr, _tc) = dag_transitive_reduction_closure(&res);
            eprintln!("{:#?}", tr);
            for edge in res.edge_references() {
                let source = edge.source();
                let target = edge.target();
                if !tr.contains_edge(source, target) {
                    comments.push(format!(
                        "Redundant edge! {} -> {}",
                        lvl2.node_weight(source)
                            .expect("Edge exists, so node does too.")
                            .0,
                        lvl2.node_weight(target)
                            .expect("Edge exists, so node does too.")
                            .0
                    ));
                }
            }
            let dot = format!(
                "{:?}",
                Dot::with_attr_getters(
                    &lvl2,
                    &[Config::EdgeNoLabel],
                    &|_g, _g_edge_ref| "".to_owned(),
                    &node_dot_attributes
                )
            );
            (
                exec(
                    graphviz_rust::parse(&dot)
                        .expect("Assuming petgraph generated valid dot syntax."),
                    &mut PrinterContext::default(),
                    vec![Format::Svg.into()],
                )
                .expect("Assuming valid graph can be rendered into SVG."),
                comments,
            )
        })
    }));
    // /home/vincent/Projects/tauritest/src-tauri/test/git.yaml
    let final_result = std::iter::zip(paths, svgs).collect();
    final_result
}

/// Associates the parent path of each supplied path with the path itself.
///
/// # Parameters
///
/// - `paths`: the paths to be included in the produced `Vec<&Path>` values, specified as a single semicolon-separated string.
///
/// # Returns
///
/// A hash map from parent paths to child paths.
///
/// # Errors
/// If any path lacks a parent, the first such path is returned as the error value.
/// This is the case for the root path and relative paths.
/// This also includes the case in which the input is empty.
///
/// # Notes
/// This function is useful for filesystem watching.
/// In addition to changes *inside* a watched folder, changes *to* the watched folder should be signaled as well.
/// This can be achieved by watching a watched folder's parent rather than the folder itself.
#[tauri::command]
fn associate_parents_children(paths: &'_ str) -> Result<HashMap<&'_ Path, Vec<&'_ Path>>, &'_ Path> {
    paths
        .split(";")
        .map(Path::new)
        .try_fold(HashMap::new(), |mut map, path| {
            if path.is_relative() {
                return Err(path);
            }
            let parent = path.parent().ok_or(path)?;
            map.entry(parent).or_insert_with(Vec::new).push(path);
            Ok(map)
        })
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, path::Path};

    use crate::associate_parents_children;

    #[test]
    fn associate_empty_string() {
        let result = associate_parents_children("");
        assert_eq!(result, Err(Path::new("")));
    }

    #[test]
    fn associate_empty_strings() {
        let result = associate_parents_children(";;");
        assert_eq!(result, Err(Path::new("")));
    }

    #[test]
    fn associate_root() {
        let result = associate_parents_children("/");
        assert_eq!(result, Err(Path::new("/")));
    }

    #[test]
    fn associate_bad_path() {
        let result = associate_parents_children("/home/user/folder1;folder2");
        assert_eq!(result, Err(Path::new("folder2")));
    }

    #[test]
    fn associate_valid_paths() {
        let result = associate_parents_children("/home/user/folder1;/var/folder2");
        assert_eq!(
            result,
            Ok(HashMap::from([
                (
                    Path::new("/home/user"),
                    vec![Path::new("/home/user/folder1")]
                ),
                (Path::new("/var"), vec![Path::new("/var/folder2")])
            ]))
        );
    }

    #[test]
    fn associate_parent_multiple_children() {
        let result = associate_parents_children("/home/user/folder1;/home/user/folder2");
        assert_eq!(
            result,
            Ok(HashMap::from([(
                Path::new("/home/user"),
                vec![
                    Path::new("/home/user/folder1"),
                    Path::new("/home/user/folder2")
                ]
            )]))
        );
    }
}

fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_fs_watch::init())
        .invoke_handler(tauri::generate_handler![read_contents, associate_parents_children])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
