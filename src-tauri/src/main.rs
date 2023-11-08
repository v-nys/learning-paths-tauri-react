// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use anyhow;
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
use std::{collections::HashMap, fmt, path::Path};

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
    ClusterBoundary(String, String), // cluster, reference
    InvalidComponentGraph,
}

impl fmt::Display for StructuralError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StructuralError::DoubleNode(id) => write!(f, "Node defined multiple times: {id}"),
            StructuralError::MissingInternalEndpoint(start_id, end_id, missing_id) => write!(f, "Node {missing_id} mentioned in edge {start_id} → {end_id} does not exist"),
            StructuralError::NodeMultipleNamespace(id) => write!(f, "Node is explicitly namespaced (which is not allowed) in its definition: {id}"),
            StructuralError::EdgeMultipleNamespace(start_id, end_id, namespaced_id) => write!(f, "Node {namespaced_id} mentioned in edge {start_id} → {end_id} is explicitly namespaced (which is not allowed if the namespace is that of the current cluster)."),
            StructuralError::ClusterBoundary(cluster,reference) => write!(f, "Cluster {} refers to non-existent external node {}", cluster, reference),
            StructuralError::InvalidComponentGraph => write!(f, "At least one component graph is invalid")
        }
    }
}

impl std::error::Error for StructuralError {}

#[derive(Debug)]
struct StructuralErrorGrouping {
    components: Vec<StructuralError>,
}

impl fmt::Display for StructuralErrorGrouping {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.components
                .iter()
                .map(|c| { c.to_string() })
                // this is necessary because join is not directly available on Iterator
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl std::error::Error for StructuralErrorGrouping {
    // source is not mandatory and would be odd here
}

/*enum Dependency {
    Requirement(Directed),
    Motivation(Directed),
}*/

type NodeData = (String, String);
type EdgeData<'a> = &'a str;

struct ClusterGraphPair<'a>(Cluster, Graph<NodeData, EdgeData<'a>>);
struct ClusterGraphCommentsTriple<'a>(Cluster, Graph<NodeData, EdgeData<'a>>, Vec<String>);
struct ClusterGraphCommentsDotQuadruple<'a>(
    Cluster,
    Graph<NodeData, EdgeData<'a>>,
    Vec<String>,
    String,
);
struct CommentsSvgPair(Vec<String>, String);

struct ClusterGraphCommentsSvgQuadruple<'a>(
    Cluster,
    Graph<NodeData, EdgeData<'a>>,
    Vec<String>,
    String,
);

fn node_dot_attributes(_: &Graph<NodeData, EdgeData>, node_ref: (NodeIndex, &NodeData)) -> String {
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
) -> Vec<(&str, Result<(Vec<String>, String), String>)> {
    read_contents_with_reader::<RealFileReader>(paths,check_redundant_edges, check_cluster_boundaries, check_missing_files, RealFileReader {})
}

trait FileReader {
    fn read_to_string(&self, path: &str) -> std::io::Result<String>;
}

struct RealFileReader;

impl FileReader for RealFileReader {
    fn read_to_string(&self, path: &str) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
}

fn read_contents_with_reader<T: FileReader>(
    paths: &str,
    check_redundant_edges: bool,
    check_cluster_boundaries: bool,
    check_missing_files: bool,
    reader: T,
) -> Vec<(&str, Result<(Vec<String>, String), String>)> {
    eprintln!("read_contents was invoked!");
    let paths = paths.split(";");
    let read_results = paths.clone().map(|p| { reader.read_to_string(p)});
    let clusters = read_results.map(|r| match r {
        Ok(ref text) => serde_yaml::from_str(text).map_err(anyhow::Error::new),
        Err(e) => Err(anyhow::Error::new(e)),
    });
    eprintln!("Clusters have been deserialized.");
    // each cluster is associated with a petgraph Graph
    // so we get a vector of results
    let cluster_graph_pairs = clusters.map(|result| {
        result.and_then(|cluster: Cluster| {
            // ??? why do I need to annotate this?
            let mut identifier_to_index_map = std::collections::HashMap::new();
            let mut single_cluster_graph = Graph::new();
            let mut structural_errors: Vec<StructuralError> = vec![];

            // check that nodes are not explicitly namespaced (because only internal ones are mentioned)
            // also check whether nodes are mentioned only once
            for node in &cluster.nodes {
                let maybe_namespaced_key = node.id.clone();
                if maybe_namespaced_key.contains("__") {
                    structural_errors
                        .push(StructuralError::NodeMultipleNamespace(maybe_namespaced_key));
                } else {
                    let definitely_namespaced_key = format!(
                        "{prefix}__{maybe_namespaced_key}",
                        prefix = cluster.namespace_prefix
                    );
                    if !identifier_to_index_map.contains_key(&definitely_namespaced_key) {
                        let idx = single_cluster_graph
                            .add_node((node.id.to_owned(), node.title.to_owned()));
                        identifier_to_index_map.insert(definitely_namespaced_key, idx);
                    } else {
                        structural_errors
                            .push(StructuralError::DoubleNode(definitely_namespaced_key));
                    }
                }
            }
            // build the single-cluster graph and check for structural errors at the same time
            for Edge { start_id, end_id } in &cluster.edges {
                let mut can_add = true;
                if start_id.starts_with(&format!("{}__", &cluster.namespace_prefix)) {
                    structural_errors.push(StructuralError::EdgeMultipleNamespace(
                        start_id.to_owned(),
                        end_id.to_owned(),
                        start_id.to_owned(),
                    ));
                    can_add = false;
                }
                if end_id.starts_with(&format!("{}__", &cluster.namespace_prefix)) {
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
                        if !identifier_to_index_map.contains_key(&start_id) {
                            let idx =
                                single_cluster_graph.add_node((start_id.clone(), start_id.clone()));
                            identifier_to_index_map.insert(start_id.clone(), idx);
                        }
                    } else {
                        start_id = format!("{}__{start_id}", cluster.namespace_prefix);
                    }
                    if end_id.contains("__") {
                        if !identifier_to_index_map.contains_key(&end_id) {
                            let idx =
                                single_cluster_graph.add_node((end_id.clone(), end_id.clone()));
                            identifier_to_index_map.insert(end_id.clone(), idx);
                        }
                    } else {
                        end_id = format!("{}__{end_id}", cluster.namespace_prefix);
                    }
                    match (
                        identifier_to_index_map.get(&start_id),
                        identifier_to_index_map.get(&end_id),
                    ) {
                        (Some(idx1), Some(idx2)) => {
                            // don't need edge labels, so just using ""
                            single_cluster_graph.add_edge(*idx1, *idx2, "");
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
                Ok(ClusterGraphPair(cluster, single_cluster_graph))
            } else {
                Err(anyhow::Error::from(StructuralErrorGrouping {
                    components: structural_errors,
                }))
            }
        })
    });
    eprintln!("Graphs have been computed from clusters.");
    let cluster_graph_comments_triples = cluster_graph_pairs.into_iter().map(|result| {
        result.map(|ClusterGraphPair(cluster, graph)| {
            let mut remarks: Vec<String> = vec![];
            if check_missing_files {
                remarks.push("Can't check for missing files yet.".to_owned());
            }
            ClusterGraphCommentsTriple(cluster, graph, remarks)
        })
    });
    let cluster_graph_comments_dot_quadruples = cluster_graph_comments_triples.map(|result| {
        result.map(|ClusterGraphCommentsTriple(cluster, graph, comments)| {
            let dot = format!(
                "{:?}",
                Dot::with_attr_getters(
                    &graph,
                    &[Config::EdgeNoLabel],
                    &|_g, _g_edge_ref| "".to_owned(),
                    &node_dot_attributes
                )
            );
            ClusterGraphCommentsDotQuadruple(cluster, graph, comments, dot)
        })
    });
    eprintln!("Dots have been generated and remarks have been added.");

    let cluster_graph_comments_svg_quadruples = cluster_graph_comments_dot_quadruples.map(|r| {
        r.map(
            |ClusterGraphCommentsDotQuadruple(cluster, graph, comments, dot_src)| {
                let g = graphviz_rust::parse(&dot_src)
                    .expect("Assuming petgraph generated valid dot syntax.");
                ClusterGraphCommentsSvgQuadruple(
                    cluster,
                    graph,
                    comments,
                    exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])
                        .expect("Assuming valid graph can be rendered into SVG."),
                )
            },
        )
    });

    let (mut cluster_graph_pairs, mut svg_comment_pair_results) = (vec![], vec![]);
    let mut error_occurred = false;
    cluster_graph_comments_svg_quadruples
        .into_iter()
        .for_each(|result| match result {
            Ok(ClusterGraphCommentsSvgQuadruple(cluster, graph, comments, svg)) => {
                if !error_occurred {
                    cluster_graph_pairs.push(ClusterGraphPair(cluster, graph));
                }
                svg_comment_pair_results.push(Ok(CommentsSvgPair(comments, svg)));
            }
            Err(e) => {
                error_occurred = true;
                svg_comment_pair_results.push(Err(e));
            }
        });

    let cluster_graph_pairs_result = if error_occurred {
        Err(anyhow::Error::from(StructuralError::InvalidComponentGraph))
    } else {
        Ok(cluster_graph_pairs)
    };

    // compute the big graph
    let mut paths: Vec<&str> = paths.collect();
    let mut boundary_errors = vec![];
    let complete_graph_result = cluster_graph_pairs_result.and_then(|cluster_graph_pairs| {
        let mut complete_graph: Graph<NodeData, EdgeData> = Graph::new();
        let mut complete_graph_map = HashMap::new();
        // first: insert all internal nodes in their namespaced form
        // also map each namespaced ID to a graph index
        cluster_graph_pairs
            .iter()
            .for_each(|ClusterGraphPair(cluster, graph)| {
                for (id, title) in graph.node_weights() {
                    // only add the internal ones to the map
                    if !id.contains("__") {
                        let node_idx = complete_graph.add_node((
                            format!("{}__{id}", cluster.namespace_prefix),
                            title.to_owned(),
                        ));
                        complete_graph_map
                            .insert(format!("{}__{id}", cluster.namespace_prefix), node_idx);
                    }
                }
            });
        // now insert the edges
        cluster_graph_pairs
            .iter()
            .for_each(|ClusterGraphPair(cluster, _)| {
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
                            boundary_errors.push(StructuralError::ClusterBoundary(
                                cluster.namespace_prefix.clone(),
                                namespaced_end_id,
                            ));
                        }
                        (None, Some(_)) => {
                            boundary_errors.push(StructuralError::ClusterBoundary(
                                cluster.namespace_prefix.clone(),
                                namespaced_start_id,
                            ));
                        }
                        (None, None) => {
                            boundary_errors.push(StructuralError::ClusterBoundary(
                                cluster.namespace_prefix.clone(),
                                namespaced_end_id,
                            ));
                            boundary_errors.push(StructuralError::ClusterBoundary(
                                cluster.namespace_prefix.clone(),
                                namespaced_start_id,
                            ));
                        }
                    }
                }
            });
        if boundary_errors.is_empty() {
            Ok(complete_graph)
        } else {
            Err(anyhow::Error::from(StructuralErrorGrouping {
                components: boundary_errors,
            }))
        }
    });
    eprintln!("Global graph has been represented.");
    let mut comments = vec![];
    let complete_graph_svg_and_comments = complete_graph_result.map(|graph| {
        let toposort_result = toposort(&graph, None)
            .expect("Currently just assuming cycle-free graph. Could take this into account.");
        let (res, _revmap): (_, Vec<NodeIndex>) =
            dag_to_toposorted_adjacency_list(&graph, &toposort_result);
        let (tr, _tc) = dag_transitive_reduction_closure(&res);
        for edge in res.edge_references() {
            let source = edge.source();
            let target = edge.target();
            if !tr.contains_edge(source, target) {
                comments.push(format!(
                    "Redundant edge! {} -> {}",
                    graph
                        .node_weight(source)
                        .expect("Edge exists, so node does too.")
                        .0,
                    graph
                        .node_weight(target)
                        .expect("Edge exists, so node does too.")
                        .0
                ));
            }
        }
        let dot = format!(
            "{:?}",
            Dot::with_attr_getters(
                &graph,
                &[Config::EdgeNoLabel],
                &|_g, _g_edge_ref| "".to_owned(),
                &node_dot_attributes
            )
        );
        CommentsSvgPair(
            comments,
            exec(
                graphviz_rust::parse(&dot).expect("Assuming petgraph generated valid dot syntax."),
                &mut PrinterContext::default(),
                vec![Format::Svg.into()],
            )
            .expect("Assuming valid graph can be rendered into SVG."),
        )
    });
    paths.push("complete graph (currently only shows hard dependencies)");
    svg_comment_pair_results.push(complete_graph_svg_and_comments);
    std::iter::zip(
        paths,
        svg_comment_pair_results.into_iter().map(|r| {
            r.map(|CommentsSvgPair(comments, svg)| (comments, svg))
                .map_err(|e| e.to_string())
        }),
    )
    .collect()
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
fn associate_parents_children(
    paths: &'_ str,
) -> Result<HashMap<&'_ Path, Vec<&'_ Path>>, &'_ Path> {
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

    struct MockFileReader {
        contents: String,
    }
    
    impl super::FileReader for MockFileReader {
        fn read_to_string(&self, _path: &str) -> std::io::Result<String> {
            Ok(self.contents.clone())
        }
    }

    // TODO: add tests for the paths -> commented SVG's function

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
        .invoke_handler(tauri::generate_handler![
            read_contents,
            associate_parents_children
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
