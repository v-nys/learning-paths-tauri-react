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
use std::{collections::HashMap, fmt, fs::File, ops::Index, path::Path};

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
    files: Option<Vec<String>>,
}

#[derive(Deserialize, Clone)]
struct Edge {
    start_id: String,
    end_id: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum EdgeType {
    All,
    AtLeastOne,
}

#[derive(Clone)]
struct TypedEdge {
    start_id: String,
    end_id: String,
    kind: EdgeType,
}

/// An namespaced collection of `Node`s which may link to `Node`s in different namespaces.
///
/// A `Cluster` can represent a thematic clustering (nodes are related to the same topic such as a common technology).
/// It can also represent a practical clustering (nodes make up a single project).
/// A `Cluster` has a (non-nested) namespace prefix, which can be used to refer to nodes in the `Cluster`.
/// E.g. if a `Cluster's` namespace prefix is `"foo"` and the `Cluster` contains a `Node` whose ID is `bar`, this node can be referred to as `foo__bar`.
/// The namespace and node ID are always separated by `"__"`.
#[derive(Clone)]
struct Cluster {
    namespace_prefix: String,
    nodes: Vec<Node>,
    edges: Vec<TypedEdge>,
    roots: Option<Vec<String>>,
}

#[derive(Deserialize, Clone)]
struct ClusterForSerialization {
    namespace_prefix: String,
    nodes: Vec<Node>,
    all_type_edges: Option<Vec<Edge>>,
    one_type_edges: Option<Vec<Edge>>,
    roots: Option<Vec<String>>,
}

impl ClusterForSerialization {
    fn build(self) -> Cluster {
        Cluster {
            namespace_prefix: self.namespace_prefix,
            nodes: self.nodes,
            edges: self
                .all_type_edges
                .unwrap_or_default()
                .into_iter()
                .map(|e| TypedEdge {
                    start_id: e.start_id,
                    end_id: e.end_id,
                    kind: EdgeType::All,
                })
                .chain(
                    self.one_type_edges
                        .unwrap_or_default()
                        .into_iter()
                        .map(|e| TypedEdge {
                            start_id: e.start_id,
                            end_id: e.end_id,
                            kind: EdgeType::AtLeastOne,
                        }),
                )
                .collect::<Vec<_>>(),
            roots: self.roots,
        }
    }
}

/// An error related to the internal structure of a (syntactically valid, semantically invalid) `Cluster`.
#[derive(Debug)]
enum StructuralError {
    DoubleNode(String),                              // creating two nodes with same ID
    MissingInternalEndpoint(String, String, String), // referring to non-existent node
    NodeMultipleNamespace(String),                   // creating a node with explicit namespace
    EdgeMultipleNamespace(String, String, String),   // edge from / to internal node with
    ClusterBoundary(String, String),                 // cluster, reference
    InvalidComponentGraph,
    Cycle(String),
    DependentRootNode(String),
    UndeclaredRoot(String),
    IncomingAnyEdge(String, String),
    OutgoingAllEdge(String, String),
}

impl fmt::Display for StructuralError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DoubleNode(id) => write!(f, "Node defined multiple times: {id}"),
            Self::MissingInternalEndpoint(start_id, end_id, missing_id) => write!(f, "Node {missing_id} mentioned in edge {start_id} → {end_id} does not exist"),
            Self::NodeMultipleNamespace(id) => write!(f, "Node is explicitly namespaced (which is not allowed) in its definition: {id}"),
            Self::EdgeMultipleNamespace(start_id, end_id, namespaced_id) => write!(f, "Node {namespaced_id} mentioned in edge {start_id} → {end_id} is explicitly namespaced (which is not allowed if the namespace is that of the current cluster)."),
            Self::ClusterBoundary(cluster,reference) => write!(f, "Cluster {} refers to non-existent external node {}", cluster, reference),
            Self::InvalidComponentGraph => write!(f, "At least one component graph is invalid"),
            Self::Cycle(id) => write!(f, "Node {} is involved in a cycle", id),
            Self::DependentRootNode(id) => write!(f, "Node {} is declared as a root and has at least one incoming edge. Roots should not have incoming edges.", id),
            Self::UndeclaredRoot(id) => write!(f, "Root {} is not declared as a node in the cluster.", id),
            Self::IncomingAnyEdge(start_id,end_id) => write!(f, "\"At least one\" type edge from {} to {}. These edges can only connect to other clusters in the \"out\" direction.", start_id, end_id),
            Self::OutgoingAllEdge(start_id,end_id) => write!(f, "\"All\" type edge from {} to {}. These edges can only connect to other clusters in the \"in\" direction.", start_id, end_id),
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
type EdgeData = EdgeType;

struct ClusterGraphTuple(Cluster, Graph<NodeData, EdgeData>);
struct ClusterGraphCommentsTuple(Cluster, Graph<NodeData, EdgeData>, Vec<String>);
struct ClusterGraphCommentsDotTuple(Cluster, Graph<NodeData, EdgeData>, Vec<String>, String);
struct CommentsSvgTuple(Vec<String>, String);
struct ClusterGraphCommentsSvgTuple(Cluster, Graph<NodeData, EdgeData>, Vec<String>, String);

fn node_dot_attributes(_: &Graph<NodeData, EdgeData>, node_ref: (NodeIndex, &NodeData)) -> String {
    // label specified last is used, so this overrides the auto-generated one
    format!("label=\"{}\"", node_ref.1 .1)
}

/// Given a sequence of filesystem paths, deserialize the cluster represented by each path and optionally run additional validation.
///
/// # Parameters
/// - `paths`: A sequence of filesystem paths, represented as a single string.
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
fn read_contents(paths: &str) -> Vec<(&str, Result<(Vec<String>, String), String>)> {
    let mut reader = RealFileReader {};
    read_all_clusters_with_dependencies::<RealFileReader>(paths, &mut reader, file_is_readable)
}

fn file_is_readable(file_path: &Path) -> bool {
    file_path.is_file() && File::open(file_path).is_ok()
}

trait FileReader {
    fn read_to_string(&mut self, path: &str) -> std::io::Result<String>;
}

struct RealFileReader;

impl FileReader for RealFileReader {
    fn read_to_string(&mut self, path: &str) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
}

fn read_all_clusters_with_dependencies<'a, T: FileReader>(
    paths: &'a str,
    reader: &mut T,
    file_is_readable: fn(&Path) -> bool,
) -> (
    Vec<Result<ClusterGraphTuple, anyhow::Error>>,
    Result<Graph<NodeData, EdgeData>, anyhow::Error>,
) {
    let paths = paths.split(";");
    let read_results = paths.clone().map(|p| reader.read_to_string(p)).collect();
    voltronize_clusters(read_results)
}

// TODO: need something that will further process the voltronized tuple
// specifically, need to add comments (missing files, implied edges,...)
// also need something to render SVG's
// also need something that will check a learning path (which does not require comments or SVG's)

fn voltronize_clusters(
    read_results: Vec<std::io::Result<String>>,
) -> (
    Vec<Result<ClusterGraphTuple, anyhow::Error>>,
    Result<Graph<NodeData, EdgeData>, anyhow::Error>,
) {
    let clusters = read_results.into_iter().map(|r| match r {
        Ok(ref text) => serde_yaml::from_str::<ClusterForSerialization>(text)
            .map_err(anyhow::Error::new)
            .map(|cfs| cfs.build()),
        Err(e) => Err(anyhow::Error::new(e)),
    });
    // each cluster is associated with a petgraph Graph
    // so we get a vector of results
    let cluster_graph_tuples: Vec<_> = clusters
        .map(|result| {
            result.and_then(|cluster: Cluster| {
                // ??? why do I need to annotate this?
                let mut identifier_to_index_map = std::collections::HashMap::new();
                let mut single_cluster_graph = Graph::new();
                let mut structural_errors: Vec<StructuralError> = vec![];

                // check that nodes are not explicitly namespaced (because only internal ones are mentioned, external ones should only appear in edges)
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
                match &cluster.roots {
                    Some(roots) => roots.iter().for_each(|root| {
                        if !identifier_to_index_map.contains_key(root) {
                            structural_errors.push(StructuralError::UndeclaredRoot(root.clone()));
                        }
                    }),
                    _ => {}
                }
                // build the single-cluster graph and check for structural errors at the same time
                for TypedEdge {
                    start_id,
                    end_id,
                    kind,
                } in &cluster.edges
                {
                    let mut can_add = true;
                    // current cluster's namespace should not be mentioned
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
                    if can_add
                        && cluster
                            .roots
                            .as_ref()
                            .is_some_and(|roots| roots.contains(end_id))
                    {
                        structural_errors
                            .push(StructuralError::DependentRootNode(end_id.to_owned()));
                        can_add = false;
                    }
                    if start_id.contains("__") && *kind == EdgeType::AtLeastOne {
                        structural_errors.push(StructuralError::IncomingAnyEdge(
                            start_id.to_owned(),
                            end_id.to_owned(),
                        ));
                        can_add = false;
                    } else if end_id.contains("__") && *kind == EdgeType::All {
                        structural_errors.push(StructuralError::OutgoingAllEdge(
                            start_id.to_owned(),
                            end_id.to_owned(),
                        ));
                        can_add = false;
                    }
                    if can_add {
                        let mut start_id = start_id.to_owned();
                        let mut end_id = end_id.to_owned();
                        if start_id.contains("__") {
                            if !identifier_to_index_map.contains_key(&start_id) {
                                let idx = single_cluster_graph
                                    .add_node((start_id.clone(), start_id.clone()));
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
                                single_cluster_graph.add_edge(*idx1, *idx2, kind.clone());
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
                // TODO: cycle detection and implied edge detection is essentially the same for the complete graph
                // should move this into a function
                let toposort_result = toposort(&single_cluster_graph, None);
                match toposort_result.as_ref() {
                    Err(cycle) => {
                        structural_errors.push(StructuralError::Cycle(
                            single_cluster_graph.index(cycle.node_id()).0.clone(),
                        ));
                    }
                    _ => {}
                };
                if structural_errors.is_empty() {
                    Ok(ClusterGraphTuple(cluster, single_cluster_graph))
                } else {
                    Err(anyhow::Error::from(StructuralErrorGrouping {
                        components: structural_errors,
                    }))
                }
            })
        })
        .collect();

    //let x: Vec<_> = cluster_graph_tuples.collect();
    // will lead to StructuralError::InvalidComponentGraph
    let cluster_graph_pairs_result: Result<Vec<ClusterGraphTuple>, _> =
        cluster_graph_tuples.into_iter().collect();
    let complete_graph_result = cluster_graph_pairs_result
        .and_then(|cluster_graph_pairs| {
            let mut boundary_errors = vec![];

            let mut complete_graph: Graph<NodeData, EdgeData> = Graph::new();
            let mut complete_graph_map = HashMap::new();
            // first: insert all internal nodes in their namespaced form
            // also map each namespaced ID to a graph index
            cluster_graph_pairs
                .iter()
                .for_each(|ClusterGraphTuple(cluster, graph)| {
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
                .for_each(|ClusterGraphTuple(cluster, _)| {
                    for TypedEdge {
                        start_id,
                        end_id,
                        kind,
                    } in cluster.edges.iter()
                    {
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
                                complete_graph.add_edge(*start_idx, *end_idx, kind.clone());
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
        })
        .and_then(|graph| {
            toposort(&graph, None)
                .map_err(|cycle| {
                    anyhow::Error::from(StructuralError::Cycle(
                        graph.index(cycle.node_id()).0.clone(),
                    ))
                })
                .map(|_| graph)
        });
    (cluster_graph_tuples, complete_graph_result)
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

    use crate::{associate_parents_children, read_all_clusters_with_dependencies};

    struct MockFileReader<'a> {
        paths: Vec<&'a Path>,
        calls_made: usize,
    }

    impl<'a> super::FileReader for MockFileReader<'a> {
        fn read_to_string(&mut self, _path: &str) -> std::io::Result<String> {
            let path_option = self.paths.get(self.calls_made);
            self.calls_made += 1;
            match path_option {
                Some(p) => std::fs::read_to_string(p),
                None => panic!("Incorrect use of mock object"),
            }
        }
    }

    impl<'a> MockFileReader<'a> {
        fn new(paths: Vec<&'a Path>) -> Self {
            Self {
                paths,
                calls_made: 0,
            }
        }
    }

    #[test]
    fn read_trivial_cluster() {
        let mut reader = MockFileReader::new(vec![&Path::new("tests/git.yaml")]);
        let analysis = read_all_clusters_with_dependencies("_", true, &mut reader, |_| true);
        assert_eq!(analysis.len(), 2);
        assert_eq!(analysis[0].0, "_");
        assert!(
            analysis[0]
                .1
                .as_ref()
                .is_ok_and(|(comments, svg)| { comments.is_empty() && !svg.is_empty() }),
            "Comments was not empty or SVG was empty. Analysis: {:#?}",
            analysis[0].1
        );
        assert_eq!(
            analysis[1].0,
            "complete graph (currently only shows hard dependencies)"
        );
        assert!(
            analysis[1]
                .1
                .as_ref()
                .is_ok_and(|(comments, svg)| { comments.is_empty() && !svg.is_empty() }),
            "Comments was not empty or SVG was empty. Analysis: {:#?}",
            analysis[1].1
        );
        assert_eq!(reader.calls_made, 1);
    }

    // TODO: multi-cluster test
    // TODO: cycle test
    // TODO: mix of correctly read and incorrectly read results
    // TODO: test for various structural errors

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
