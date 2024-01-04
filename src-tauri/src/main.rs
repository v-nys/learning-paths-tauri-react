// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use anyhow;
use graphviz_rust::{cmd::Format, exec, printer::PrinterContext};
use petgraph::adj::List;
use petgraph::visit::IntoNeighbors;
use petgraph::{
    algo::{
        toposort,
        tred::{dag_to_toposorted_adjacency_list, dag_transitive_reduction_closure},
    },
    dot::Dot,
    graph::NodeIndex,
    visit::{EdgeRef, IntoEdgeReferences, IntoNodeReferences},
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use zip::{ZipWriter, CompressionMethod};
use zip::write::FileOptions;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::sync::{Mutex, MutexGuard};
use std::thread::sleep;
use std::time::Duration;
use std::{
    collections::{HashMap, HashSet},
    fmt,
    fs::File,
    ops::Index,
    path::Path,
};
use walkdir::{DirEntry, WalkDir};

/* Maybe more use of references would be more idiomatic here. */

/// A single unit of learning material.
///
/// A `Node` represents knowledge that can be processed as one whole.
/// It does not need to be entirely standalone, as it can have dependencies in the form of `Edge` values.
#[derive(Deserialize, Clone, Debug, JsonSchema)]
struct Node {
    /// An ID should be locally unique inside a `Cluster` and is used to refer to a node inside its `Cluster`.
    ///
    /// The ID also be used to refer to the node from outside its `Cluster`, if it is preceded by the `Cluster`'s namespace prefix.
    id: String,
    /// Human-readable title for this unit of knowledge.
    ///
    /// This is not required to be unique at any level.
    title: String,
    // NOTE: once these become assignments, also check file structure
    // similar to how contents.md is checked for nodes themselves
    assignments: Option<Vec<Assignment>>,
}

#[derive(Deserialize, Debug, Clone, JsonSchema)]
struct Assignment {
    id: String,
    title: Option<String>,
    attachments: Option<Vec<String>> // TODO: best ook "vlak" houden, want path separator betekent incompatibiliteit Windows - UNIX...
}

#[derive(Deserialize, Clone, JsonSchema)]
struct Edge {
    start_id: String,
    end_id: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
enum EdgeType {
    All,
    AtLeastOne,
}

#[derive(Clone, Debug)]
struct TypedEdge {
    start_id: String,
    end_id: String,
    kind: EdgeType,
}

#[derive(Debug, Serialize)]
struct UnlockingCondition {
    allOf: HashSet<String>,
    oneOf: HashSet<String>,
}

/// An namespaced collection of `Node`s which may link to `Node`s in different namespaces.
///
/// A `Cluster` can represent a thematic clustering (nodes are related to the same topic such as a common technology).
/// It can also represent a practical clustering (nodes make up a single project).
/// A `Cluster` has a (non-nested) namespace prefix, which can be used to refer to nodes in the `Cluster`.
/// E.g. if a `Cluster's` namespace prefix is `"foo"` and the `Cluster` contains a `Node` whose ID is `bar`, this node can be referred to as `foo__bar`.
/// The namespace and node ID are always separated by `"__"`.
#[derive(Clone, Debug)]
struct Cluster {
    namespace_prefix: String,
    nodes: Vec<Node>,
    edges: Vec<TypedEdge>,
    roots: Option<Vec<String>>,
}

#[derive(Deserialize, Clone, JsonSchema)]
#[serde(deny_unknown_fields)]
struct ClusterForSerialization {
    /// Units of information inside this `Cluster`.
    nodes: Vec<Node>,
    /// Strict dependencies. A non-root `Node` can only be accessed if all of its dependencies of this type have been marked complete, along with one interchangeable dependency of this `Node` or of a `Node` which is strictly dependent on this `Node`.
    all_type_edges: Option<Vec<Edge>>,
    /// Interchangeable dependencies. A non-root `Node` can only be accessed if one dependency of this type has been marked complete for this node or for a `Node` which is strictly dependent on this `Node`. Furthermore, all strict dependencies must still be marked complete.
    any_type_edges: Option<Vec<Edge>>,
    /// IDs of `Node`s with no dependencies whatsoever, i.e. the only `Node`s which can be accessed unconditionally.
    roots: Option<Vec<String>>,
}

impl ClusterForSerialization {
    fn build(self, folder_name: String) -> Cluster {
        Cluster {
            namespace_prefix: folder_name,
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
                    self.any_type_edges
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

// TODO: may want to add variant for cluster id / node id / assignment id containing whitespace characters (which is possible in yaml with quoted text)
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
    // TODO: invalid naming, bv. clusternaam of nodenaam of assignmentnaam met spaties,...
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

type NodeData = (String, String);
type EdgeData = EdgeType;
type Graph = petgraph::Graph<NodeData, EdgeData>;

#[derive(Debug)]
struct ClusterGraphTuple(Cluster, Graph);
struct CommentsSvgTuple(Vec<String>, String);
struct ReadResultForPath(Result<String, std::io::Error>, PathBuf);

#[derive(Default)]
struct AppState {
    voltron_with_roots: Mutex<Option<(Graph, Vec<String>)>>,
}

fn node_dot_attributes(_: &Graph, node_ref: (NodeIndex, &NodeData)) -> String {
    // label specified last is used, so this overrides the auto-generated one
    format!("label=\"{}\" tooltip=\"{}\"", node_ref.1 .1, node_ref.1 .0)
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
fn read_contents<'a>(
    paths: &'a str,
    state: tauri::State<'_, AppState>,
) -> Vec<(&'a str, Result<(Vec<String>, String), String>)> {
    let mut app_state = state
        .voltron_with_roots
        .lock()
        .expect("Should always be able to gain access eventually.");
    app_state.take();
    let mut reader = RealFileReader {};
    read_contents_with_dependencies(paths, reader, file_is_readable, path_is_dir, app_state)
}

fn read_contents_with_dependencies<'a, R: FileReader>(
    paths: &'a str,
    mut reader: R,
    file_is_readable: fn(&Path) -> bool,
    directory_is_readable: fn(&Path) -> bool,
    mut app_state: MutexGuard<Option<(Graph, Vec<String>)>>,
) -> Vec<(&'a str, Result<(Vec<String>, String), String>)> {
    let mut reader = RealFileReader {};
    let (components, voltron) =
        read_all_clusters_with_dependencies::<RealFileReader>(paths, &mut reader, file_is_readable);
    match voltron.as_ref() {
        Ok(voltron) => {
            let _ = app_state.insert(voltron.clone());
        }
        _ => {}
    }
    let components_svgs: Vec<_> = components
        .iter()
        .map(|result| result.as_ref().ok().map(|component| svgify(&component.1)))
        .collect();
    let voltron_svg = voltron
        .as_ref()
        .ok()
        .map(|(voltron, _roots)| svgify(voltron));
    // let paths_and_components = paths.split(";").zip(&components);

    let paths = paths.split(";");
    let paths_and_components = paths.clone().map(|p| PathBuf::from(p)).zip(&components);
    let components_comments: Vec<_> = paths_and_components
        .map(|(path, result)| {
            result.as_ref().ok().map(|component| {
                comment_cluster(&component.0, &component.1, &path, file_is_readable, directory_is_readable)
            })
        })
        .collect();
    let mut voltron_comments = vec![];

    match voltron {
        Ok(ref voltron) => {
            comment_graph(&voltron.0, &mut voltron_comments);
        }
        _ => {}
    };
    let expectation = "Should only be None if component_result is Err.";
    let mut path_result_tuples: Vec<_> = paths
        .zip(components)
        .zip(components_svgs)
        .zip(components_comments)
        .map(|(((path, component_result), svg), comments)| {
            (
                path,
                component_result.map(|component| {
                    CommentsSvgTuple(comments.expect(expectation), svg.expect(expectation))
                }),
            )
        })
        .collect();
    let voltron_tuple = (
        "Complete graph",
        voltron.map(|_| CommentsSvgTuple(voltron_comments, voltron_svg.expect(expectation))),
    );
    path_result_tuples.push(voltron_tuple);
    let outcome = path_result_tuples
        .into_iter()
        .map(|(path, result)| {
            (
                path,
                match result {
                    Ok(CommentsSvgTuple(comments, svg)) => Ok((comments, svg)),
                    Err(e) => Err(e.to_string()),
                },
            )
        })
        .collect();
    outcome
}

fn file_is_readable(file_path: &Path) -> bool {
    file_path.is_file() && File::open(file_path).is_ok()
}

fn path_is_dir(directory_path: &Path) -> bool {
    directory_path.is_dir()
}

trait FileReader {
    fn read_to_string(&mut self, path: &Path) -> std::io::Result<String>;
}

struct RealFileReader;

impl FileReader for RealFileReader {
    fn read_to_string(&mut self, path: &Path) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
}

fn read_all_clusters_with_dependencies<'a, T: FileReader>(
    paths: &'a str,
    reader: &mut T,
    file_is_readable: fn(&Path) -> bool,
) -> (
    Vec<Result<ClusterGraphTuple, anyhow::Error>>,
    Result<(Graph, Vec<String>), anyhow::Error>,
) {
    let paths = paths.split(";").map(|p| PathBuf::from(p));
    let read_results = paths
        .clone()
        .map(|p| {
            let yaml_location = p.join("contents.lc.yaml");
            ReadResultForPath(reader.read_to_string(yaml_location.as_path()), p)
        })
        .collect();
    voltronize_clusters(read_results)
}

fn svgify(graph: &Graph) -> String {
    let dot = format!(
        "{:?}",
        Dot::with_attr_getters(
            graph,
            &[],
            &|_g, g_edge_ref| match g_edge_ref.weight() {
                EdgeType::All => {
                    "style=\"solid\"".to_owned()
                }
                EdgeType::AtLeastOne => {
                    "style=\"dashed\"".to_owned()
                }
            },
            &node_dot_attributes
        )
    );
    let g = graphviz_rust::parse(&dot).expect("Assuming petgraph generated valid dot syntax.");
    exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])
        .expect("Assuming valid graph can be rendered into SVG.")
}

fn comment_graph(graph: &Graph, remarks: &mut Vec<String>) {
    let toposort_order = toposort(&graph, None).expect(
        "This function should only be called for graphs which have already been cycle-checked.",
    );
    let (res, _revmap): (_, Vec<NodeIndex>) =
        dag_to_toposorted_adjacency_list(&graph, &toposort_order);
    let (tr, _tc) = dag_transitive_reduction_closure(&res);
    for edge in res.edge_references() {
        let source = edge.source();
        let target = edge.target();
        if !tr.contains_edge(source, target) {
            remarks.push(format!(
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
}

fn comment_cluster(
    cluster: &Cluster,
    graph: &Graph,
    cluster_path: &PathBuf,
    file_is_readable: fn(&Path) -> bool,
    directory_is_readable: fn(&Path) -> bool,
) -> Vec<String> {
    let mut remarks: Vec<String> = vec![];
    let cluster_path = Path::new(cluster_path);
    cluster.nodes
        .iter()
        .for_each(|n| {
            if !directory_is_readable(&cluster_path.join(&n.id).as_path()) {
                remarks.push(format!("{} should contain a child directory {}.", cluster_path.to_string_lossy(), n.id));
            }
            else {
                if !file_is_readable(&cluster_path.join(&n.id).join("contents.md").as_path()) {
                    remarks.push(format!("Directory for node {} should contain a contents.md file.", n.id));
                }
                match n.assignments {
                    Some(ref file_paths) => {
                        file_paths.iter().for_each(|assignment| {
                            
                            let base_assignment_path = cluster_path.join(&n.id).join(&assignment.id);
                            let contents_path = base_assignment_path.join("contents.md");
                            
                            if !file_is_readable(&contents_path) {
                                remarks.push(format!("Assignment {} associated with node {} lacks a readable file contents.md file.", assignment.id , n.title));
                            }
                            let _ = assignment.attachments.as_ref().map(|attachments| {
                                attachments.iter().for_each(|attachment| {
                                    let attachment_path = base_assignment_path.join(attachment);
                                    if !file_is_readable(attachment_path.as_path()) {
                                        remarks.push(format!("Attachment cannot be found at {}", attachment_path.to_string_lossy()))
                                    }
                                })
                            });
                        })},
                    None => {}
                }
            }
        });
    comment_graph(&graph, &mut remarks);
    remarks
}

// TODO: need something that will further process the voltronized tuple
// specifically, need to add comments (missing files, implied edges,...)
// also need something that will check a learning path (which does not require comments or SVG's)

fn voltronize_clusters(
    read_results: Vec<ReadResultForPath>,
) -> (
    Vec<Result<ClusterGraphTuple, anyhow::Error>>,
    Result<(Graph, Vec<String>), anyhow::Error>,
) {
    let mut all_roots: Vec<String> = vec![];
    let clusters = read_results
        .into_iter()
        .map(|ReadResultForPath(r, p)| match r {
            Ok(ref text) => serde_yaml::from_str::<ClusterForSerialization>(text)
                .map_err(anyhow::Error::new)
                .and_then(|cfs| {
                    let cluster_name = p.file_name().map(|osstr| osstr.to_owned().into_string());
                    match cluster_name {
                        Some(Ok(cluster_name)) => Ok(cfs.build(cluster_name)),
                        _ => Err(anyhow::Error::msg(
                            "Could not derive cluster name from path.",
                        )),
                    }
                }),
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
                        // TODO: checken op aanwezigheid leesbare contents.md?
                        // ANDERZIJDS: telt dit als structural error, of is het een IO error?
                        // eerder dat laatste, toch?
                    }
                }
                match &cluster.roots {
                    Some(roots) => roots.iter().for_each(|root| {
                        let namespaced_root = format!("{}__{}", cluster.namespace_prefix, root);
                        if !identifier_to_index_map.contains_key(&namespaced_root) {
                            structural_errors
                                .push(StructuralError::UndeclaredRoot(namespaced_root.clone()));
                        }
                        all_roots.push(namespaced_root);
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
                // TODO: can check for redundant motivations
                // i.e. motivating a strict predecessor is unncessary when motivating a successor
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

    let cluster_graph_pairs_result =
        cluster_graph_tuples
            .iter()
            .try_fold(Vec::new(), |mut vec, elem| match elem {
                Err(_) => Err(anyhow::Error::from(StructuralError::InvalidComponentGraph)),
                Ok(cgt) => {
                    vec.push(cgt);
                    Ok(vec)
                }
            });

    let complete_graph_result = cluster_graph_pairs_result
        .and_then(|cluster_graph_pairs| {
            let mut boundary_errors = vec![];

            let mut complete_graph: Graph = Graph::new();
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
                .map(|_| (graph, all_roots))
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

fn add_dir_to_zip(
    iterator: &mut dyn Iterator<Item = DirEntry>,
    prefix: &str, // i.e. the directory
    zip: &mut ZipWriter<File>
) -> zip::result::ZipResult<()>
{
    let options = FileOptions::default()
        // i.e. no actual compression!
        .compression_method(CompressionMethod::Stored)
        .unix_permissions(0o755);

    let mut buffer = Vec::new();
    for entry in iterator {
        let path = entry.path();
        // TODO: fix unwraps
        let name = path.strip_prefix(Path::new(prefix).parent().unwrap()).unwrap();
        // Write file or directory explicitly
        // Some unzip tools unzip files with directory paths correctly, some do not!
        if path.is_file() {
            println!("adding file {path:?} as {name:?} ...");
            #[allow(deprecated)]
            zip.start_file_from_path(name, options)?;
            let mut f = File::open(path)?;

            f.read_to_end(&mut buffer)?;
            zip.write_all(&buffer)?;
            buffer.clear();
        } else if !name.as_os_str().is_empty() {
            // Only if not root! Avoids path spec / warning
            // and mapname conversion failed error on unzip
            println!("adding dir {path:?} as {name:?} ...");
            #[allow(deprecated)]
            zip.add_directory_from_path(name, options)?;
        }
    }
    Result::Ok(())
}

#[tauri::command]
fn build_zip(paths: &'_ str, state: tauri::State<'_, AppState>) { // TODO: should probably return a Result<Path,Error>
    // TODO
    // maak een geserialiseerde voorstelling van de nodige data voor Moodle
    // moet Voltron en de clusters weergeven
    // want wil die kunnen tonen op de kaart
    // moet ook de (toposorted) topics oplijsten, met hun attachments
    // moet bij elk topic ook de vereenvoudigde voorwaarden voor unlocking geven
    // en moet alle clusterdata opnemen in zip
    // 1. (x) maak een zip met daarin gewoon brondata van elke cluster (kan later nog stuff renderen,...)
    // 2. (x) serialiseer Voltron, misschien best naar zelfde formaat als de clusters en neem mee op in zip (zou vooral nuttig zijn voor visualisatie, vandaar enkel ID en titel)
    // 3. (-) voeg JSON/YAML met toposorted topics en metadata toe (controle at least one/... zal wel daar staan, of op plaats waar concreet pad gecheckt wordt)
    let zip_path = std::path::Path::new("archive.zip");
    let zip_file = std::fs::File::create(zip_path).expect("TODO: deal with result");
    // copy clusters into zipped folder
    let absolute_cluster_dirs = paths.split(";");
    let mut zip = zip::ZipWriter::new(zip_file);
    absolute_cluster_dirs.for_each(|absolute_dir| {
        let walkdir = WalkDir::new(absolute_dir);
        let iterator = walkdir.into_iter();
        add_dir_to_zip(&mut iterator.flatten(), absolute_dir, &mut zip);
    });
    // serialize Voltron
    let voltron_with_roots = state
        .voltron_with_roots
        .lock()
        .expect("Should always be able to gain access eventually.");
    let voltron_with_roots = voltron_with_roots
        .as_ref()
        .unwrap(); // this command can only be invoked if there is a combined graph
    let voltron = &voltron_with_roots.0;
    // graph without nodes would not be valid
    let mut serialized = "nodes:\n".to_string();
    voltron.node_weights().for_each(|(id,title)| {
        serialized.push_str(&format!("  - id: {}\n", id));
        serialized.push_str(&format!("    title: {}\n", title));
    });
    // TODO: misschien gebruik maken van partition op edge_references?
    let all_type_edges: Vec<_> = voltron.edge_references().filter(|e| e.weight() == &EdgeType::All).map(|e| {
        Option::zip(voltron.node_weight(e.source()), voltron.node_weight(e.target())).map(|(n1,n2)| { (n1.0.clone(), n2.0.clone()) })
    }).flatten().collect();
    let any_type_edges: Vec<_> = voltron.edge_references().filter(|e| e.weight() == &EdgeType::AtLeastOne).map(|e| {
        Option::zip(voltron.node_weight(e.source()), voltron.node_weight(e.target())).map(|(n1,n2)| { (n1.0.clone(), n2.0.clone()) })
    }).flatten().collect();
    if all_type_edges.len() > 0 {
        serialized.push_str("all_type_edges:\n");
        all_type_edges.iter().for_each(|(id1,id2)| {
            serialized.push_str(&format!("  - start_id: {}\n", id1));
            serialized.push_str(&format!("    end_id: {}\n", id2));
        })
    }
    if any_type_edges.len() > 0 {
        serialized.push_str("any_type_edges:\n");
        any_type_edges.iter().for_each(|(id1,id2)| {
            serialized.push_str(&format!("  - start_id: {}\n", id1));
            serialized.push_str(&format!("    end_id: {}\n", id2));
        })
    }
    let roots = &voltron_with_roots.1;
    if roots.len() > 0 {
        serialized.push_str("roots:\n");
        roots.iter().for_each(|root| {
            serialized.push_str(&format!("  - {}\n", root));
        });
    }
    let options = FileOptions::default()
        .compression_method(CompressionMethod::Stored)
        .unix_permissions(0o755);
    zip.start_file("serialized_complete_graph.yaml", options);
    zip.write(serialized.as_bytes());

    // serialize unlocking conditions per topic (other metadata is in clusters anyway)
    let ((dependent_to_dependency_graph,dependent_to_dependency_tc,dependent_to_dependency_revmap,dependent_to_dependency_toposort_order),
         (dependency_to_dependent_graph,dependency_to_dependent_tc,dependency_to_dependent_revmap,dependency_to_dependent_toposort_order),
         motivations_graph) = dependency_helpers(voltron_with_roots);
    let mut unlocking_conditions: HashMap<String,Option<UnlockingCondition>> = HashMap::new();
    voltron.node_references().for_each(|(voltron_node_index, (voltron_node_id,_))| {
        if roots.contains(voltron_node_id) {
            unlocking_conditions.insert(voltron_node_id.clone(), None);
        }
        else {
            // dependent_to... uses a subgraph, so indexes are different!
            // matching_node = "all-type" graph counterpart to the current Voltron node
            let matching_nodes = dependency_to_dependent_graph
                .node_references()
                .filter(|(idx, weight)| &weight.0 == voltron_node_id)
                .collect::<Vec<_>>();
            let matching_node = matching_nodes
                .get(0)
                .expect("Subgraph should contain all the Voltron nodes.");
            let matching_node_idx = matching_node.0.index();
            // denk dat dit strenger is dan nodig
            // dependent_to_dependency_tc betekent dat we *alle* harde dependencies zullen oplijsten
            // kan dit beperken tot enkel directe dependencies
            // i.e. de neighbors in dependent_to_depency_graph (neighbors = bereikbaar in één gerichte hop)
            let hard_dependency_ids: HashSet<String> = dependent_to_dependency_tc
                        .neighbors(dependent_to_dependency_revmap[matching_node_idx])
                        .map(|ix: NodeIndex| dependent_to_dependency_toposort_order[ix.index()])
                        .filter_map(|idx| {
                            dependent_to_dependency_graph
                                .node_weight(idx)
                                .map(|(id, _)| id.clone())
                        })
                        .collect();
            let mut dependent_ids: HashSet<String> = dependency_to_dependent_tc
                        .neighbors(dependency_to_dependent_revmap[matching_node.0.index()])
                        .map(|ix: NodeIndex| dependency_to_dependent_toposort_order[ix.index()])
                        .filter_map(|idx| {
                            dependency_to_dependent_graph
                                .node_weight(idx)
                                .map(|(id, _)| id.clone())
                        })
                        .collect();
                    dependent_ids.insert(matching_node.1 .0.clone());
            let soft_dependency_ids = motivations_graph.node_references().filter_map(|potential_motivator| {
                let neighbors: HashSet<_> = motivations_graph
                                            .neighbors(potential_motivator.0)
                                            .filter_map(|motivator_index| { motivations_graph.node_weight(motivator_index).map(|(id,title)| id.to_string()) })
                                            .collect();
                if neighbors.is_disjoint(&dependent_ids) {
                    None
                }
                else {
                    Some(potential_motivator.1.0.to_string())
                }
            }).collect();
            unlocking_conditions.insert(voltron_node_id.clone(), Some(UnlockingCondition { allOf: hard_dependency_ids, oneOf: soft_dependency_ids }));
        }
    });
    unlocking_conditions.iter().for_each(|(k,v)| {
        println!("Unlocking conditions for {}:", k);
        println!("{:#?}", v);
    });
    // TODO: write serialized stuff to files
    zip.finish();
}

fn subgraph_with_edges(parent: &Graph, predicate: impl Fn(&EdgeData) -> bool) -> Graph {
    let mut subgraph = Graph::new();
    let mut node_map = HashMap::new();
    for edge in parent.edge_references() {
        if predicate(edge.weight()) {
            let (source, target) = (edge.source(), edge.target());
            let new_source = *node_map
                .entry(source)
                .or_insert_with(|| subgraph.add_node(parent.node_weight(source).unwrap().clone()));
            let new_target = *node_map
                .entry(target)
                .or_insert_with(|| subgraph.add_node(parent.node_weight(target).unwrap().clone()));
            subgraph.add_edge(new_source, new_target, edge.weight().clone());
        }
    }
    subgraph
}

#[tauri::command]
fn check_learning_path_stateful(
    nodes: Vec<String>,
    state: tauri::State<'_, AppState>,
) -> Vec<String> {
    let app_state = state
        .voltron_with_roots
        .lock()
        .expect("Should always be able to get app state.");
    app_state.as_ref().map_or(
        vec!["No stored result. This should not be possible, because text box should only be shown if there is a complete graph.".to_string()],
        |existing| {
        check_learning_path(existing, nodes.iter().map(|s| s.as_str()).collect())
    })
}

// factored out because it is needed both for checking learning path and for building zip
fn dependency_helpers((voltron, roots): &(Graph, Vec<String>)) -> ((Graph,List<(), NodeIndex>,Vec<NodeIndex>,Vec<NodeIndex>),(Graph,List<(), NodeIndex>,Vec<NodeIndex>,Vec<NodeIndex>),Graph) {
    let is_any_type = |edge: &EdgeData| edge == &EdgeType::AtLeastOne;
    let is_all_type = |edge: &EdgeData| edge == &EdgeType::All;

    // FIXME: pretty sure I am doing some redundant work here
    let motivations_graph = subgraph_with_edges(voltron, is_any_type);
    let dependency_to_dependent_graph = subgraph_with_edges(voltron, is_all_type);
    let mut dependent_to_dependency_graph = dependency_to_dependent_graph.clone();
    dependent_to_dependency_graph.reverse();
    let dependent_to_dependency_toposort_order = toposort(&dependent_to_dependency_graph, None)
        .expect(
            "This function should only be called for graphs which have already been cycle-checked.",
        );
    let dependency_to_dependent_toposort_order = toposort(&dependency_to_dependent_graph, None)
        .expect(
            "This function should only be called for graphs which have already been cycle-checked.",
        );
    let (dependent_to_dependency_res, dependent_to_dependency_revmap) =
        dag_to_toposorted_adjacency_list(
            &dependent_to_dependency_graph,
            &dependent_to_dependency_toposort_order,
        );
    let (_, dependent_to_dependency_tc) =
        dag_transitive_reduction_closure(&dependent_to_dependency_res);

    let (dependency_to_dependent_res, dependency_to_dependent_revmap) =
        dag_to_toposorted_adjacency_list(
            &dependency_to_dependent_graph,
            &dependency_to_dependent_toposort_order,
        );
    let (_, dependency_to_dependent_tc) =
        dag_transitive_reduction_closure(&dependency_to_dependent_res);
    
    return ((dependent_to_dependency_graph,dependent_to_dependency_tc,dependent_to_dependency_revmap,dependent_to_dependency_toposort_order),
            (dependency_to_dependent_graph,dependency_to_dependent_tc,dependency_to_dependent_revmap,dependency_to_dependent_toposort_order),
            motivations_graph);
}

fn check_learning_path(
    voltron_with_roots: &(Graph, Vec<String>),
    node_ids: Vec<&str>,
) -> Vec<String> {
    // TODO: consider combining the &Graph with roots?
    // might clarify that they belong together
    // might even refactor the voltronize_clusters function to also return the roots, would be feasible
    let mut remarks = vec![];
    let ((dependent_to_dependency_graph,dependent_to_dependency_tc,dependent_to_dependency_revmap,dependent_to_dependency_toposort_order),
         (dependency_to_dependent_graph,dependency_to_dependent_tc,dependency_to_dependent_revmap,dependency_to_dependent_toposort_order),
         motivations_graph) = dependency_helpers(voltron_with_roots);
    let (_,roots) = voltron_with_roots;
    let mut seen_nodes = HashSet::new();
    for (index, namespaced_id) in node_ids.iter().enumerate() {
        let human_index = index + 1;
        if !roots.contains(&namespaced_id.to_string()) {
            match dependency_to_dependent_graph
                .node_references()
                .filter(|(idx, weight)| &weight.0 == namespaced_id)
                .collect::<Vec<_>>()
                .get(0)
            {
                Some(matching_node) => {
                    let matching_node_idx = matching_node.0.index();
                    let hard_dependency_ids: HashSet<String> = dependent_to_dependency_tc
                        .neighbors(dependent_to_dependency_revmap[matching_node_idx])
                        .map(|ix: NodeIndex| dependent_to_dependency_toposort_order[ix.index()])
                        .filter_map(|idx| {
                            dependent_to_dependency_graph
                                .node_weight(idx)
                                .map(|(id, _)| id.clone())
                        })
                        .collect();
                    for dependency in hard_dependency_ids.difference(&seen_nodes) {
                        remarks.push(format!(
                            "Node {human_index} ({namespaced_id}) has unmet dependency {dependency}."
                        ));
                    }

                    let mut dependent_ids: HashSet<String> = dependency_to_dependent_tc
                        .neighbors(dependency_to_dependent_revmap[matching_node.0.index()])
                        .map(|ix: NodeIndex| dependency_to_dependent_toposort_order[ix.index()])
                        .filter_map(|idx| {
                            dependency_to_dependent_graph
                                .node_weight(idx)
                                .map(|(id, _)| id.clone())
                        })
                        .collect();
                    dependent_ids.insert(matching_node.1 .0.clone());

                    let is_motivated = seen_nodes.iter().fold(false, |acc, seen_node| {
                        acc || {
                            let motivations_entry = motivations_graph
                                .node_references()
                                .find(|node_ref| &node_ref.1 .0 == seen_node);
                            motivations_entry.is_some_and(|motivations_entry| {
                                let motivated_by_seen_node: Vec<_> =
                                    motivations_graph.neighbors(motivations_entry.0).collect();
                                motivated_by_seen_node.into_iter().any(|idx| {
                                    let motivated = motivations_graph.node_weight(idx);
                                    motivated
                                        .is_some_and(|weight| dependent_ids.contains(&weight.0))
                                })
                            })
                        }
                    });

                    if !is_motivated {
                        remarks.push(format!(
                            "Node {human_index} ({namespaced_id}) is not motivated by any predecessor, nor are any of its dependents."
                        ));
                    }
                    seen_nodes.insert(matching_node.1 .0.clone());
                }
                None => {
                    remarks.push(format!(
                        "Node {human_index} ({namespaced_id}) does not occur in the graph."
                    ));
                }
            }
        } else {
            seen_nodes.insert(namespaced_id.to_string());
        }
    }

    remarks
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };

    use crate::{
        associate_parents_children, check_learning_path, comment_cluster,
        read_all_clusters_with_dependencies, ClusterGraphTuple,
    };

    struct MockFileReader<'a> {
        paths: Vec<&'a Path>,
        calls_made: usize,
    }

    impl<'a> super::FileReader for MockFileReader<'a> {
        fn read_to_string(&mut self, _path: &Path) -> std::io::Result<String> {
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
        let mut reader =
            MockFileReader::new(vec![&Path::new("tests/technicalinfo/contents.lc.yaml")]);
        let (component_analysis, _voltron_analysis) =
            read_all_clusters_with_dependencies("_", &mut reader, |_| true);
        assert_eq!(component_analysis.len(), 1);
        assert!(component_analysis.get(0).as_ref().is_some());
        component_analysis.into_iter().for_each(|result| {
            let ClusterGraphTuple(cluster, graph) = result.expect("There should be a result here.");
            let comments = comment_cluster(&cluster, &graph, &PathBuf::from("_"), |_| true, |_| true);
            assert!(comments.is_empty());
            assert_eq!(reader.calls_made, 1);
            assert_eq!(cluster.edges.len(), 4);
        });
    }

    #[test]
    fn check_learning_path_trivial_cluster() {
        let mut reader =
            MockFileReader::new(vec![&Path::new("tests/technicalinfo/contents.lc.yaml")]);
        let (_, voltron_analysis) =
            read_all_clusters_with_dependencies("technicalinfo", &mut reader, |_| true);
        let comments = check_learning_path(
            &voltron_analysis.unwrap(),
            vec!["technicalinfo__concept_A", "technicalinfo__concept_B"],
        );
        assert_eq!(comments,
            vec![
                "Node 1 (technicalinfo__concept_A) is not motivated by any predecessor, nor are any of its dependents.",
                "Node 2 (technicalinfo__concept_B) is not motivated by any predecessor, nor are any of its dependents."
            ]
        );
    }

    #[test]
    fn check_valid_learning_path_two_small_clusters() {
        let mut reader = MockFileReader::new(vec![
            &Path::new("tests/technicalinfo/contents.lc.yaml"),
            &Path::new("tests/simpleproject/contents.lc.yaml"),
        ]);
        // TODO: could avoid writing two paths here...
        let (components_analysis, voltron_analysis) =
            read_all_clusters_with_dependencies("technicalinfo;simpleproject", &mut reader, |_| {
                true
            });
        assert_eq!(reader.calls_made, 2);
        let comments = check_learning_path(
            &voltron_analysis.unwrap(),
            vec![
                "simpleproject__introduction",
                "technicalinfo__concept_A",
                "technicalinfo__concept_B",
                "technicalinfo__concept_E",
                "technicalinfo__concept_C",
                "simpleproject__implementation",
            ],
        );
        assert!(comments.is_empty());
    }

    #[test]
    fn check_invalid_learning_path_two_small_clusters_missing_root() {
        let mut reader = MockFileReader::new(vec![
            &Path::new("tests/technicalinfo/contents.lc.yaml"),
            &Path::new("tests/simpleproject/contents.lc.yaml"),
        ]);
        // TODO: could avoid writing two paths here...
        let (_, voltron_analysis) =
            read_all_clusters_with_dependencies("technicalinfo;simpleproject", &mut reader, |_| {
                true
            });
        assert_eq!(reader.calls_made, 2);
        let comments = check_learning_path(
            &voltron_analysis.unwrap(),
            vec![
                "technicalinfo__concept_A",
                "technicalinfo__concept_B",
                "technicalinfo__concept_C",
                "simpleproject__implementation",
            ],
        );
        assert_eq!(comments,
            vec!["Node 1 (technicalinfo__concept_A) is not motivated by any predecessor, nor are any of its dependents.",
        "Node 2 (technicalinfo__concept_B) is not motivated by any predecessor, nor are any of its dependents.",
        "Node 3 (technicalinfo__concept_C) has unmet dependency technicalinfo__concept_E.",
        "Node 3 (technicalinfo__concept_C) is not motivated by any predecessor, nor are any of its dependents.",
        "Node 4 (simpleproject__implementation) has unmet dependency technicalinfo__concept_E.",
        "Node 4 (simpleproject__implementation) is not motivated by any predecessor, nor are any of its dependents."]);
    }

    #[test]
    fn check_invalid_learning_path_two_small_clusters_missing_dependency() {
        let mut reader = MockFileReader::new(vec![
            &Path::new("tests/technicalinfo/contents.lc.yaml"),
            &Path::new("tests/simpleproject/contents.lc.yaml"),
        ]);
        // TODO: could avoid writing two paths here...
        let (_, voltron_analysis) =
            read_all_clusters_with_dependencies("technicalinfo;simpleproject", &mut reader, |_| {
                true
            });
        assert_eq!(reader.calls_made, 2);
        let comments = check_learning_path(
            &voltron_analysis.unwrap(),
            vec![
                "simpleproject__introduction",
                "technicalinfo__concept_B",
                "technicalinfo__concept_E",
                "technicalinfo__concept_C",
                "simpleproject__implementation",
            ],
        );
        assert_eq!(
            comments,
            vec![
        "Node 2 (technicalinfo__concept_B) has unmet dependency technicalinfo__concept_A.",
        "Node 4 (technicalinfo__concept_C) has unmet dependency technicalinfo__concept_A.",
        "Node 5 (simpleproject__implementation) has unmet dependency technicalinfo__concept_A.",]
        );
    }

    #[test]
    fn check_structural_error_cycle() {
        let mut reader = MockFileReader::new(vec![&Path::new(
            "tests/technicalinfo_cycle/contents.lc.yaml",
        )]);
        let (components_analysis, voltron_analysis) =
            read_all_clusters_with_dependencies("_", &mut reader, |_| true);
        assert_eq!(reader.calls_made, 1);
        // TODO: could check for specific error type
        assert!(components_analysis
            .get(0)
            .is_some_and(|analysis| analysis.is_err()));
        assert!(voltron_analysis.is_err());
    }

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
    //let schema = schemars::schema_for!(ClusterForSerialization);
    //println!("{}", serde_json::to_string_pretty(&schema).unwrap());
    tauri::Builder::default()
        .manage(AppState::default())
        .plugin(tauri_plugin_fs_watch::init())
        .invoke_handler(tauri::generate_handler![
            read_contents,
            associate_parents_children,
            check_learning_path_stateful,
            build_zip
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
