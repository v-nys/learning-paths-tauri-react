// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use anyhow;
use graphviz_rust::{cmd::Format, exec, printer::PrinterContext};
use petgraph::{
    algo::{
        toposort,
        tred::{dag_to_toposorted_adjacency_list, dag_transitive_reduction_closure},
    },
    dot::Dot,
    graph::NodeIndex,
    visit::{EdgeRef, IntoEdgeReferences},
};

use std::path::PathBuf;
use std::sync::{Mutex, MutexGuard};
use std::{collections::HashMap, fmt, fs::File, ops::Index, path::Path};

mod deserialization;
mod domain;

use crate::domain::{EdgeType, NodeID, TypedEdge, StructuralError};

/* Maybe more use of references would be more idiomatic here. */

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

type NodeData = (NodeID, String);
type EdgeData = EdgeType;
type Graph = petgraph::Graph<NodeData, EdgeData>;

#[derive(Debug)]
struct ClusterGraphTuple(domain::Cluster, Graph);
struct CommentsSvgTuple(Vec<String>, String);
struct ReadResultForPath(Result<String, std::io::Error>, PathBuf);

#[derive(Default)]
struct AppState {
    voltron_with_roots: Mutex<Option<(Graph, Vec<NodeID>)>>,
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
    let reader = RealFileReader {};
    read_contents_with_dependencies(paths, reader, file_is_readable, path_is_dir, app_state)
}

fn read_contents_with_dependencies<'a, R: FileReader>(
    paths: &'a str,
    mut reader: R,
    file_is_readable: fn(&Path) -> bool,
    directory_is_readable: fn(&Path) -> bool,
    mut app_state: MutexGuard<Option<(Graph, Vec<NodeID>)>>,
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
                comment_cluster(
                    &component.0,
                    &component.1,
                    &path,
                    file_is_readable,
                    directory_is_readable,
                )
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
    Result<(Graph, Vec<NodeID>), anyhow::Error>,
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
    cluster: &domain::Cluster,
    graph: &Graph,
    cluster_path: &PathBuf,
    file_is_readable: fn(&Path) -> bool,
    directory_is_readable: fn(&Path) -> bool,
) -> Vec<String> {
    let mut remarks: Vec<String> = vec![];
    let cluster_path = Path::new(cluster_path);
    cluster.nodes.iter().for_each(|n| {
        if !directory_is_readable(&cluster_path.join(&n.node_id.local_id).as_path()) {
            remarks.push(format!(
                "{} should contain a child directory {}.",
                cluster_path.to_string_lossy(),
                n.node_id.local_id
            ));
        } else {
            if !file_is_readable(
                &cluster_path
                    .join(&n.node_id.local_id)
                    .join("contents.md")
                    .as_path(),
            ) {
                remarks.push(format!(
                    "Directory for node {} should contain a contents.md file.",
                    n.node_id.local_id
                ));
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
    Result<(Graph, Vec<NodeID>), anyhow::Error>,
) {
    let mut all_roots: Vec<NodeID> = vec![];
    let clusters = read_results
        .into_iter()
        .map(|ReadResultForPath(r, p)| match r {
            Ok(ref text) => serde_yaml::from_str::<deserialization::ClusterForSerialization>(text)
                .map_err(anyhow::Error::new)
                .and_then(|cfs| {
                    let cluster_name = p.file_name().map(|osstr| osstr.to_owned().into_string());
                    match cluster_name {
                        // dit wrappen is niet goed genoeg
                        // build kan dus een Err(String) opleveren
                        Some(Ok(cluster_name)) => {
                            cfs.build(cluster_name).map_err(anyhow::Error::msg)
                        }
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
            result.and_then(|cluster: domain::Cluster| {
                let mut identifier_to_index_map = std::collections::HashMap::new();
                let mut single_cluster_graph = Graph::new();
                let mut structural_errors: Vec<StructuralError> = vec![];
                for node in &cluster.nodes {
                    if !identifier_to_index_map.contains_key(&node.node_id) {
                        let idx = single_cluster_graph
                            .add_node((node.node_id.clone(), node.title.clone()));
                        identifier_to_index_map.insert(node.node_id.clone(), idx);
                    } else {
                        structural_errors.push(StructuralError::DoubleNode(node.node_id.clone()));
                    }
                }
                cluster.roots.iter().for_each(|root| {
                    if !identifier_to_index_map.contains_key(&root) {
                        structural_errors.push(StructuralError::UndeclaredRoot(root.clone()));
                    }
                    all_roots.push(root.clone());
                });
                // build the single-cluster graph and check for structural errors at the same time
                for TypedEdge {
                    start_id,
                    end_id,
                    kind,
                } in &cluster.edges
                {
                    let mut can_add = true;
                    if cluster.roots.contains(end_id) {
                        structural_errors.push(StructuralError::DependentRootNode(
                            end_id.to_owned(),
                            start_id.to_owned(),
                        ));
                        can_add = false;
                    }
                    if start_id.namespace != cluster.namespace_prefix
                        && *kind == EdgeType::AtLeastOne
                    {
                        structural_errors.push(StructuralError::IncomingAnyEdge(
                            start_id.to_owned(),
                            end_id.to_owned(),
                        ));
                        can_add = false;
                    } else if end_id.namespace != cluster.namespace_prefix && *kind == EdgeType::All
                    {
                        structural_errors.push(StructuralError::OutgoingAllEdge(
                            start_id.to_owned(),
                            end_id.to_owned(),
                        ));
                        can_add = false;
                    }
                    if can_add {
                        if start_id.namespace != cluster.namespace_prefix
                            && !identifier_to_index_map.contains_key(&start_id)
                        {
                            let idx = single_cluster_graph
                                .add_node((start_id.clone(), format!("{}", &start_id)));
                            identifier_to_index_map.insert(start_id.clone(), idx);
                        }
                        if end_id.namespace != cluster.namespace_prefix
                            && !identifier_to_index_map.contains_key(&end_id)
                        {
                            let idx = single_cluster_graph
                                .add_node((end_id.clone(), format!("{}", &end_id)));
                            identifier_to_index_map.insert(end_id.clone(), idx);
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
            // first: insert all internal nodes
            // also map each ID to a graph index
            cluster_graph_pairs
                .iter()
                .for_each(|ClusterGraphTuple(cluster, graph)| {
                    for (id, title) in graph.node_weights() {
                        // only add the internal ones to the map
                        if cluster.namespace_prefix == id.namespace {
                            let node_idx = complete_graph.add_node((id.clone(), title.to_owned()));
                            complete_graph_map.insert(id.clone(), node_idx);
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
                        match (
                            complete_graph_map.get(&start_id),
                            complete_graph_map.get(&end_id),
                        ) {
                            (Some(start_idx), Some(end_idx)) => {
                                complete_graph.add_edge(*start_idx, *end_idx, kind.clone());
                            }
                            (Some(_), None) => {
                                boundary_errors.push(StructuralError::ClusterBoundary(
                                    cluster.namespace_prefix.clone(),
                                    end_id.clone(),
                                ));
                            }
                            (None, Some(_)) => {
                                boundary_errors.push(StructuralError::ClusterBoundary(
                                    cluster.namespace_prefix.clone(),
                                    start_id.clone(),
                                ));
                            }
                            (None, None) => {
                                boundary_errors.push(StructuralError::ClusterBoundary(
                                    cluster.namespace_prefix.clone(),
                                    end_id.clone(),
                                ));
                                boundary_errors.push(StructuralError::ClusterBoundary(
                                    cluster.namespace_prefix.clone(),
                                    start_id.clone(),
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

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };

    use crate::{
        associate_parents_children, comment_cluster, read_all_clusters_with_dependencies,
        ClusterGraphTuple,
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
            let comments =
                comment_cluster(&cluster, &graph, &PathBuf::from("_"), |_| true, |_| true);
            assert!(comments.is_empty());
            assert_eq!(reader.calls_made, 1);
            assert_eq!(cluster.edges.len(), 4);
        });
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
    tauri::Builder::default()
        .manage(AppState::default())
        .plugin(tauri_plugin_fs_watch::init())
        .invoke_handler(tauri::generate_handler![
            read_contents,
            associate_parents_children
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
