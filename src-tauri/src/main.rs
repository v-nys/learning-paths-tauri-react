// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use anyhow;

use petgraph::{
    algo::{
        toposort,
        tred::{dag_to_toposorted_adjacency_list, dag_transitive_reduction_closure},
    },
    graph::NodeIndex,
    visit::{EdgeRef, IntoEdgeReferences},
};

use std::path::PathBuf;
use std::sync::{Mutex, MutexGuard};
use std::{collections::HashMap, fmt, fs::File, ops::Index, path::Path};

mod deserialization;
mod domain;
mod rendering;

use crate::domain::{EdgeType, Graph, NodeID, StructuralError, TypedEdge};
use crate::rendering::svgify;

/// A way to bundle multiple structural errors, so they can be signalled simultaneously.
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

#[derive(Debug)]
// A combination of a `Cluster`, its Petgraph representation and its roots.
struct ClusterDAGRootsTriple(domain::Cluster, Graph, Vec<NodeID>);

/// A combination of the comments that apply to a cluster and its SVG representation.
struct CommentsSvgTuple(Vec<String>, String);

/// The result of reading a Path, along with that Path.
struct ReadResultForPath(Result<String, std::io::Error>, PathBuf);

/// A supercluster (result of merging normal Clusters) and dependency-free nodes.
#[derive(Clone)]
struct RootedSupercluster {
    graph: Graph,
    roots: Vec<NodeID>,
}

#[derive(Default)]
struct AppState {
    supercluster_with_roots: Mutex<Option<RootedSupercluster>>,
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
        .supercluster_with_roots
        .lock()
        .expect("Should always be able to gain access eventually.");
    app_state.take();
    read_contents_with_test_dependencies(paths, file_is_readable, path_is_dir, app_state)
}

fn read_contents_with_test_dependencies<'a>(
    paths: &'a str,
    file_is_readable: fn(&Path) -> bool,
    directory_is_readable: fn(&Path) -> bool,
    mut app_state: MutexGuard<Option<RootedSupercluster>>,
) -> Vec<(&'a str, Result<(Vec<String>, String), String>)> {
    let mut reader = RealFileReader {};
    let (components, supercluster) =
        read_all_clusters_with_test_dependencies::<RealFileReader>(paths, &mut reader);
    match supercluster.as_ref() {
        Ok(supercluster) => {
            let _ = app_state.insert(supercluster.clone());
        }
        _ => {}
    }
    let components_svgs: Vec<_> = components
        .iter()
        .map(|result| result.as_ref().ok().map(|component| svgify(&component.1)))
        .collect();
    let supercluster_svg = supercluster
        .as_ref()
        .ok()
        .map(|RootedSupercluster { graph, roots: _ }| svgify(graph));

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
    let mut supercluster_comments = vec![];

    match supercluster {
        Ok(ref supercluster) => {
            comment_graph(&supercluster.graph, &mut supercluster_comments);
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
                component_result.map(|_| {
                    CommentsSvgTuple(comments.expect(expectation), svg.expect(expectation))
                }),
            )
        })
        .collect();
    let supercluster_tuple = (
        "Supercluster",
        supercluster
            .map(|_| CommentsSvgTuple(supercluster_comments, supercluster_svg.expect(expectation))),
    );
    path_result_tuples.push(supercluster_tuple);
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

/// Reads input files, returning individual clusters and supercluster.
fn read_all_clusters_with_test_dependencies<'a, T: FileReader>(
    paths: &'a str,
    reader: &mut T,
) -> (
    Vec<Result<ClusterDAGRootsTriple, anyhow::Error>>,
    Result<RootedSupercluster, anyhow::Error>,
) {
    let paths = paths.split(";").map(|p| PathBuf::from(p));
    let read_results = paths
        .clone()
        .map(|p| {
            let yaml_location = p.join("contents.lc.yaml");
            ReadResultForPath(reader.read_to_string(yaml_location.as_path()), p)
        })
        .collect();
    merge_clusters(read_results)
}

/// Add remarks to a (previously cycle-checked) graph.
///
/// Remarks do not indicate structural problems (i.e. the graph makes sense), but should be fixed regardless.
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

/// Deserialize clusters and, if possible, merge them into a supercluster.
fn merge_clusters(
    read_results: Vec<ReadResultForPath>,
) -> (
    Vec<Result<ClusterDAGRootsTriple, anyhow::Error>>,
    Result<RootedSupercluster, anyhow::Error>,
) {
    // step 1: get individual clusters
    let clusters = read_results
        .into_iter()
        .map(|ReadResultForPath(r, p)| match r {
            Ok(ref text) => serde_yaml::from_str::<deserialization::ClusterForSerialization>(text)
                .map_err(anyhow::Error::new)
                .and_then(|cfs| {
                    let cluster_name = p.file_name().map(|osstr| osstr.to_owned().into_string());
                    match cluster_name {
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
    // step 2: associate individual clusters with separate Petgraph graphs
    let cluster_graph_tuples: Vec<_> = clusters
        .map(|result| result.and_then(associate_with_dag))
        .collect();

    // step 3: turn a vector of entirely Ok results into a a single Ok result
    // couldn't I just
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
    /* can I simplify along these lines?
    let cluster_graph_pairs_result: Result<Vec<&ClusterDAGRootsTriple>, anyhow::Error> =
       cluster_graph_tuples
           .iter()
           .collect::<Result<Vec<_>,anyhow::Error>>()
           .map_err(|_| anyhow::Error::from(StructuralError::InvalidComponentGraph));*/

    // step 4: compute the supercluster
    let supercluster = cluster_graph_pairs_result
        .and_then(|triples| {
            merge_into_supercluster(&triples).map(|graph| {
                (
                    graph,
                    triples.iter().flat_map(|triple| triple.2.clone()).collect(),
                )
            })
        })
        .and_then(|(graph, all_roots)| {
            toposort(&graph, None)
                .map_err(|cycle| {
                    anyhow::Error::from(StructuralError::Cycle(
                        graph.index(cycle.node_id()).0.clone(),
                    ))
                })
                .map(|_| RootedSupercluster {
                    graph,
                    // TODO: all_roots can be obtained by mapping over the ClusterDAGRootsTriples to
                    // get the roots and then just joining all of them
                    roots: all_roots,
                })
        });

    (cluster_graph_tuples, supercluster)
}

fn associate_with_dag(cluster: domain::Cluster) -> Result<ClusterDAGRootsTriple, anyhow::Error> {
    let mut all_roots: Vec<NodeID> = vec![];
    // Petgraph uses its own indexing system
    // so map own node identifiers to Petgraph indexes
    let mut identifier_to_index_map = std::collections::HashMap::new();
    let mut single_cluster_graph = Graph::new();
    let mut structural_errors: Vec<StructuralError> = vec![];
    for node in &cluster.nodes {
        if !identifier_to_index_map.contains_key(&node.node_id) {
            let idx = single_cluster_graph.add_node((node.node_id.clone(), node.title.clone()));
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
        if start_id.namespace != cluster.namespace_prefix && *kind == EdgeType::AtLeastOne {
            structural_errors.push(StructuralError::IncomingAnyEdge(
                start_id.to_owned(),
                end_id.to_owned(),
            ));
            can_add = false;
        } else if end_id.namespace != cluster.namespace_prefix && *kind == EdgeType::All {
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
                let idx =
                    single_cluster_graph.add_node((start_id.clone(), format!("{}", &start_id)));
                identifier_to_index_map.insert(start_id.clone(), idx);
            }
            if end_id.namespace != cluster.namespace_prefix
                && !identifier_to_index_map.contains_key(&end_id)
            {
                let idx = single_cluster_graph.add_node((end_id.clone(), format!("{}", &end_id)));
                identifier_to_index_map.insert(end_id.clone(), idx);
            }
            let ids = [start_id, end_id];
            let idxs = ids.map(|id| identifier_to_index_map.get(id));
            ids.iter().zip(idxs).for_each(|(id, idx)| {
                if let None = idx {
                    structural_errors.push(StructuralError::MissingInternalEndpoint(
                        start_id.to_owned(),
                        end_id.to_owned(),
                        (*id).to_owned(),
                    ));
                }
            });
            if let [Some(start_idx), Some(end_idx)] = idxs {
                single_cluster_graph.add_edge(*start_idx, *end_idx, kind.clone());
            }
        }
    }
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
        Ok(ClusterDAGRootsTriple(
            cluster,
            single_cluster_graph,
            all_roots,
        ))
    } else {
        Err(anyhow::Error::from(StructuralErrorGrouping {
            components: structural_errors,
        }))
    }
}

fn merge_into_supercluster(
    cluster_graph_pairs: &Vec<&ClusterDAGRootsTriple>,
) -> Result<Graph, anyhow::Error> {
    let mut boundary_errors = vec![];
    let mut complete_graph: Graph = Graph::new();
    let mut complete_graph_map = HashMap::new();
    cluster_graph_pairs
        .iter()
        .for_each(|ClusterDAGRootsTriple(cluster, graph, _)| {
            for (id, title) in graph.node_weights() {
                // only add the internal ones to the map
                if cluster.namespace_prefix == id.namespace {
                    let node_idx = complete_graph.add_node((id.clone(), title.to_owned()));
                    complete_graph_map.insert(id.clone(), node_idx);
                }
            }
        });
    cluster_graph_pairs
        .iter()
        .for_each(|ClusterDAGRootsTriple(cluster, _, _)| {
            for TypedEdge {
                start_id,
                end_id,
                kind,
            } in cluster.edges.iter()
            {
                let ids = [start_id, end_id];
                let idxs = ids.map(|id| complete_graph_map.get(id));
                ids.iter().zip(idxs).for_each(|(id, idx)| {
                    if let None = idx {
                        boundary_errors.push(StructuralError::ClusterBoundary(
                            cluster.namespace_prefix.clone(),
                            (*id).clone(),
                        ));
                    }
                });
                if let [Some(start_idx), Some(end_idx)] = idxs {
                    complete_graph.add_edge(*start_idx, *end_idx, kind.clone());
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
        associate_parents_children, comment_cluster, read_all_clusters_with_test_dependencies,
        ClusterDAGRootsTriple,
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
        let (component_analysis, _supercluster_analysis) =
            read_all_clusters_with_test_dependencies("_", &mut reader);
        assert_eq!(component_analysis.len(), 1);
        assert!(component_analysis.get(0).as_ref().is_some());
        component_analysis.into_iter().for_each(|result| {
            let ClusterDAGRootsTriple(cluster, graph, _roots) =
                result.expect("There should be a result here.");
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
        let (components_analysis, supercluster_analysis) =
            read_all_clusters_with_test_dependencies("_", &mut reader);
        assert_eq!(reader.calls_made, 1);
        // TODO: could check for specific error type
        assert!(components_analysis
            .get(0)
            .is_some_and(|analysis| analysis.is_err()));
        assert!(supercluster_analysis.is_err());
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
