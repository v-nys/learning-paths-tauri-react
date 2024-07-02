// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
use anyhow;
use learning_paths_tauri_react::plugins::{
    load_pre_zip_plugins, ArtifactMapping, NodeProcessingError, NodeProcessingPlugin,
};
use petgraph::adj::List;
use petgraph::visit::IntoNeighbors;
use serde::Serialize;
use std::io::{Read, Write};
use std::{collections::HashSet, str::FromStr};
use walkdir::{DirEntry, WalkDir};
use yaml2json_rs::Yaml2Json;
use zip::write::FileOptions;
use zip::{CompressionMethod, ZipWriter};

use petgraph::{
    algo::{
        toposort,
        tred::{dag_to_toposorted_adjacency_list, dag_transitive_reduction_closure},
    },
    csr::DefaultIx,
    graph::NodeIndex,
    visit::{EdgeRef, IntoEdgeReferences, IntoNodeReferences},
};

use std::path::PathBuf;
use std::sync::{Mutex, MutexGuard};
use std::{collections::HashMap, fmt, fs::File, ops::Index, path::Path};

mod deserialization;
mod rendering;

use crate::rendering::svgify;
use learning_paths_tauri_react::domain;
use learning_paths_tauri_react::domain::{
    EdgeData, EdgeType, Graph, NodeID, StructuralError, TypedEdge,
};

type SVGSource = String;
type Comment = String;

/// A way to bundle multiple structural errors, so they can be signalled simultaneously.
#[derive(Debug)]
struct StructuralErrorGrouping {
    components: Vec<StructuralError>,
}

#[derive(Debug, Serialize)]
struct UnlockingCondition {
    allOf: HashSet<NodeID>,
    oneOf: HashSet<NodeID>,
}

#[derive(Serialize)]
struct ReadableUnlockingCondition {
    allOf: HashSet<String>,
    oneOf: HashSet<String>,
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
struct CommentsSvgTuple(Vec<String>, SVGSource);

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
    supercluster_with_roots:
        Mutex<Option<(RootedSupercluster, HashSet<ArtifactMapping>, Vec<String>)>>,
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
    mut app_state: MutexGuard<Option<(RootedSupercluster, HashSet<ArtifactMapping>, Vec<String>)>>,
) -> Vec<(&'a str, Result<(Vec<Comment>, SVGSource), String>)> {
    // in result, first str is "path" (but can also be "supercluster")
    let mut reader = RealFileReader {};
    // step 1: just get and combine data structures
    let (components, supercluster): (
        Vec<Result<ClusterDAGRootsTriple, _>>,
        Result<RootedSupercluster, _>,
    ) = read_all_clusters_with_test_dependencies::<RealFileReader>(paths, &mut reader);
    match supercluster.as_ref() {
        Ok(supercluster) => {
            let _ = app_state.insert((supercluster.clone(), HashSet::new(), vec![]));
        }
        _ => {}
    }
    // step 2: render to SVG
    let components_svgs: Vec<_> = components
        .iter()
        .map(|result| result.as_ref().ok().map(|component| svgify(&component.1)))
        .collect();
    let supercluster_svg = supercluster
        .as_ref()
        .ok()
        .map(|RootedSupercluster { graph, roots: _ }| svgify(graph));

    // step 3: link each path to a bunch of comments
    // this involves running extensions, so collect their outputs in one go
    let paths = paths.split(";");
    let paths_and_component_graphs = paths.clone().map(|p| PathBuf::from(p)).zip(&components);
    let mut artifacts = HashSet::new();
    let components_comments: Vec<_> = paths_and_component_graphs
        .map(|(path, result)| {
            result
                .as_ref()
                .ok()
                .map(|ClusterDAGRootsTriple(cluster, graph, _roots)| {
                    process_and_comment_cluster(
                        cluster,
                        graph,
                        &path,
                        file_is_readable,
                        directory_is_readable,
                        &mut artifacts,
                    )
                })
        })
        .collect();
    let mut supercluster_comments = vec![];
    match supercluster.as_ref() {
        Ok(supercluster) => {
            let state_contents = app_state.insert((supercluster.clone(), artifacts, vec![]));
            let pre_zip_plugin_vectors: Vec<_> = components
                .iter()
                .filter_map(|c| c.as_ref().ok().map(|triple| &triple.0.pre_zip_plugin_paths))
                .collect();
            if pre_zip_plugin_vectors.len() > 1usize {
                supercluster_comments.push(
                    "Multiple clusters define pre-zip plugins. This is not allowed.".to_owned(),
                );
            } else {
                state_contents.2 = pre_zip_plugin_vectors
                    .iter()
                    .flat_map(|c| c.iter())
                    .map(|p| p.clone())
                    .collect();
            }
        }
        _ => {}
    }
    if let Ok(ref supercluster) = supercluster {
        comment_graph(&supercluster.graph, &mut supercluster_comments);
    }
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
    // translate into simpler data types
    // nothing pertaining to the archive happens here
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

fn subgraph_with_edges(parent: &Graph, predicate: impl Fn(&EdgeData) -> bool) -> Graph {
    let mut subgraph = Graph::new();
    let node_map = parent
        .node_references()
        .map(|(index_in_parent, node_data)| (index_in_parent, subgraph.add_node(node_data.clone())))
        .collect::<HashMap<_, _>>();

    parent
        .edge_references()
        .filter(|edge| predicate(edge.weight()))
        .for_each(|edge| {
            let new_source = node_map[&edge.source()];
            let new_target = node_map[&edge.target()];
            subgraph.add_edge(new_source, new_target, edge.weight().clone());
        });

    subgraph
}

/// Flip both direction and type of "all"-edges.
fn flip_all_type_edges(graph: &Graph) -> Graph {
    let mut new_graph = Graph::new();
    let node_map: Vec<_> = graph
        .node_indices()
        .map(|n| new_graph.add_node(graph[n].clone()))
        .collect();
    for edge in graph.edge_references() {
        let source = edge.source();
        let target = edge.target();
        let weight = edge.weight();

        match weight {
            EdgeType::All => {
                new_graph.add_edge(
                    node_map[target.index()],
                    node_map[source.index()],
                    EdgeType::AtLeastOne,
                );
            }
            _ => {
                new_graph.add_edge(
                    node_map[source.index()],
                    node_map[target.index()],
                    weight.clone(),
                );
            }
        }
    }
    new_graph
}

/// Add remarks to a (previously cycle-checked) graph.
///
/// Remarks do not indicate structural problems (i.e. the graph makes sense), but should be fixed regardless.
fn comment_graph(graph: &Graph, remarks: &mut Vec<String>) {
    /* assume the following notation:
     * ⇒ means the relationship "is a necessary dependency of" (directly expressed by "all"-type)
     * → means "is an interchangeable dependency of" (directly expressed by "any"-type)
     * then we have these rules
     * 1. A ⇒ B, B ⇒ C implies A ⇒ C (Petgraph can just perform transitive closure)
     * 2. A → C, B ⇒ C implies A → B
     * 3. A → C, ~∃B: B ‡ A ∧ B → C implies A ⇒ C (may be good to insert these edges, though...)
     * 4. A → B, A ⇒ B, C → B implies C → B is useless (because we need to do A anyway)
     *
     * to make all implied edges explicit:
     * can I translate to representation with single type of edge?
     * e.g. 1 only requires ALL-edges anyway
     * for 2: what if I flip type and direction of B ⇒ C (so I get C ← B)?
     * could I then apply TC/TR to infer implied edges?
     * 3 is a little different
     * it adds an implied edge that would be seen by the previous 2 steps
     * so a fixpoint computation might be a good idea here
     * 4 should be checked after the fixpoint computation
     */

    // rough implementation of rule 1 ("rough" because rule 3 is not currently implemented)
    let is_all_type = |edge: &EdgeData| edge == &EdgeType::All;
    let all_type_subgraph = subgraph_with_edges(graph, is_all_type);
    let order = toposort(&all_type_subgraph, None)
        .expect("If parent graph was cycle-checked, subgraph should be cycle-free.");
    let redundant_edges = filter_redundant_edges(&all_type_subgraph, order, EdgeType::All);
    redundant_edges.iter().for_each(|te| {
        remarks.push(format!(
            "Redundant \"all\"-type edge {} -> {}",
            te.start_id, te.end_id
        ));
    });

    // rough implementation of rule 2
    let flipped_graph = flip_all_type_edges(&graph);
    /*let is_at_least_one_type = |edge: &EdgeData| edge == &EdgeType::AtLeastOne;
    let flipped_graph = subgraph_with_edges(&flipped_graph, is_at_least_one_type);*/
    let toposort_order = toposort(&flipped_graph, None);
    match toposort_order {
        Ok(order) => {
            let redundant_edges =
                filter_redundant_edges(&flipped_graph, order, EdgeType::AtLeastOne);

            redundant_edges.iter().for_each(|te| {
                // te is a TypedEdge
                // should only indicate that it is redundant if it occurred in the original graph
                if graph.edge_indices().any(|edge_index| {
                    let edge_weight = graph
                        .edge_weight(edge_index)
                        .expect("Index is guaranteed to exist inside this loop.");
                    let (start_idx, end_idx) = graph
                        .edge_endpoints(edge_index)
                        .expect("Index is guaranteed to exist inside this loop.");
                    let ((start_id, _), (end_id, _)) = (
                        graph
                            .node_weight(start_idx)
                            .expect("Node is definitely present."),
                        graph
                            .node_weight(end_idx)
                            .expect("Node is definitely present."),
                    );
                    return &te.kind == edge_weight
                        && &te.start_id == start_id
                        && &te.end_id == end_id;
                }) {
                    remarks.push(format!(
                        "Redundant \"at least one\"-type edge {} -> {}",
                        te.start_id, te.end_id
                    ))
                };
            });
        }
        Err(_cycle) => {
            remarks.push("Checking for redundant \"at least one\" edges introduces a cycle and cannot be performed here. Probably indicates a structural issue, but this situation still needs further examination.".to_owned());
        }
    }
}

fn filter_redundant_edges<'a>(
    graph: &'a Graph,
    order: Vec<NodeIndex>,
    implied_kind: EdgeType, // applying TR removes weights
) -> Vec<TypedEdge> {
    let (res, revmap) = dag_to_toposorted_adjacency_list(graph, &order);
    let (tr, _tc) = dag_transitive_reduction_closure(&res);
    let redundant_edges: Vec<_> = res
        .edge_references()
        .filter(|edge| !tr.contains_edge(edge.source(), edge.target()))
        .map(|edge| {
            let source: NodeIndex<DefaultIx> = edge.source();
            let source_rev = revmap[source.index()];
            let target = edge.target();
            let target_rev = revmap[target.index()];
            TypedEdge {
            start_id: graph
                .node_weight(source_rev)
                .expect(
                    "Edge was already established to be in the graph, so endpoint must be, too.",
                )
                .0
                .clone(),
            end_id: graph
                .node_weight(target_rev)
                .expect(
                    "Edge was already established to be in the graph, so endpoint must be, too.",
                )
                .0
                .clone(),
            kind: implied_kind.clone(),
        }
        })
        .collect();
    redundant_edges
}

fn process_and_comment_cluster(
    cluster: &domain::Cluster,
    graph: &Graph,
    cluster_path: &PathBuf,
    file_is_readable: fn(&Path) -> bool,
    directory_is_readable: fn(&Path) -> bool,
    artifacts: &mut HashSet<ArtifactMapping>,
) -> Vec<String> {
    let mut remarks: Vec<String> = vec![];
    let cluster_path = Path::new(cluster_path);
    cluster.pre_cluster_plugins.iter().for_each(|p| {
        p.process_cluster(cluster_path);
    });
    let contents_path = cluster_path.join("contents.lc.yaml");
    let cluster_contents =
        std::fs::read_to_string(&contents_path).expect("Has to be there. Deal with absence later.");
    let yaml2json = Yaml2Json::new(yaml2json_rs::Style::PRETTY);
    let json_contents = yaml2json.document_to_string(&cluster_contents);
    std::fs::write(
        &cluster_path.join("contents.lc.json"),
        json_contents.expect("Conversion should not be an issue"),
    );
    artifacts.insert(ArtifactMapping {
        local_file: cluster_path.join("contents.lc.yaml"),
        root_relative_target_dir: PathBuf::from(cluster.namespace_prefix.clone()),
    });
    artifacts.insert(ArtifactMapping {
        local_file: cluster_path.join("contents.lc.json"),
        root_relative_target_dir: PathBuf::from(cluster.namespace_prefix.clone()),
    });
    let mandatory_fields: HashSet<_> = cluster
        .node_plugins
        .iter()
        .flat_map(|p| p.get_mandatory_fields())
        .collect();
    cluster.nodes.iter().for_each(|n| {
        let node_dir_is_readable =
            directory_is_readable(&cluster_path.join(&n.node_id.local_id).as_path());
        if !node_dir_is_readable {
            remarks.push(format!(
                "{} should contain a child directory {}.",
                cluster_path.to_string_lossy(),
                n.node_id.local_id
            ));
        } else {
            let missing_fields = mandatory_fields.iter().filter(|mandatory_field| {
                !n.extension_fields.keys().any(|key| key.eq(*mandatory_field))
            });
            missing_fields.for_each(|field_name| {
                remarks.push(format!("node {} is missing required field {}", n.node_id , field_name))
            });
            n.extension_fields.iter().for_each(|(k, v)| {
                let first_processing_result = cluster
                    .node_plugins
                    .iter()
                    // note that Rust iterators are lazy
                    // so only the first runnable field processor has side-effects
                    .map(|p| {
                        p.process_extension_field(
                            &cluster_path,
                            n,
                            k,
                            v
                        )
                    })
                    .find(|p| {
                        p.is_ok()
                            || p.as_ref()
                                .is_err_and(|e| !e.indicates_inability_to_process_field())
                    });
                match first_processing_result {
                    Some(Ok(extension_artifacts)) => {
                        for artifact in extension_artifacts {
                            artifacts.insert(artifact);
                        }
                    },
                    Some(Err(NodeProcessingError::Remarks(additional_remarks))) => {
                        remarks.extend(additional_remarks.into_iter());
                    },
                    Some(Err(NodeProcessingError::CannotProcessFieldType)) => { unreachable!("This indicates an inability to prcess the field, which is checked earlier."); },
                    None => { remarks.push(format!("No plugin able to process field {}", k)) }
                }
            });

            let contents_file_path = &cluster_path
                .join(&n.node_id.local_id)
                .join("contents.html")
                .as_path()
                .to_owned();
            if !file_is_readable(contents_file_path) {
                remarks.push(format!(
                    "Directory for node {} should contain a contents.html file.",
                    n.node_id.local_id
                ));
            } else {
                artifacts.insert(ArtifactMapping {
                    local_file: contents_file_path.to_path_buf(),
                    root_relative_target_dir: PathBuf::from(format!("{}/{}", cluster.namespace_prefix, n.node_id.local_id))
                });
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

    // step 3: get a result for a whole vector
    let cluster_graph_pairs_result: Result<Vec<&ClusterDAGRootsTriple>, anyhow::Error> =
        cluster_graph_tuples
            .iter()
            .map(Result::as_ref)
            .collect::<Result<_, _>>()
            .map_err(|_| StructuralError::InvalidComponentGraph.into());

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
            let ids = [start_id, end_id];
            ids.iter().for_each(|id| {
                if id.namespace != cluster.namespace_prefix
                    && !identifier_to_index_map.contains_key(&id)
                {
                    let idx = single_cluster_graph.add_node(((*id).clone(), format!("{}", &id)));
                    identifier_to_index_map.insert((*id).clone(), idx);
                }
            });
            let idxs = ids.map(|id| identifier_to_index_map.get(id));
            ids.iter().zip(idxs).for_each(|(id, idx)| {
                if idx.is_none() {
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
        Err(StructuralErrorGrouping {
            components: structural_errors,
        }
        .into())
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
        Err(StructuralErrorGrouping {
            components: boundary_errors,
        }
        .into())
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
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
    };

    use crate::{
        associate_parents_children, comment_graph, process_and_comment_cluster,
        read_all_clusters_with_test_dependencies, ClusterDAGRootsTriple,
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
    #[ignore] // will require some reorganizing...
              // should only use "dummy plugins" here that are supplied with the code?
    fn read_trivial_cluster() {
        let mut reader =
            MockFileReader::new(vec![&Path::new("tests/technicalinfo/contents.lc.yaml")]);
        let (component_analysis, _supercluster_analysis) =
            read_all_clusters_with_test_dependencies("_", &mut reader);
        let mut artifacts = HashSet::new();
        assert_eq!(component_analysis.len(), 1);
        assert!(component_analysis.get(0).as_ref().is_some());
        component_analysis.into_iter().for_each(|result| {
            let ClusterDAGRootsTriple(cluster, graph, _roots) =
                result.expect("There should be a result here.");
            let comments = process_and_comment_cluster(
                &cluster,
                &graph,
                &PathBuf::from("_"),
                |_| true,
                |_| true,
                &mut artifacts,
            );
            let expected_comments: Vec<String> = vec![];
            assert_eq!(comments, expected_comments);
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

    #[test]
    fn detect_redundant_hard_dependency() {
        /* FIXME
         * Here, A → C being considered a redundant "at least one"-type makes sense.
         * The problem is that it does not exist in the original graph.
         * So only the ones that actually exist in the original graph should be considered.
         * Should rewrite so that comment is not inserted directly,
         * but problem edges are returned?
         */
        let mut reader = MockFileReader::new(vec![&Path::new(
            "tests/clusterwithredundantharddependency/contents.lc.yaml",
        )]);
        let (component_analysis, _supercluster_analysis) = read_all_clusters_with_test_dependencies(
            "clusterwithredundantharddependency",
            &mut reader,
        );
        assert_eq!(component_analysis.len(), 1);
        assert!(component_analysis.get(0).as_ref().is_some());
        component_analysis.into_iter().for_each(|result| {
            let ClusterDAGRootsTriple(_cluster, graph, _roots) =
                result.expect("There should be a result here.");
            let mut comments = vec![];
            comment_graph(&graph, &mut comments);
            comments = dbg!(comments);
            assert_eq!(comments.len(), 1);
            assert_eq!(
                vec!["Redundant \"all\"-type edge clusterwithredundantharddependency__concept_A -> clusterwithredundantharddependency__concept_C".to_owned()],
                comments
            );
            assert_eq!(reader.calls_made, 1);
        });
    }

    #[test]
    fn detect_redundant_soft_dependency() {
        /* FIXME
         * Here, we have A → B, A → C, B ⇒ C
         * So that should transform into A → B, A → C, C → B
         * Currently says A → C is redundant, but should say A → B is redundant...
         */
        let mut reader = MockFileReader::new(vec![&Path::new(
            "tests/clusterwithredundantsoftdependency/contents.lc.yaml",
        )]);
        let (component_analysis, _supercluster_analysis) = read_all_clusters_with_test_dependencies(
            "clusterwithredundantsoftdependency",
            &mut reader,
        );
        assert_eq!(component_analysis.len(), 1);
        assert!(component_analysis.get(0).as_ref().is_some());
        component_analysis.into_iter().for_each(|result| {
            let ClusterDAGRootsTriple(_cluster, graph, _roots) =
                result.expect("There should be a result here.");
            let mut comments = vec![];
            comment_graph(&graph, &mut comments);
            comments = dbg!(comments);
            assert_eq!(comments.len(), 1);
            assert_eq!(
                vec!["Redundant \"at least one\"-type edge clusterwithredundantsoftdependency__concept_A -> clusterwithredundantsoftdependency__concept_B".to_owned()],
                comments
            );
            assert_eq!(reader.calls_made, 1);
        });
    }
}

#[tauri::command]
fn build_zip(paths: &'_ str, state: tauri::State<'_, AppState>) -> Result<PathBuf, String> {
    let zip_path = std::path::Path::new("archive.zip");
    let zip_file = std::fs::File::create(zip_path).map_err(|e| e.to_string())?;
    // copy clusters into zipped folder
    let absolute_cluster_dirs: Vec<_> = paths.split(";").map(PathBuf::from).collect();
    let mut zip = zip::ZipWriter::new(zip_file);

    let mutex_guard = state
        .supercluster_with_roots
        .lock()
        .expect("Should always be able to gain access eventually.");
    let (supercluster, artifacts, pre_zip_plugin_paths) = mutex_guard
        .as_ref()
        .expect("Should only be possible to invoke this command when there is a supercluster.");
    let pre_zip_plugins =
        load_pre_zip_plugins(pre_zip_plugin_paths.iter().map(|p| p.clone()).collect());
    for pre_zip_plugin in pre_zip_plugins {
        pre_zip_plugin.process_project(
            absolute_cluster_dirs
                .iter()
                .map(|pb| pb.as_path())
                .collect(),
        );
    }
    let options = FileOptions::default()
        .compression_method(CompressionMethod::Stored)
        .unix_permissions(0o755);
    let mut buffer = Vec::new();
    for ArtifactMapping {
        local_file,
        root_relative_target_dir,
    } in artifacts
    {
        zip.start_file(
            root_relative_target_dir
                .join(
                    local_file
                        .file_name()
                        .expect("Artifacts should be files, not directories."),
                )
                .to_string_lossy(),
            options,
        )
        .map_err(|e| e.to_string())?;
        let mut file = File::open(local_file).map_err(|e| e.to_string())?;
        file.read_to_end(&mut buffer).map_err(|e| e.to_string())?;
        zip.write_all(&buffer).map_err(|e| e.to_string())?;
        buffer.clear();
    }

    // TODO: factor this out?
    {
        let (supercluster, roots) = (&supercluster.graph, &supercluster.roots);

        // graph without nodes would not be valid
        let mut serialized = "nodes:\n".to_string();
        supercluster.node_weights().for_each(|(id, title)| {
            serialized.push_str(&format!("  - id: {}\n", id));
            serialized.push_str(&format!("    title: {}\n", title));
        });
        // misschien gebruik maken van partition op edge_references?
        let all_type_edges: Vec<_> = supercluster
            .edge_references()
            .filter(|e| e.weight() == &EdgeType::All)
            .map(|e| {
                Option::zip(
                    supercluster.node_weight(e.source()),
                    supercluster.node_weight(e.target()),
                )
                .map(|(n1, n2)| (n1.0.clone(), n2.0.clone()))
            })
            .flatten()
            .collect();
        let any_type_edges: Vec<_> = supercluster
            .edge_references()
            .filter(|e| e.weight() == &EdgeType::AtLeastOne)
            .map(|e| {
                Option::zip(
                    supercluster.node_weight(e.source()),
                    supercluster.node_weight(e.target()),
                )
                .map(|(n1, n2)| (n1.0.clone(), n2.0.clone()))
            })
            .flatten()
            .collect();
        if all_type_edges.len() > 0 {
            serialized.push_str("all_type_edges:\n");
            all_type_edges.iter().for_each(|(id1, id2)| {
                serialized.push_str(&format!("  - start_id: {}\n", id1));
                serialized.push_str(&format!("    end_id: {}\n", id2));
            })
        }
        if any_type_edges.len() > 0 {
            serialized.push_str("any_type_edges:\n");
            any_type_edges.iter().for_each(|(id1, id2)| {
                serialized.push_str(&format!("  - start_id: {}\n", id1));
                serialized.push_str(&format!("    end_id: {}\n", id2));
            })
        }
        if roots.len() > 0 {
            serialized.push_str("roots:\n");
            roots.iter().for_each(|root| {
                serialized.push_str(&format!("  - {}\n", root));
            });
        }

        zip.start_file("serialized_complete_graph.yaml", options);
        zip.write(serialized.as_bytes());
    }

    let (
        (
            dependent_to_dependency_graph,
            dependent_to_dependency_tc,
            dependent_to_dependency_revmap,
            dependent_to_dependency_toposort_order,
        ),
        (
            dependency_to_dependent_graph,
            dependency_to_dependent_tc,
            dependency_to_dependent_revmap,
            dependency_to_dependent_toposort_order,
        ),
        motivations_graph,
    ) = dependency_helpers(supercluster);
    let mut unlocking_conditions: HashMap<NodeID, Option<UnlockingCondition>> = HashMap::new();
    let roots = &supercluster.roots;
    supercluster.graph.node_references().for_each(
        |(supercluster_node_index, (supercluster_node_id, _))| {
            if roots.contains(supercluster_node_id) {
                unlocking_conditions.insert(supercluster_node_id.clone(), None);
            } else {
                // dependent_to... uses a subgraph, so indexes are different!
                // matching_node = "all-type" graph counterpart to the current Voltron node
                let matching_nodes = dependency_to_dependent_graph
                    .node_references()
                    .filter(|(idx, weight)| &weight.0 == supercluster_node_id)
                    .collect::<Vec<_>>();
                let matching_node = matching_nodes
                    .get(0)
                    .expect("Subgraph should contain all the Voltron nodes.");
                let matching_node_idx = matching_node.0.index();
                // denk dat dit strenger is dan nodig
                // dependent_to_dependency_tc betekent dat we *alle* harde dependencies zullen oplijsten
                // kan dit beperken tot enkel directe dependencies
                // i.e. de neighbors in dependent_to_depency_graph (neighbors = bereikbaar in één gerichte hop)
                let hard_dependency_ids: HashSet<NodeID> = dependent_to_dependency_tc
                    .neighbors(dependent_to_dependency_revmap[matching_node_idx])
                    .map(|ix: NodeIndex| dependent_to_dependency_toposort_order[ix.index()])
                    .filter_map(|idx| {
                        dependent_to_dependency_graph
                            .node_weight(idx)
                            .map(|(id, _)| id.clone())
                    })
                    .collect();
                let mut dependent_ids: HashSet<NodeID> = dependency_to_dependent_tc
                    .neighbors(dependency_to_dependent_revmap[matching_node.0.index()])
                    .map(|ix: NodeIndex| dependency_to_dependent_toposort_order[ix.index()])
                    .filter_map(|idx| {
                        dependency_to_dependent_graph
                            .node_weight(idx)
                            .map(|(id, _)| id.clone())
                    })
                    .collect();
                dependent_ids.insert(matching_node.1 .0.clone());
                let soft_dependency_ids = motivations_graph
                    .node_references()
                    .filter_map(|potential_motivator| {
                        let neighbors: HashSet<NodeID> = motivations_graph
                            .neighbors(potential_motivator.0)
                            .filter_map(|motivator_index| {
                                motivations_graph
                                    .node_weight(motivator_index)
                                    .map(|(id, title)| id.to_owned())
                            })
                            .collect();
                        if neighbors.is_disjoint(&dependent_ids) {
                            None
                        } else {
                            Some(potential_motivator.1 .0.to_owned())
                        }
                    })
                    .collect();
                unlocking_conditions.insert(
                    supercluster_node_id.clone(),
                    Some(UnlockingCondition {
                        allOf: hard_dependency_ids,
                        oneOf: soft_dependency_ids,
                    }),
                );
            }
        },
    );
    let representation: HashMap<_, _> = unlocking_conditions
        .iter()
        .map(|(k, v)| {
            (
                format!("{}", k),
                v.as_ref().map(|condition| ReadableUnlockingCondition {
                    allOf: condition
                        .allOf
                        .iter()
                        .map(|node_id| format!("{}", node_id))
                        .collect(),
                    oneOf: condition
                        .oneOf
                        .iter()
                        .map(|node_id| format!("{}", node_id))
                        .collect(),
                }),
            )
        })
        .collect();
    zip.start_file("unlocking_conditions.json", options);
    zip.write(
        serde_json::to_string_pretty(&representation)
            .unwrap()
            .as_bytes(),
    );
    zip.finish()
        .map(|_| zip_path.to_path_buf())
        .map_err(|ze| ze.to_string())
}

#[tauri::command]
fn check_learning_path_stateful(
    nodes: Vec<String>,
    state: tauri::State<'_, AppState>,
) -> Vec<String> {
    let app_state = state
        .supercluster_with_roots
        .lock()
        .expect("Should always be able to get app state.");
    app_state.as_ref().map_or(
        vec!["No stored result. This should not be possible, because text box should only be shown if there is a complete graph.".to_string()],
        |existing| {
        check_learning_path(&existing.0, nodes.iter().map(|s| s.as_str()).collect())
    })
}

// factored out because it is needed both for checking learning path and for building zip
fn dependency_helpers(
    supercluster_with_roots: &RootedSupercluster,
) -> (
    (Graph, List<(), NodeIndex>, Vec<NodeIndex>, Vec<NodeIndex>),
    (Graph, List<(), NodeIndex>, Vec<NodeIndex>, Vec<NodeIndex>),
    Graph,
) {
    let supercluster = &supercluster_with_roots.graph;
    let is_any_type = |edge: &EdgeData| edge == &EdgeType::AtLeastOne;
    let is_all_type = |edge: &EdgeData| edge == &EdgeType::All;

    let motivations_graph = subgraph_with_edges(&supercluster, is_any_type);
    let dependency_to_dependent_graph = subgraph_with_edges(&supercluster, is_all_type);
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

    return (
        (
            dependent_to_dependency_graph,
            dependent_to_dependency_tc,
            dependent_to_dependency_revmap,
            dependent_to_dependency_toposort_order,
        ),
        (
            dependency_to_dependent_graph,
            dependency_to_dependent_tc,
            dependency_to_dependent_revmap,
            dependency_to_dependent_toposort_order,
        ),
        motivations_graph,
    );
}

fn check_learning_path(
    supercluster_with_roots: &RootedSupercluster,
    node_ids: Vec<&str>,
) -> Vec<String> {
    let mut remarks = vec![];
    let (
        (
            dependent_to_dependency_graph,
            dependent_to_dependency_tc,
            dependent_to_dependency_revmap,
            dependent_to_dependency_toposort_order,
        ),
        (
            dependency_to_dependent_graph,
            dependency_to_dependent_tc,
            dependency_to_dependent_revmap,
            dependency_to_dependent_toposort_order,
        ),
        motivations_graph,
    ) = dependency_helpers(supercluster_with_roots);
    let mut seen_nodes = HashSet::new();
    for (index, namespaced_id) in node_ids.iter().enumerate() {
        let namespaced_id = domain::NodeID::from_two_part_string(namespaced_id);
        let human_index = index + 1;
        if let Ok(namespaced_id) = namespaced_id {
            if !supercluster_with_roots.roots.contains(&namespaced_id) {
                match dependency_to_dependent_graph
                    .node_references()
                    .filter(|(idx, weight)| weight.0 == namespaced_id)
                    .collect::<Vec<_>>()
                    .get(0)
                {
                    Some(matching_node) => {
                        let matching_node_idx = matching_node.0.index();
                        let hard_dependency_ids: HashSet<NodeID> = dependent_to_dependency_tc
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

                        let mut dependent_ids: HashSet<NodeID> = dependency_to_dependent_tc
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
                seen_nodes.insert(namespaced_id);
            }
        } else {
            remarks.push(format!("{:#?}", namespaced_id))
        }
    }

    remarks
}

fn main() {
    tauri::Builder::default()
        .manage(AppState::default())
        .plugin(tauri_plugin_fs_watch::init())
        .invoke_handler(tauri::generate_handler![
            read_contents,
            associate_parents_children,
            check_learning_path_stateful,
            build_zip,
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
