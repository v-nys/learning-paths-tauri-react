// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
use crate::readers::FileReader;
use anyhow;
use ignore;
use logic_based_learning_paths_bin::plugins::LBLPPlugin;
use petgraph::adj::List;
use petgraph::visit::IntoNeighbors;
use regex;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::io::Write;
use zip::write::FileOptions;
use zip::CompressionMethod;

use schemars::{
    schema::{
        InstanceType, RootSchema, Schema::Object, SchemaObject, SingleOrVec, StringValidation,
    },
    schema_for,
};

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

mod readers;
mod rendering;

use logic_based_learning_paths_bin::domain::{self, UnpopulatedCluster};
use logic_based_learning_paths_bin::domain::{
    EdgeData, EdgeType, ExtensionFieldProcessingResult, Graph, NodeID, NodeProcessingError,
    StructuralError, TypedEdge,
};
use logic_based_learning_paths_bin::{deserialization, domain::ArtifactMapping};

type SVGSource = String;
type Comment = String;

/// A way to bundle multiple structural errors, so they can be signalled simultaneously.
#[derive(Debug)]
struct StructuralErrorGrouping {
    components: Vec<StructuralError>,
}

#[derive(Debug, Serialize)]
struct UnlockingCondition {
    all_of: HashSet<NodeID>,
    one_of: HashSet<NodeID>,
}

#[derive(Serialize)]
struct ReadableUnlockingCondition {
    all_of: HashSet<String>,
    one_of: HashSet<String>,
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
#[derive(Debug, Clone)]
struct RootedSupercluster {
    graph: Graph,
    roots: Vec<NodeID>,
}

#[derive(Default)]
struct AppState {
    // this stuff is stored so it can be accessed if user clicks on "create zip" button
    supercluster_with_roots: Mutex<Option<(RootedSupercluster, Vec<domain::Cluster>)>>,
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

#[derive(Default)]
struct NoDataLoaded {}

#[derive(Debug)]
struct UnpopulatedClusterWithMetadata {
    cluster_path: PathBuf,
    unpopulated_cluster: UnpopulatedCluster,
    contents_file_contents: String,
}

#[derive(Debug)]
struct UnpopulatedClusterResultWithMetadata {
    cluster_path: PathBuf,
    unpopulated_cluster_with_contents_file_contents: anyhow::Result<(UnpopulatedCluster, String)>,
}

#[derive(Debug)]
struct SchemaGenerationResult {
    cluster_path: PathBuf,
    unpopulated_cluster_with_contents_file_contents: anyhow::Result<(UnpopulatedCluster, String)>,
}

fn read_contents_with_test_dependencies<'a>(
    paths: &'a str,
    file_is_readable: fn(&Path) -> bool,
    directory_is_readable: fn(&Path) -> bool,
    // NOTE: app_state's current value does not matter
    // we just have the MutexGuard so we can write!
    mut _app_state: MutexGuard<Option<(RootedSupercluster, Vec<domain::Cluster>)>>,
) -> Vec<(&'a str, Result<(Vec<Comment>, SVGSource), String>)> {
    let mut reader = readers::RealFileReader {};
    let paths = paths.split(";").map(|p| PathBuf::from(p));
    let read_results = paths.clone().map(|p| {
        let yaml_location = p.join("contents.lc.yaml");
        ReadResultForPath(reader.read_to_string(yaml_location.as_path()), p)
    });
    /* read_results contains results of reading *unpopulated* clusters
     * this won't always be successful
     * it has to be successful for every cluster if cluster merging is to succeed
     */
    let read_results = read_results
        .map(|ReadResultForPath(r, p)| {
            (
                p,
                match r {
                    Ok(ref text) => serde_yaml::from_str::<
                        deserialization::UnpopulatedClusterForSerialization,
                    >(text)
                    .map(|ucfs| (ucfs, text.to_owned()))
                    .map_err(anyhow::Error::new),
                    Err(e) => Err(anyhow::Error::new(e)),
                },
            )
        })
        .map(|(p, res)| UnpopulatedClusterResultWithMetadata {
            cluster_path: p.clone(),
            unpopulated_cluster_with_contents_file_contents: res
                .and_then(|(ucfs, text)| ucfs.build(&p).map(|uc| (uc, text))),
        })
        .collect::<Vec<_>>();
    println!("So I am seeing this. And I know there are read results.");
    // how would schema gen work?
    // should perform a mapping over the results
    // no need to really transform the contents yet, but those for which schema cannot be generated
    // should become error values
    // the schema is for a *populated* cluster
    // but deserialization will happen for *unpopulated* cluster
    // see https://raw.githubusercontent.com/v-nys/learning-paths-tauri-react/4c143bb58e3a5066c40bb7fde71c5518c2289eb2/rust-workspace/logic_based_learning_paths_bin/src/main.rs
    let mut overall_schema = dbg!(schema_for!(deserialization::ClusterForSerialization));
    let mut plugin_schema = schemars::schema_for!(deserialization::PluginForSerialization);
    let schema_generation_results = read_results.into_iter().map(|ucrwm| {
        SchemaGenerationResult {
            cluster_path: ucrwm.cluster_path,
            unpopulated_cluster_with_contents_file_contents: ucrwm
                .unpopulated_cluster_with_contents_file_contents
                .and_then(|(uc, contents)| {
                    // TODO: create customized version of overall_schema
                    // and only map to Ok value if that goes smoothly
                    Ok((uc, contents))
                }),
        }
    });
    // for sgr in schema_generation_results {
    //     dbg!(sgr);
    // }
    std::process::exit(0);
    todo!("add this part")
}

fn file_is_readable(file_path: &Path) -> bool {
    file_path.is_file() && File::open(file_path).is_ok()
}

fn path_is_dir(directory_path: &Path) -> bool {
    directory_path.is_dir()
}

#[derive(Debug)]
struct SuperclusterComposition {
    composition: Vec<ClusterDAGRootsTriple>,
    supercluster: RootedSupercluster,
}

#[derive(Debug)]
struct SuperclusterErrorBreakdown {
    supercluster_error: anyhow::Error,
    component_results: Vec<anyhow::Result<ClusterDAGRootsTriple>>,
}

/// Reads input files, returning individual clusters and supercluster.
fn read_all_clusters_with_test_dependencies<'a, T: readers::FileReader>(
    paths: &'a str,
    reader: &mut T,
) -> Result<SuperclusterComposition, SuperclusterErrorBreakdown> {
    todo!("Switch to new approach.")
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

fn plugin_to_paths_to_schemas_entry(
    plugin_path: &String,
    params_and_schemas: HashMap<String, (bool, serde_json::Value)>,
    mut schema_for_plugin: RootSchema,
) -> (&String, RootSchema) {
    let mut required_properties_for_plugin = schema_for_plugin.schema.object().required.clone();
    let mut properties_for_plugin = schema_for_plugin.schema.object().properties.clone();
    params_and_schemas
        .iter()
        .for_each(|(param, (required, param_schema))| {
            if *required {
                required_properties_for_plugin.insert(param.into());
            }
            let mut param_schema: RootSchema = serde_json::from_value(param_schema.clone())
                .expect("Assuming (de)serializating by libraries works.");
            param_schema.meta_schema = None;
            properties_for_plugin.insert(param.into(), Object(param_schema.schema));
        });
    schema_for_plugin.schema.object().required = required_properties_for_plugin;
    schema_for_plugin.schema.object().properties = properties_for_plugin;
    (plugin_path, schema_for_plugin)
}

fn process_and_comment_cluster(
    cluster: &mut domain::Cluster,
    graph: &Graph,
    cluster_path: &PathBuf,
    file_is_readable: fn(&Path) -> bool,
    directory_is_readable: fn(&Path) -> bool,
    artifacts: &mut HashSet<ArtifactMapping>,
) -> Vec<String> {
    let mut remarks: Vec<String> = vec![];
    let cluster_path = Path::new(cluster_path);
    artifacts.insert(ArtifactMapping {
        local_file: cluster_path.join("contents.lc.yaml"),
        root_relative_target_dir: PathBuf::from(cluster.namespace_prefix.clone()),
    });
    let mut overall_schema = schema_for!(deserialization::ClusterForSerialization);
    let mut plugin_schema = schemars::schema_for!(deserialization::PluginForSerialization);
    plugin_schema.meta_schema = None;
    let mut node_schema = schemars::schema_for!(deserialization::Node);
    node_schema.meta_schema = None;
    let mut mandatory_fields = HashSet::new();
    cluster.node_plugins.iter_mut().for_each(|node_plugin| {
        let extension_field_schema = node_plugin.get_extension_field_schema();
        if let Ok(extension_field_schema) = extension_field_schema {
            extension_field_schema
                .iter()
                .for_each(|(field, (required, field_schema))| {
                    if *required {
                        node_schema.schema.object().required.insert(field.into());
                        mandatory_fields.insert(field.to_string());
                    }
                    let mut field_schema: RootSchema = serde_json::from_value(field_schema.clone())
                        .expect("Assuming (de)serializating by libraries works.");
                    field_schema.meta_schema = None;
                    node_schema
                        .schema
                        .object()
                        .properties
                        .insert(field.into(), Object(field_schema.schema));
                    field_schema
                        .definitions
                        .iter()
                        .for_each(|(ref_string, schema)| {
                            overall_schema
                                .definitions
                                .insert(ref_string.into(), schema.clone());
                        });
                });
        } else {
            remarks.push(format!(
                "Node plugin at {} cannot provide get_extension_field_schema.",
                node_plugin.get_path()
            ));
        }
    });
    let mut plugin_paths_to_schemas: HashMap<&String, RootSchema> = cluster
        .node_plugins
        .iter_mut()
        .filter_map(|plugin| {
            let params_schema = plugin.get_params_schema();
            match params_schema {
                Ok(params_schema) => {
                    if params_schema.is_empty() {
                        None
                    } else {
                        Some(plugin_to_paths_to_schemas_entry(
                            plugin.get_path(),
                            params_schema,
                            plugin_schema.clone(),
                        ))
                    }
                }
                Err(e) => {
                    remarks.push(format!(
                        "Cannot obtain parameter schema from plugin at {}: {}",
                        plugin.get_path(),
                        e
                    ));
                    None
                }
            }
        })
        .collect();
    cluster.cluster_plugins.iter_mut().for_each(|plugin| {
        let params_schema = plugin.get_params_schema();
        if let Ok(params_schema) = params_schema {
            if !params_schema.is_empty() {
                let (key, value) = plugin_to_paths_to_schemas_entry(
                    plugin.get_path(),
                    params_schema,
                    plugin_schema.clone(),
                );
                plugin_paths_to_schemas.insert(key, value);
            }
        } else {
            remarks.push(format!(
                "Cluster plugin at {} cannot provide get_params_schema.",
                plugin.get_path()
            ));
        }
    });
    plugin_paths_to_schemas.values().for_each(|root_schema| {
        root_schema
            .definitions
            .iter()
            .for_each(|(ref_string, schema)| {
                overall_schema
                    .definitions
                    .insert(ref_string.into(), schema.clone());
            });
    });
    let mut sorted_plugin_paths_to_schemas = plugin_paths_to_schemas.iter().collect::<Vec<_>>();
    sorted_plugin_paths_to_schemas.sort_by(|a, b| a.0.cmp(b.0));
    let conditional_schema = sorted_plugin_paths_to_schemas.iter().fold(
        plugin_schema.schema.clone(),
        |acc, (plugin_path, plugin_schema_object)| {
            let mut if_clause = SchemaObject::new_ref("dummy-ref".into());
            let mut if_clause_required = BTreeSet::new();
            if_clause.reference = None;
            if_clause.instance_type = Some(SingleOrVec::from(InstanceType::Object));
            if_clause_required.insert("path".into());
            if_clause.object().required = if_clause_required;
            let mut if_clause_properties = BTreeMap::new();
            let mut path_schema = SchemaObject::new_ref("dummy-ref".into());
            path_schema.reference = None;
            let mut path_string_validation = StringValidation::default();
            let plugin_filename = Path::new(plugin_path)
                .file_name()
                .and_then(|file_name| file_name.to_str())
                .unwrap_or("problemwithpluginfilename");
            let escaped_path_string = format!("{}$", regex::escape(plugin_filename));
            path_string_validation.pattern = Some(escaped_path_string);
            path_schema.string = Some(Box::new(path_string_validation));
            if_clause_properties.insert("path".into(), Object(path_schema));
            if_clause.object().properties = if_clause_properties;
            let mut conditional = SchemaObject::new_ref("dummy-ref".into());
            conditional.reference = None;
            let conditional_subschemas = conditional.subschemas();
            conditional_subschemas.if_schema = Some(Box::new(Object(if_clause)));
            conditional_subschemas.then_schema =
                Some(Box::new(Object(plugin_schema_object.schema.clone())));
            conditional_subschemas.else_schema = Some(Box::new(Object(acc)));
            conditional
        },
    );
    overall_schema
        .definitions
        .insert("PluginForSerialization".into(), Object(conditional_schema));
    overall_schema
        .definitions
        .insert("Node".into(), Object(node_schema.schema));
    let stringified_schema = serde_json::to_string_pretty(&overall_schema);
    if stringified_schema.is_ok() {
        let write_result = std::fs::write(
            cluster_path.join("cluster_schema.json"),
            stringified_schema.unwrap().as_bytes(),
        );
        if write_result.is_err() {
            remarks.push("Failed to write schema.".into());
        }
    } else {
        remarks.push("Failed to stringify schema.".into());
    }
    cluster
        .cluster_plugins
        .iter_mut()
        .for_each(|cluster_processing_plugin| {
            let res = cluster_processing_plugin.process_cluster(cluster_path);
            if let Err(e) = res {
                remarks.push(format!("Cluster processing error: {e}"));
            }
        });
    dbg!("Still have a TODO here!");
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
                    .iter_mut()
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
                        p.result.is_ok()
                            || p.result.as_ref()
                                .is_err_and(|e| !e.indicates_inability_to_process_field())
                    });
                match first_processing_result {
                    Some(ExtensionFieldProcessingResult { result: Ok(extension_artifacts) }) => {
                        for artifact in extension_artifacts {
                            artifacts.insert(artifact);
                        }
                    },
                    Some(ExtensionFieldProcessingResult { result: Err(NodeProcessingError::Remarks(additional_remarks)) }) => {
                        remarks.extend(additional_remarks.into_iter());
                    },
                    Some(ExtensionFieldProcessingResult { result: Err(NodeProcessingError::CannotProcessFieldType) }) => { unreachable!("This indicates an inability to prcess the field, which is checked earlier."); },
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
    cluster_graph_pairs: &Vec<ClusterDAGRootsTriple>,
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

    use super::readers::{FileReader, MockFileReader, RealFileReader};
    use crate::{
        associate_parents_children, can_trigger_change, comment_graph, process_and_comment_cluster,
        read_all_clusters_with_test_dependencies, ClusterDAGRootsTriple, Pipeline,
        UnpopulatedClustersResult,
    };

    #[test]
    fn simple_unpopulated_clusters() {
        let mut reader = RealFileReader {};
        let base_path = std::fs::canonicalize(
            PathBuf::from("tests/pipeline-tests/loading-of-unpopulated-clusters/simple").as_path(),
        );
        let base_path = base_path.expect("If this panics, the test fails, which is fine.");
        let cluster_1_path = base_path
            .join("simpleproject")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let cluster_2_path = base_path
            .join("technicalinfo")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let combined_paths = vec![cluster_1_path, cluster_2_path].join(";");
        // not sure if there is all that much to test for this scenario
        // there are no plugins involved
        let pipeline = Pipeline::new().load_unpopulated_clusters(&combined_paths, &mut reader);
        match pipeline.state {
            UnpopulatedClustersResult::ZeroIssues(_) => {}
            UnpopulatedClustersResult::Issues(issues) => {
                dbg!(issues);
                panic!("Unpopulated clusters have issues when they shouldn't.")
            }
        }
    }

    #[test]
    fn unpopulated_clusters_with_noop_plugins() {
        let mut reader = RealFileReader {};
        let base_path = std::fs::canonicalize(
            PathBuf::from("tests/pipeline-tests/loading-of-unpopulated-clusters/with-noop-plugins")
                .as_path(),
        );
        let base_path = base_path.expect("If this panics, the test fails, which is fine.");
        let cluster_1_path = base_path
            .join("simpleproject")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let cluster_2_path = base_path
            .join("technicalinfo")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let combined_paths = vec![cluster_1_path, cluster_2_path].join(";");
        let pipeline = Pipeline::new().load_unpopulated_clusters(&combined_paths, &mut reader);
        match pipeline.state {
            UnpopulatedClustersResult::ZeroIssues(ucwms) => {
                assert!(ucwms.len() == 2);
                let simpleproject_cluster = &ucwms[0].unpopulated_cluster;
                let technicalinfo_cluster = &ucwms[1].unpopulated_cluster;
                assert!(simpleproject_cluster.pre_node_node_plugins.len() == 1);
                assert!(simpleproject_cluster.post_node_node_plugins.len() == 1);
                assert!(simpleproject_cluster.post_node_cluster_plugins.len() == 0);
                assert!(simpleproject_cluster.post_merge_node_plugins.len() == 1);
                assert!(simpleproject_cluster.post_merge_cluster_plugins.len() == 0);
                assert!(simpleproject_cluster
                    .pre_archive_plugins
                    .as_ref()
                    .is_some_and(|ps| ps.len() == 1));
                assert!(technicalinfo_cluster.pre_node_node_plugins.len() == 0);
                assert!(technicalinfo_cluster.post_node_cluster_plugins.len() == 1);
                assert!(technicalinfo_cluster.post_merge_cluster_plugins.len() == 1);
                assert!(technicalinfo_cluster.pre_archive_plugins.is_none());
            }
            UnpopulatedClustersResult::Issues(issues) => {
                dbg!(issues);
                panic!("Unpopulated clusters have issues when they shouldn't.")
            }
        }
    }

    #[test]
    fn unpopulated_clusters_with_missing_plugins() {
        let mut reader = RealFileReader {};
        let base_path = std::fs::canonicalize(
            PathBuf::from(
                "tests/pipeline-tests/loading-of-unpopulated-clusters/with-missing-plugins",
            )
            .as_path(),
        );
        let base_path = base_path.expect("If this panics, the test fails, which is fine.");
        let cluster_1_path = base_path
            .join("simpleproject")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let cluster_2_path = base_path
            .join("technicalinfo")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let combined_paths = vec![cluster_1_path, cluster_2_path].join(";");
        let pipeline = Pipeline::new().load_unpopulated_clusters(&combined_paths, &mut reader);
        match pipeline.state {
            UnpopulatedClustersResult::ZeroIssues(_) => {
                panic!("Missing plugins should cause an issue but are not doing so.")
            }
            UnpopulatedClustersResult::Issues(issues) => {
                assert!(issues.len() == 2);
                assert!(format!("{:#?}", issues[0]).contains("Unable to load Wasm file"));
                assert!(format!("{:#?}", issues[1]).contains("Unable to load Wasm file"));
            }
        }
    }

    #[test]
    fn ignored_file_cannot_trigger_change() {
        // use a different cluster from other tests that manipulate files
        // tests are run in parallel!
        let cluster_path =
            std::fs::canonicalize(PathBuf::from("tests/cluster1withlblpignore").as_path());
        assert!(cluster_path.is_ok());
        let cluster_path = cluster_path.unwrap();
        let lblpignore_path = cluster_path.join(".lblpignore");
        let contents_file_path = cluster_path.join("contents.lc.yaml");
        let write_result = std::fs::write(lblpignore_path, "contents.lc.yaml");
        assert!(write_result.is_ok_and(|_| !can_trigger_change(
            contents_file_path
                .to_str()
                .expect("Should be able to convert to string.")
        )));
    }

    #[test]
    fn regular_file_can_trigger_change() {
        // use a different cluster from other tests that manipulate files
        // tests are run in parallel!
        let cluster_path =
            std::fs::canonicalize(PathBuf::from("tests/cluster2withlblpignore").as_path());
        assert!(cluster_path.is_ok());
        let cluster_path = cluster_path.unwrap();
        let lblpignore_path = cluster_path.join(".lblpignore");
        let removal_result = std::fs::remove_file(lblpignore_path);
        let contents_file_path = cluster_path.join("contents.lc.yaml");
        assert!(
            removal_result.is_ok()
                || removal_result.is_err_and(|e| e.kind() == std::io::ErrorKind::NotFound)
        );
        let path_as_str = contents_file_path
            .to_str()
            .expect("Should be able to convert to string.");
        assert!(can_trigger_change(path_as_str));
    }

    #[ignore]
    #[test]
    fn read_trivial_cluster() {
        let mut reader =
            MockFileReader::new(vec![&Path::new("tests/technicalinfo/contents.lc.yaml")]);
        let supercluster_analysis =
            read_all_clusters_with_test_dependencies("technicalinfo", &mut reader);
        let mut artifacts = HashSet::new();
        assert!(supercluster_analysis.is_ok());
        let supercluster_analysis = supercluster_analysis.unwrap();
        assert_eq!(supercluster_analysis.composition.len(), 1);
        supercluster_analysis.composition.into_iter().for_each(
            |ClusterDAGRootsTriple(mut cluster, graph, _roots)| {
                let comments = process_and_comment_cluster(
                    &mut cluster,
                    &graph,
                    &PathBuf::from("tests/technicalinfo"),
                    |_| true,
                    |_| true,
                    &mut artifacts,
                );
                let expected_comments: Vec<String> = vec![];
                assert_eq!(comments, expected_comments);
                assert_eq!(reader.calls_made, 1);
                assert_eq!(cluster.edges.len(), 4);
            },
        );
    }

    #[ignore]
    #[test]
    fn check_structural_error_cycle() {
        let mut reader = MockFileReader::new(vec![&Path::new(
            "tests/technicalinfo_cycle/contents.lc.yaml",
        )]);

        let supercluster_analysis = read_all_clusters_with_test_dependencies("_", &mut reader);

        assert_eq!(reader.calls_made, 1);
        // could be more specific...
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

    #[ignore]
    #[test]
    fn detect_redundant_hard_dependency() {
        let mut reader = MockFileReader::new(vec![&Path::new(
            "tests/clusterwithredundantharddependency/contents.lc.yaml",
        )]);
        let supercluster_analysis = read_all_clusters_with_test_dependencies(
            "clusterwithredundantharddependency",
            &mut reader,
        );
        assert!(supercluster_analysis.is_ok());
        let supercluster_analysis = supercluster_analysis.unwrap();
        assert_eq!(supercluster_analysis.composition.len(), 1);
        supercluster_analysis.composition.into_iter().for_each(|ClusterDAGRootsTriple(_cluster, graph, _roots)| {
            let mut comments = vec![];
            comment_graph(&graph, &mut comments);
            assert_eq!(comments.len(), 1);
            assert_eq!(
                vec!["Redundant \"all\"-type edge clusterwithredundantharddependency__concept_A -> clusterwithredundantharddependency__concept_C".to_owned()],
                comments
            );
            assert_eq!(reader.calls_made, 1);
        });
    }

    #[ignore]
    #[test]
    fn detect_redundant_soft_dependency() {
        let mut reader = MockFileReader::new(vec![&Path::new(
            "tests/clusterwithredundantsoftdependency/contents.lc.yaml",
        )]);
        let supercluster_analysis = read_all_clusters_with_test_dependencies(
            "clusterwithredundantsoftdependency",
            &mut reader,
        );
        assert!(supercluster_analysis.is_ok());
        let supercluster_analysis = supercluster_analysis.unwrap();
        assert_eq!(supercluster_analysis.composition.len(), 1);
        supercluster_analysis.composition.into_iter().for_each(|ClusterDAGRootsTriple(_cluster, graph, _roots)| {
            let mut comments = vec![];
            comment_graph(&graph, &mut comments);
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
fn build_zip(_paths: &'_ str, state: tauri::State<'_, AppState>) -> Result<PathBuf, String> {
    let zip_path = std::path::Path::new("archive.zip");
    let zip_file = std::fs::File::create(zip_path).map_err(|e| e.to_string())?;
    // copy clusters into zipped folder
    let mut zip = zip::ZipWriter::new(zip_file);
    let mut mutex_guard = state
        .supercluster_with_roots
        .lock()
        .expect("Should always be able to gain access eventually.");
    let (supercluster, _component_clusters) = mutex_guard
        .as_mut()
        .expect("Should only be possible to invoke this command when there is a supercluster.");

    let options = FileOptions::default()
        .compression_method(CompressionMethod::Stored)
        .unix_permissions(0o755);
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

        let _ = zip.start_file("serialized_complete_graph.yaml", options); // TODO: use result
        let _ = zip.write(serialized.as_bytes()); // same
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
        |(_supercluster_node_index, (supercluster_node_id, _))| {
            if roots.contains(supercluster_node_id) {
                unlocking_conditions.insert(supercluster_node_id.clone(), None);
            } else {
                // dependent_to... uses a subgraph, so indexes are different!
                // matching_node = "all-type" graph counterpart to the current supercluster node
                let matching_nodes = dependency_to_dependent_graph
                    .node_references()
                    .filter(|(_idx, weight)| &weight.0 == supercluster_node_id)
                    .collect::<Vec<_>>();
                let matching_node = matching_nodes
                    .get(0)
                    .expect("Subgraph should contain all the supercluster nodes.");
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
                                    .map(|(id, _)| id.to_owned())
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
                        all_of: hard_dependency_ids,
                        one_of: soft_dependency_ids,
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
                    all_of: condition
                        .all_of
                        .iter()
                        .map(|node_id| format!("{}", node_id))
                        .collect(),
                    one_of: condition
                        .one_of
                        .iter()
                        .map(|node_id| format!("{}", node_id))
                        .collect(),
                }),
            )
        })
        .collect();
    zip.start_file("unlocking_conditions.json", options)
        .map_err(|ze| ze.to_string())?;
    zip.write(
        serde_json::to_string_pretty(&representation)
            .unwrap()
            .as_bytes(),
    )
    .map_err(|ze| ze.to_string())?;
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
                    .filter(|(_idx, weight)| weight.0 == namespaced_id)
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

#[tauri::command]
fn read_collections() -> Result<HashMap<String, String>, String> {
    let data_dir =
        tauri::api::path::data_dir().ok_or_else(|| "Unable to request data directory.")?;
    let collections_file = data_dir.join("lblpcollections.json");
    let collections_read_result = tauri::api::file::read_string(&collections_file);
    let collections_data = match collections_read_result {
        Ok(data) => data,
        Err(tauri::api::Error::Io(e)) if e.kind() == std::io::ErrorKind::NotFound => "{}".into(),
        Err(e) => Err(e.to_string())?,
    };
    let current_collections: HashMap<String, String> =
        serde_json::from_str(&collections_data).map_err(|e| e.to_string())?;
    Ok(current_collections)
}

#[tauri::command]
fn remove_collection(name: &str) -> Result<HashMap<String, String>, String> {
    let data_dir =
        tauri::api::path::data_dir().ok_or_else(|| "Unable to request data directory.")?;
    let collections_file = data_dir.join("lblpcollections.json");
    let mut current_collections: HashMap<String, String> = read_collections()?;
    current_collections.remove(name);
    let serialized = serde_json::to_string(&current_collections).map_err(|e| e.to_string())?;
    std::fs::write(collections_file, serialized).map_err(|e| e.to_string())?;
    Ok(current_collections)
}

#[tauri::command]
fn store_collection(collection: &str, paths: &str) -> Result<HashMap<String, String>, String> {
    let data_dir =
        tauri::api::path::data_dir().ok_or_else(|| "Unable to request data directory.")?;
    let collections_file = data_dir.join("lblpcollections.json");
    let mut current_collections: HashMap<String, String> = read_collections()?;
    current_collections.insert(collection.into(), paths.into());
    let serialized = serde_json::to_string(&current_collections).map_err(|e| e.to_string())?;
    std::fs::write(collections_file, serialized).map_err(|e| e.to_string())?;
    Ok(current_collections)
}

#[tauri::command]
fn can_trigger_change(path: &str) -> bool {
    let triggering_path_buf = PathBuf::from(path);
    let triggering_path = triggering_path_buf.as_path();
    assert!(
        triggering_path.is_absolute(),
        "This command is only intended for use with absolute paths."
    );
    let mut cluster_root = PathBuf::from(path)
        .parent()
        .map(|p| p.to_path_buf())
        .expect("Only an file in a folder should be able to trigger a potential change.");
    while !cluster_root.join("contents.lc.yaml").exists() {
        cluster_root = cluster_root
            .parent()
            .map(|p| p.to_path_buf())
            .expect("Only an file in a folder should be able to trigger a potential change.");
    }
    // keep invoking parent() until we get a contents.lc.yaml
    let walk = ignore::WalkBuilder::new(cluster_root)
        .add_custom_ignore_filename(".lblpignore")
        .ignore(false)
        .git_ignore(false)
        .hidden(true)
        .build();
    walk.into_iter()
        .any(|res| res.is_ok_and(|dir_entry| dir_entry.path() == triggering_path))
}

fn main() {
    tauri::Builder::default()
        .manage(AppState::default())
        .plugin(tauri_plugin_fs_watch::init())
        .invoke_handler(tauri::generate_handler![
            can_trigger_change,
            read_contents,
            associate_parents_children,
            check_learning_path_stateful,
            build_zip,
            read_collections,
            store_collection,
            remove_collection
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
