use crate::readers;
use anyhow;
use schemars::schema_for;

use std::path::PathBuf;
use std::collections::HashSet;

use logic_based_learning_paths_bin::deserialization;
use logic_based_learning_paths_bin::domain::{Cluster, UnpopulatedCluster};

/* TODO:
 * split into smaller modules (one per step)?
 */

#[derive(Debug)]
/// The result of reading a Path, along with that Path.
struct ReadResultForPath(Result<String, std::io::Error>, PathBuf);

#[derive(Default)]
pub(crate) struct NoDataLoaded {}

impl NoDataLoaded {
    pub(crate) fn new() -> Self {
        NoDataLoaded {}
    }
}

pub(crate) struct AllSchemasGenerated {}

pub(crate) struct SubsetOfSchemasGenerated {}

#[derive(Debug)]
pub(crate) struct UnpopulatedClusterWithMetadata {
    cluster_path: PathBuf,
    unpopulated_cluster: UnpopulatedCluster,
    contents_file_contents: String,
}

impl UnpopulatedClusterWithMetadata {
    pub(crate) fn new(
        cluster_path: PathBuf,
        unpopulated_cluster: UnpopulatedCluster,
        contents_file_contents: String,
    ) -> Self {
        Self {
            cluster_path,
            unpopulated_cluster,
            contents_file_contents,
        }
    }

    pub(crate) fn get_cluster_path(&self) -> &PathBuf {
        &self.cluster_path
    }
    pub(crate) fn get_unpopulated_cluster(&self) -> &UnpopulatedCluster {
        &self.unpopulated_cluster
    }
    pub(crate) fn get_contents_file_contents(&self) -> &String {
        &self.contents_file_contents
    }
}

#[derive(Debug)]
pub(crate) struct EmptyClusterWithMetadata {
    cluster_path: PathBuf,
    empty_cluster: Cluster,
    contents_file_contents: String,
}

impl EmptyClusterWithMetadata {
    pub(crate) fn new(
        cluster_path: PathBuf,
        empty_cluster: Cluster,
        contents_file_contents: String,
    ) -> Self {
        Self {
            cluster_path,
            empty_cluster,
            contents_file_contents,
        }
    }

    pub(crate) fn get_cluster_path(&self) -> &PathBuf {
        &self.cluster_path
    }
    pub(crate) fn get_empty_cluster(&self) -> &Cluster {
        &self.empty_cluster
    }
    pub(crate) fn get_contents_file_contents(&self) -> &String {
        &self.contents_file_contents
    }
}

#[derive(Debug)]
pub struct UnpopulatedClusterResultWithMetadata {
    pub cluster_path: PathBuf,
    pub unpopulated_cluster_with_contents_file_contents:
        anyhow::Result<(UnpopulatedCluster, String)>,
}

#[derive(Debug)]
pub(crate) struct LoadedAllUnpopulatedClusters {
    unpopulated_clusters_with_metadata: Vec<UnpopulatedClusterWithMetadata>,
}

#[derive(Debug)]
pub(crate) struct LoadedSubsetOfUnpopulatedClusters {
    unpopulated_cluster_results_with_metadata: Vec<UnpopulatedClusterResultWithMetadata>,
}

impl NoDataLoaded {
    pub(crate) fn load_unpopulated_clusters<'a, T: readers::FileReader>(
        self,
        paths: &'a str,
        reader: &mut T,
    ) -> Result<LoadedAllUnpopulatedClusters, LoadedSubsetOfUnpopulatedClusters> {
        let paths = paths.split(";").map(|p| PathBuf::from(p));
        let read_results = paths.clone().map(|p| {
            let yaml_location = p.join("contents.lc.yaml");
            ReadResultForPath(reader.read_to_string(yaml_location.as_path()), p)
        });
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
        if read_results.iter().all(
            |UnpopulatedClusterResultWithMetadata {
                 unpopulated_cluster_with_contents_file_contents,
                 ..
             }| unpopulated_cluster_with_contents_file_contents.is_ok(),
        ) {
            let total_result = read_results
                .into_iter()
                .map(
                    |UnpopulatedClusterResultWithMetadata {
                         cluster_path,
                         unpopulated_cluster_with_contents_file_contents,
                     }| {
                        let tup = unpopulated_cluster_with_contents_file_contents
                            .expect("Just checked this via .all.");
                        UnpopulatedClusterWithMetadata {
                            cluster_path,
                            unpopulated_cluster: tup.0,
                            contents_file_contents: tup.1,
                        }
                    },
                )
                .collect();
            Ok(LoadedAllUnpopulatedClusters {
                unpopulated_clusters_with_metadata: total_result,
            })
        } else {
            Err(LoadedSubsetOfUnpopulatedClusters {
                unpopulated_cluster_results_with_metadata: read_results,
            })
        }
    }
}

impl LoadedAllUnpopulatedClusters {
    pub(crate) fn generate_cluster_schemas(
        self,
    ) -> Result<AllSchemasGenerated, SubsetOfSchemasGenerated> {
        let ucwms = self.unpopulated_clusters_with_metadata;
        let empty_normal_clusters_with_metadata = ucwms.into_iter().map(
            |UnpopulatedClusterWithMetadata {
                 cluster_path: cp,
                 unpopulated_cluster: uc,
                 contents_file_contents: cfc,
             }| {
                let empty_cluster = uc.to_empty_cluster();
                EmptyClusterWithMetadata {
                    cluster_path: cp,
                    empty_cluster,
                    contents_file_contents: cfc,
                }
            },
        );

        let mut overall_schema = schema_for!(deserialization::ClusterForSerialization);
        let mut plugin_schema = schemars::schema_for!(deserialization::PluginForSerialization);
        plugin_schema.meta_schema = None;
        let mut node_schema = schemars::schema_for!(deserialization::Node);
        node_schema.meta_schema = None;
        let mut mandatory_fields = HashSet::new();

        for encwm in empty_normal_clusters_with_metadata {
            let cluster_path = &encwm.cluster_path;
            let cluster = &encwm.empty_cluster;
        }
        todo!("Complete schema generation! Won't need contents_file_contents, that only comes in when nodes and edges are loaded.")
    }
}

// TODO: do I have tests for the following function from before? If not, should write those first.
/*
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
 */

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::NoDataLoaded;
    use crate::readers;

    #[test]
    fn simple_unpopulated_clusters() {
        let mut reader = readers::RealFileReader {};
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
        let pipeline = NoDataLoaded::new().load_unpopulated_clusters(&combined_paths, &mut reader);
        assert!(
            pipeline.is_ok(),
            "This should yield a state on the happy path."
        );
    }

    #[test]
    fn unpopulated_clusters_with_noop_plugins() {
        let mut reader = readers::RealFileReader {};
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
        let resulting_state =
            NoDataLoaded::new().load_unpopulated_clusters(&combined_paths, &mut reader);
        match resulting_state {
            Ok(state) => {
                let ucwms = state.unpopulated_clusters_with_metadata;
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
            Err(state) => {
                dbg!(state);
                panic!("Wound up off the expected happy path.");
            }
        }
    }

    #[test]
    fn unpopulated_clusters_with_missing_plugins() {
        let mut reader = readers::RealFileReader {};
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
        let resulting_state =
            NoDataLoaded::new().load_unpopulated_clusters(&combined_paths, &mut reader);
        match resulting_state {
            Ok(_) => panic!("State should not be without issues."),
            Err(state) => {
                let issues = state.unpopulated_cluster_results_with_metadata;
                assert!(issues.len() == 2);
                assert!(format!("{:#?}", issues[0]).contains("Unable to load Wasm file"));
                assert!(format!("{:#?}", issues[1]).contains("Unable to load Wasm file"));
            }
        }
    }
}
