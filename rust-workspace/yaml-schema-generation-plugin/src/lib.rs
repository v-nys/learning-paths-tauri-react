#![allow(improper_ctypes_definitions)]

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use learning_paths_tauri_react::{
    deserialization, domain,
    plugins::{ArtifactMapping, ClusterProcessingPlugin, Plugin},
};
use schemars::JsonSchema;
use serde_yaml::Value;
use std::path::Path;

pub struct YamlSchemaGenerationPlugin {}

#[derive(JsonSchema)]
pub struct PluginParameters {}

impl Plugin for YamlSchemaGenerationPlugin {
    fn set_params(&mut self, params: HashMap<String, Value>) -> Result<(), String> {
        if params.is_empty() {
            Ok(())
        } else {
            Err("This plugin currently does not support any parameters.".into())
        }
    }

    fn get_params_schema(&self) -> HashMap<(String, bool), serde_json::Value> {
        HashMap::new()
    }

    fn get_name(&self) -> &str {
        "YAML schema generation"
    }

    fn get_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }
}

impl ClusterProcessingPlugin for YamlSchemaGenerationPlugin {
    fn process_cluster(
        &self,
        _cluster_path: &Path,
        cluster: &domain::Cluster,
    ) -> Result<HashSet<ArtifactMapping>, anyhow::Error> {
        let mut overall_schema = schemars::schema_for!(deserialization::ClusterForSerialization);
        let mut plugin_schema = schemars::schema_for!(deserialization::PluginForSerialization);
        let plugin_schema_object = &mut plugin_schema.schema;
        let object_validation = plugin_schema_object.object();
        let mut required_properties = BTreeSet::new();
        let mut properties = BTreeMap::new();
        // println!("Here is the basic cluster schema:");
        // println!("{:#?}", overall_schema);
        // TODO: don't just do this for specific plugin
        // will probably need to let each plugin know its own path
        cluster
            .node_plugins
            .iter()
            .for_each(|plugin| {
                println!("Adding to the plugin schema...");
                let params_and_schemas = plugin.get_params_schema();
                params_and_schemas.iter().for_each(
                    |((field, field_is_required), serialized_schema)| {
                        if *field_is_required {
                            let _ = required_properties.insert(field.into());
                        }
                        let deserialized_schema = serde_json::from_value(serialized_schema.clone())
                            .expect("This should have been properly serialized.");
                        let _ = properties.insert(field.into(), deserialized_schema);
                    },
                );
            });
        object_validation.required.extend(required_properties.into_iter()); // = required_properties;
        object_validation.properties.extend(properties.into_iter()); // = properties;
        println!(
            "Modified plugin schema:\n\n{}",
            serde_json::to_string_pretty(&plugin_schema).unwrap()
        );

        // for each hashmap entry, add a property whose subschema is given
        // to get the overall schema, use a Visitor to traverse the schema for Cluster?
        // then write the overall schema to the cluster's directory and voila

        /*
        cluster.pre_cluster_plugins.iter().for_each(|plugin| {
            println!("{:#?}", plugin.get_params_schema())
        });
        cluster.pre_zip_plugins.iter().for_each(|plugin| {
            println!("{:#?}", plugin.get_params_schema())
        });
        */
        Ok(HashSet::new())
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn ClusterProcessingPlugin {
    let plugin = Box::new(YamlSchemaGenerationPlugin {});
    Box::into_raw(plugin)
}
