#![allow(improper_ctypes_definitions)]

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use learning_paths_tauri_react::{
    deserialization, domain,
    plugins::{ArtifactMapping, ClusterProcessingPlugin, Plugin},
};

use regex;
use schemars::{
    schema::{InstanceType, Schema::Object, SchemaObject, SingleOrVec, StringValidation},
    JsonSchema,
};
use serde_yaml::Value;
use std::path::Path;

pub struct YamlSchemaGenerationPlugin {
    path: String,
}

#[derive(JsonSchema)]
#[schemars(deny_unknown_fields)]
pub struct PluginParameters {}

impl Plugin for YamlSchemaGenerationPlugin {
    fn get_path(&self) -> &String {
        &self.path
    }

    fn set_path(&mut self, path: String) {
        self.path = path;
    }

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
        let plugin_schema = schemars::schema_for!(deserialization::PluginForSerialization);
        // TODO: don't just restrict to node plugins!
        let plugin_paths_to_schemas: HashMap<&String, SchemaObject> = cluster
            .node_plugins
            .iter()
            // this doesn't work because we have an iterator over a specific kind of container
            // .chain(cluster.pre_zip_plugins.iter())
            // .chain(cluster.pre_cluster_plugins.iter())
            .map(|plugin| {
                let params_and_schemas = plugin.get_params_schema();
                let mut schema_for_plugin = plugin_schema.clone();
                // cloning rather than using &mut because I'd have 2 &muts into the same struct
                let mut required_properties_for_plugin =
                    schema_for_plugin.schema.object().required.clone();
                let mut properties_for_plugin =
                    schema_for_plugin.schema.object().properties.clone();
                params_and_schemas
                    .iter()
                    .for_each(|((param, required), param_schema)| {
                        if *required {
                            required_properties_for_plugin.insert(param.into());
                        }
                        let param_schema = serde_json::from_value(param_schema.clone())
                            .expect("Assuming (de)serializating by libraries works.");
                        properties_for_plugin.insert(param.into(), param_schema);
                    });
                schema_for_plugin.schema.object().required = required_properties_for_plugin;
                schema_for_plugin.schema.object().properties = properties_for_plugin;
                (plugin.get_path(), schema_for_plugin.schema)
            })
            .collect();

        // can probably avoid some cloning using into_iter...
        let conditional_schema = plugin_paths_to_schemas.iter().fold(
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
                let escaped_path_string = format!("^{}$", regex::escape(plugin_path));
                path_string_validation.pattern = Some(escaped_path_string);
                path_schema.string = Some(Box::new(path_string_validation));
                if_clause_properties.insert("path".into(), Object(path_schema));
                if_clause.object().properties = if_clause_properties;
                let mut conditional = SchemaObject::new_ref("dummy-ref".into());
                conditional.reference = None;
                let conditional_subschemas = conditional.subschemas();
                conditional_subschemas.if_schema = Some(Box::new(Object(if_clause)));
                conditional_subschemas.then_schema =
                    Some(Box::new(Object(plugin_schema_object.clone())));
                conditional_subschemas.else_schema = Some(Box::new(Object(acc)));
                conditional
            },
        );
        println!(
            "Modified plugin schema:\n\n{}",
            serde_json::to_string_pretty(&conditional_schema).unwrap()
        );
        Ok(HashSet::new())
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn ClusterProcessingPlugin {
    let plugin = Box::new(YamlSchemaGenerationPlugin { path: "".into() });
    Box::into_raw(plugin)
}
