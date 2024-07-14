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
    path: String
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
        let mut overall_schema = schemars::schema_for!(deserialization::ClusterForSerialization);
        let mut plugin_schema = schemars::schema_for!(deserialization::PluginForSerialization);
        println!(
            "Base plugin schema:\n\n{}",
            serde_json::to_string_pretty(&plugin_schema).unwrap()
        );
        let plugin_schema_object = &mut plugin_schema.schema;
        let subschema_validation = plugin_schema_object.subschemas();
        let mut conditional_schemas = vec![];
        // there is only one ClusterForSerialization schema
        // so it should be modified through conditional clauses
        // it can't be duplicated for every individual plugin
        //
        // TODO
        // the "path" property is always required
        // anything else is simply prohibited, *unless* we are dealing with a cluster plugin
        // i.e. unless the path property has a value matching plugin.get_path()
        //
        // should be manageable, so focus on case where we are dealing with a cluster plugin
        // instead of having multiple identical "ifs" per plugin with different "thens"
        // there should be a single "if" for each plugin
        // and the "then" should bundle all properties
        // and it should prohibit any other properties
        cluster.node_plugins.iter().for_each(|plugin| {
            let plugin_path: &String = plugin.get_path();
            let params_and_schemas = plugin.get_params_schema();
            params_and_schemas.iter().for_each(
                |((field_name, field_is_required), field_value_schema)| {
                    // if the path matches the plugin path...
                    let mut if_clause = SchemaObject::new_ref("dummy-ref".into());
                    if_clause.reference = None;
                    if_clause.instance_type = Some(SingleOrVec::from(InstanceType::Object));
                    let mut if_clause_required = BTreeSet::new();
                    if_clause_required.insert("path".into());
                    if_clause.object().required = if_clause_required;
                    let mut if_clause_properties = BTreeMap::new();
                    let mut path_schema = SchemaObject::new_ref("dummy-ref".into());
                    path_schema.reference = None;
                    let mut path_string_validation = StringValidation::default();
                    let escaped_path_string =
                        format!("^{}$", regex::escape(plugin_path));
                    path_string_validation.pattern = Some(escaped_path_string);
                    path_schema.string = Some(Box::new(path_string_validation));
                    if_clause_properties.insert("path".into(), Object(path_schema));
                    if_clause.object().properties = if_clause_properties;
                    // ... the parameter should be checked according to the schema ...
                    let mut then_clause = SchemaObject::new_ref("dummy-ref".into());
                    then_clause.reference = None;
                    then_clause.instance_type = Some(SingleOrVec::from(InstanceType::Object));
                    let mut then_clause_required = BTreeSet::new();
                    if *field_is_required {
                        then_clause_required.insert(field_name.into());
                    }
                    let mut then_clause_properties = BTreeMap::new();
                    then_clause_properties.insert(
                        field_name.into(),
                        serde_json::from_value(field_value_schema.clone())
                            .expect("Assuming (de)serializating by libraries works."),
                    );
                    then_clause.object().required = then_clause_required;
                    then_clause.object().properties = then_clause_properties;
                    // ... and the "if" and "then need to be joined
                    let mut conditional = SchemaObject::new_ref("dummy-ref".into());
                    conditional.reference = None;
                    let conditional_subschemas = conditional.subschemas();
                    conditional_subschemas.if_schema = Some(Box::new(Object(if_clause)));
                    conditional_subschemas.then_schema = Some(Box::new(Object(then_clause)));
                    conditional_schemas.push(Object(conditional));
                },
            );
        });
        subschema_validation.all_of = Some(conditional_schemas);
        println!(
            "Modified plugin schema:\n\n{}",
            serde_json::to_string_pretty(&plugin_schema).unwrap()
        );
        Ok(HashSet::new())
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn ClusterProcessingPlugin {
    let plugin = Box::new(YamlSchemaGenerationPlugin {
        path: "".into()
    });
    Box::into_raw(plugin)
}
