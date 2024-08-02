#![allow(improper_ctypes_definitions)]

use std::path::Path;
use schemars::JsonSchema;
use std::collections::{HashSet, HashMap};
use serde_yaml::Value;
use logic_based_learning_paths::domain;
use logic_based_learning_paths::plugins::{Plugin, NodeProcessingPlugin, NodeProcessingError, ArtifactMapping};


pub struct DummyNodePlugin {
    params: HashMap<String, Value>,
    path: String
}

#[derive(JsonSchema)]
#[schemars(deny_unknown_fields)]
#[allow(dead_code)]
pub struct PluginParameters {
    param1: u64,
    param2: bool
}

impl Plugin for DummyNodePlugin {

    fn set_path(&mut self, path: String) {
        self.path = path;
    }

    fn get_path(&self) -> &String {
        &self.path
    }

    fn get_name(&self) -> &str {
        "Dummy node plugin"
    }

    fn get_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }

    fn set_params(&mut self, params: HashMap<String, Value>) -> Result<(), String> {
        self.params = params;
        Ok(())
    }

    fn get_params_schema(&self) -> HashMap<(String, bool), serde_json::Value> {
        // let u64_schema = schemars::schema_for!(u64);
        // let bool_schema = schemars::schema_for!(bool);
        let mut parameters = HashMap::new();
        // parameters.insert(("param1".into(), true), serde_json::to_value(u64_schema).unwrap());
        // parameters.insert(("param2".into(), true), serde_json::to_value(bool_schema).unwrap());
        parameters
    }

}

impl NodeProcessingPlugin for DummyNodePlugin {

    fn get_mandatory_fields(&self) -> HashSet<String> {
        HashSet::new()
    }

    fn get_extension_field_schema(&self) -> HashMap<(String, bool), serde_json::Value> {
        HashMap::new()
    }

    fn process_extension_field(
        &self,
        _cluster_path: &Path,
        _node: &domain::Node,
        _field_name: &str,
        _value: &Value,
    ) -> Result<HashSet<ArtifactMapping>, NodeProcessingError> {
        Err(NodeProcessingError::CannotProcessFieldType)
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn NodeProcessingPlugin {
    let plugin = Box::new(DummyNodePlugin { params: HashMap::new(), path: "".into() });
    Box::into_raw(plugin)
}
