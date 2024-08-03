#![allow(improper_ctypes_definitions)]

use logic_based_learning_paths::prelude::{schemars, serde_json, serde_yaml};

use logic_based_learning_paths::domain;
use logic_based_learning_paths::plugins::{
    ArtifactMapping, NodeProcessingError, NodeProcessingPlugin, Plugin,
};
use schemars::JsonSchema;
use serde_yaml::Value;
use std::collections::{HashMap, HashSet};
use std::path::Path;

pub struct DummyNodePlugin {
    params: HashMap<String, Value>,
    path: String,
}

#[derive(JsonSchema)]
#[schemars(deny_unknown_fields)]
#[allow(dead_code)]
pub struct PluginParameters {
    param1: u64,
    param2: bool,
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
        let u64_schema = schemars::schema_for!(u64);
        let bool_schema = schemars::schema_for!(bool);
        let mut parameters = HashMap::new();
        parameters.insert(("param1".into(), true), serde_json::to_value(u64_schema).expect("Should be convertible."));
        parameters.insert(("param2".into(), true), serde_json::to_value(bool_schema).expect("Should be convertible."));
        parameters
    }
}

impl NodeProcessingPlugin for DummyNodePlugin {
    fn get_mandatory_fields(&self) -> HashSet<String> {
        let mut fields = HashSet::new();
        fields.insert("foo".into());
        fields
    }

    fn get_extension_field_schema(&self) -> HashMap<(String, bool), serde_json::Value> {
        let mut schema = HashMap::new();
        let u32_schema = schemars::schema_for!(u32);
        schema.insert(("foo".into(), true), serde_json::to_value(u32_schema).unwrap());
        schema
    }

    fn process_extension_field(
        &self,
        _cluster_path: &Path,
        _node: &domain::Node,
        field_name: &str,
        _value: &Value,
    ) -> Result<HashSet<ArtifactMapping>, NodeProcessingError> {
        if field_name == "foo" {
            Ok(HashSet::new())
        }
        else {
            Err(NodeProcessingError::CannotProcessFieldType)
        }
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn NodeProcessingPlugin {
    let plugin = Box::new(DummyNodePlugin {
        params: HashMap::new(),
        path: "".into(),
    });
    Box::into_raw(plugin)
}
