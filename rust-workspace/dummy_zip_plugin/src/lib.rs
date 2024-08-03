#![allow(improper_ctypes_definitions)]

use logic_based_learning_paths::prelude::{schemars, serde_json, serde_yaml};

use logic_based_learning_paths::plugins::{ArtifactMapping, Plugin, PreZipPlugin};
use schemars::JsonSchema;
use serde_yaml::Value;
use std::collections::{HashMap, HashSet};
use std::path::Path;

pub struct DummyZipPlugin {
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

impl Plugin for DummyZipPlugin {
    fn set_path(&mut self, path: String) {
        self.path = path;
    }

    fn get_path(&self) -> &String {
        &self.path
    }

    fn get_name(&self) -> &str {
        "Dummy zip plugin"
    }

    fn get_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }

    fn set_params(&mut self, params: HashMap<String, Value>) -> Result<(), String> {
        self.params = params;
        Ok(())
    }

    fn get_params_schema(&self) -> HashMap<(String, bool), String> {
        let u64_schema = schemars::schema_for!(u64);
        let bool_schema = schemars::schema_for!(bool);
        let mut parameters = HashMap::new();
        parameters.insert(
            ("param1".into(), true),
            serde_json::to_string(&u64_schema).expect("Should be stringifyable."),
        );
        parameters.insert(
            ("param2".into(), true),
            serde_json::to_string(&bool_schema).expect("Should be stringifyable."),
        );
        parameters
    }
}

impl PreZipPlugin for DummyZipPlugin {
    fn process_project(
        &self,
        _cluster_paths: Vec<&Path>,
        _artifacts: &mut HashSet<ArtifactMapping>,
    ) -> Result<(), logic_based_learning_paths::prelude::anyhow::Error> {
        Ok(())
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn PreZipPlugin {
    let plugin = Box::new(DummyZipPlugin {
        params: HashMap::new(),
        path: "".into(),
    });
    Box::into_raw(plugin)
}
