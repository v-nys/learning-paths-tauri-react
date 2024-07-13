#![allow(improper_ctypes_definitions)]

use std::collections::{HashMap, HashSet};

use schemars::JsonSchema;
use std::path::Path;
use learning_paths_tauri_react::{domain, plugins::{ArtifactMapping, ClusterProcessingPlugin, Plugin}};
use serde_yaml::Value;

pub struct YamlSchemaGenerationPlugin {
    params: HashMap<String, Value>,
}

#[derive(JsonSchema)]
pub struct PluginParameters {
}

impl Plugin for YamlSchemaGenerationPlugin {
    fn get_params_schema(&self) -> serde_json::Value {
        let schema = schemars::schema_for!(PluginParameters);
        serde_json::to_value(schema).unwrap()
    }

    fn get_name(&self) -> &str {
        "YAML schema generation"
    }

    fn get_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }

    fn set_params(&mut self, params: HashMap<String, Value>) -> Result<(), String> {
        self.params = params;
        Ok(())
    }
}

impl ClusterProcessingPlugin for YamlSchemaGenerationPlugin {
    fn process_cluster(
        &self,
        _cluster_path: &Path,
        _cluster: &domain::Cluster
    ) -> Result<HashSet<ArtifactMapping>, anyhow::Error> {
        Ok(HashSet::new())
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn ClusterProcessingPlugin {
    let plugin = Box::new(YamlSchemaGenerationPlugin {
        params: HashMap::new(),
    });
    Box::into_raw(plugin)
}
