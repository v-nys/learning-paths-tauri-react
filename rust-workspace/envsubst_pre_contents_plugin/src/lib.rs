#![allow(improper_ctypes_definitions)]
use envsubst::substitute;

use std::collections::{HashMap, HashSet};

use logic_based_learning_paths::{
    domain,
    plugins::{ArtifactMapping, ClusterProcessingPlugin, Plugin},
    prelude::{anyhow, schemars, serde_json, serde_yaml},
};

use schemars::JsonSchema;
use serde_yaml::Value;
use std::path::Path;

pub struct EnvsubstPreContentsPlugin {
    path: String,
}

#[derive(JsonSchema)]
#[schemars(deny_unknown_fields)]
pub struct PluginParameters {}

impl Plugin for EnvsubstPreContentsPlugin {
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
        "envsubst pre-contents"
    }

    fn get_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }
}

impl ClusterProcessingPlugin for EnvsubstPreContentsPlugin {
    fn process_cluster(
        &self,
        cluster_path: &Path,
        _cluster: &domain::Cluster,
    ) -> Result<HashSet<ArtifactMapping>, anyhow::Error> {
        let input_file_contents =
            std::fs::read_to_string(cluster_path.join("pre-contents.lc.yaml"))?;
        let replacement_text = substitute(input_file_contents, &std::env::vars().filter(|e| e.0 == "APPLICATION_SOURCE_DIR").collect())?;
        std::fs::write(cluster_path.join("contents.lc.yaml"), replacement_text)?;
        Ok(HashSet::new())
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn ClusterProcessingPlugin {
    let plugin = Box::new(EnvsubstPreContentsPlugin { path: "".into() });
    Box::into_raw(plugin)
}
