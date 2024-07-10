#![allow(improper_ctypes_definitions)]

extern crate learning_paths_tauri_react;

use std::collections::{HashMap, HashSet};

use learning_paths_tauri_react::plugins::{ArtifactMapping, Plugin, PreZipPlugin};
use serde_yaml::Value;
use std::path::Path;
use yaml2json_rs::Yaml2Json;

pub struct MoodleFormatPlugin {
    params: HashMap<String, Value>,
}

impl Plugin for MoodleFormatPlugin {
    fn get_name(&self) -> &str {
        "Moodle format"
    }

    fn get_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }

    fn set_params(&mut self, params: HashMap<String, Value>) -> Result<(), String> {
        self.params = params;
        Ok(())
    }
}

impl PreZipPlugin for MoodleFormatPlugin {
    fn process_project(
        &self,
        cluster_paths: Vec<&Path>,
        artifacts: &mut HashSet<ArtifactMapping>,
    ) -> Result<(), anyhow::Error> {
        let added_artifacts: HashSet<_> = artifacts
            .iter()
            .filter_map(|artifact| {
                if artifact
                    .local_file
                    .file_name()
                    .is_some_and(|os_str| os_str.to_string_lossy() == "contents.lc.yaml")
                {
                    let mut clone = artifact.clone();
                    clone.local_file.set_extension("json");
                    Some(clone)
                } else {
                    None
                }
            })
            .collect();
        cluster_paths.iter().for_each(|cluster_path| {
            let contents_path = cluster_path.join("contents.lc.yaml");
            let cluster_contents = std::fs::read_to_string(&contents_path)
                .expect("Has to be there. Deal with absence later.");
            let yaml2json = Yaml2Json::new(yaml2json_rs::Style::PRETTY);
            let json_contents = yaml2json.document_to_string(&cluster_contents);
            // TODO: use Result
            let _ = std::fs::write(
                &cluster_path.join("contents.lc.json"),
                json_contents.expect("Conversion should not be an issue"),
            );
        });
        added_artifacts.into_iter().for_each(|modified_clone| {
            artifacts.insert(modified_clone);
        });
        Ok(())
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn PreZipPlugin {
    let plugin = Box::new(MoodleFormatPlugin {
        params: HashMap::new(),
    });
    Box::into_raw(plugin)
}
