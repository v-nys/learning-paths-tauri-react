#![allow(improper_ctypes_definitions)]

// extern crate logic_based_learning_paths;

use std::path::PathBuf;
use std::collections::{HashSet, HashMap};
use std::fs::File;
use schemars::{schema_for, JsonSchema};

use logic_based_learning_paths::domain;
use logic_based_learning_paths::prelude::{serde_yaml, serde_json, schemars, anyhow};
use logic_based_learning_paths::plugins::{
    ArtifactMapping, NodeProcessingError, NodeProcessingPlugin, Plugin,
};
use serde::Deserialize;
use serde_yaml::Value;
use std::path::Path;
// use serde_json;

pub struct AssignmentsPlugin {
    params: HashMap<String, Value>,
    path: String
}

#[derive(JsonSchema)]
#[schemars(deny_unknown_fields)]
pub struct PluginParameters {
    require_model_solutions: Option<bool>
}

#[derive(Deserialize, Debug, Clone, JsonSchema)]
struct Assignment {
    id: String,
    //title: Option<String>,
    attachments: Option<Vec<String>>, // TODO: best ook "vlak" houden, want path separator betekent incompatibiliteit Windows - UNIX...
}

fn file_is_readable(file_path: &Path) -> bool {
    file_path.is_file() && File::open(file_path).is_ok()
}

impl Plugin for AssignmentsPlugin {

    fn set_path(&mut self, path: String) {
        self.path = path;
    }

    fn get_path(&self) -> &String {
        &self.path
    }

    fn get_name(&self) -> &str {
        "Assignments"
    }

    fn get_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }

    fn set_params(&mut self, params: HashMap<String, Value>) -> Result<(), String> {
        // TODO: properly validate, should match schema!
        self.params = params;
        Ok(())
    }

    fn get_params_schema(&self) -> HashMap<(String, bool), String> {
        let schema = schemars::schema_for!(Option<bool>);
        let mut parameters = HashMap::new();
        // parameters.insert(("require_model_solutions".into(), false), serde_json::to_value(schema).unwrap());
        parameters
    }

}

impl NodeProcessingPlugin for AssignmentsPlugin {

    fn get_mandatory_fields(&self) -> HashSet<String> {
        HashSet::new()
    }

    fn get_extension_field_schema(&self) -> HashMap<(String, bool), serde_json::Value> {
        let mut map = HashMap::new();
        let _ = map.insert(("assignments".into(), false),
                           serde_json::to_value(schema_for!(Vec<Assignment>)).unwrap());
        map

    }

    fn process_extension_field(
        &self,
        cluster_path: &Path,
        node: &domain::Node,
        field_name: &str,
        value: &Value,
    ) -> Result<HashSet<ArtifactMapping>, NodeProcessingError> {
        // TODO: base on schema instead
        if field_name != "assignments" {
            Err(NodeProcessingError::CannotProcessFieldType)
        } else {
            let assignments = serde_yaml::from_value::<Vec<Assignment>>(value.clone());
            match assignments {
                Ok(assignments) => {
                    let mut additional_remarks = vec![];
                    let mut artifacts = HashSet::new();
                    let assignment_ids: HashSet<_> = assignments.iter().map(|a| &a.id).collect();
                    if assignment_ids.len() < assignments.len() {
                        additional_remarks
                            .push(format!("Duplicate assignment IDs in node {}", node.node_id));
                    }
                    assignments.iter().for_each(|assignment| {
                        let base_assignment_path = cluster_path.join(node.node_id.local_id.clone()).join(&assignment.id);
                        let contents_path = base_assignment_path.join("contents.html");
                        if !file_is_readable(&contents_path) {
                            additional_remarks.push(
                                format!("Assignment {} associated with node {} lacks a readable contents.html file.", 
                                        &assignment.id,
                                        node.node_id.local_id)
                                );
                        }
                        else {
                            artifacts.insert(ArtifactMapping {
                                local_file: contents_path,
                                root_relative_target_dir: PathBuf::from(format!("{}/{}/assignments/{}", &node.node_id.namespace, &node.node_id.local_id, &assignment.id))
                            });
                            
                        }
                        if let Some(attachments) = assignment.attachments.as_ref() {
                            attachments.iter().for_each(|attachment| {
                                let attachment_path = base_assignment_path.join(attachment);
                                if !file_is_readable(attachment_path.as_path()) {
                                    additional_remarks.push(format!("Attachment cannot be read at {}", attachment_path.to_string_lossy()));
                                }
                                else {
                            artifacts.insert(
                                ArtifactMapping {
                                    local_file: attachment_path,
                                    root_relative_target_dir: PathBuf::from(format!("{}/{}/assignments/{}/attachments", &node.node_id.namespace, &node.node_id.local_id, &assignment.id))
                                });
                                }
                            });
                        }
                    });
                    // attachments should be present and readable
                    if additional_remarks.is_empty() {
                        Ok(artifacts)
                    } else {
                        Err(NodeProcessingError::Remarks(additional_remarks))
                    }
                }
                Err(e) => Err(NodeProcessingError::Remarks(vec![format!(
                    "Something went wrong while deserializing assignments in node {}: {}",
                    node.node_id,
                    e.to_string()
                )])),
            }
        }
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn NodeProcessingPlugin {
    let plugin = Box::new(AssignmentsPlugin { params: HashMap::new(), path: "".into() });
    Box::into_raw(plugin)
}
