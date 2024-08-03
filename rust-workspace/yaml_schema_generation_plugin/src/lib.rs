#![allow(improper_ctypes_definitions)]

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use logic_based_learning_paths::{
    deserialization, domain,
    plugins::{ArtifactMapping, ClusterProcessingPlugin, Plugin},
    prelude::{anyhow, schemars, serde_json, serde_yaml},
};

use regex;
use schemars::{
    schema::{
        InstanceType, RootSchema, Schema::Object, SchemaObject, SingleOrVec, StringValidation,
    },
    schema_for, JsonSchema,
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

    fn get_params_schema(&self) -> HashMap<(String, bool), String> {
        HashMap::new()
    }

    fn get_name(&self) -> &str {
        "YAML schema generation"
    }

    fn get_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }
}

fn plugin_to_paths_to_schemas_entry(
    plugin_path: &String,
    params_and_schemas: HashMap<(String, bool), String>,
    mut schema_for_plugin: RootSchema,
) -> (&String, RootSchema) {
    let mut required_properties_for_plugin = schema_for_plugin.schema.object().required.clone();
    let mut properties_for_plugin = schema_for_plugin.schema.object().properties.clone();
    params_and_schemas
        .iter()
        .for_each(|((param, required), param_schema)| {
            if *required {
                required_properties_for_plugin.insert(param.into());
            }
            let mut param_schema: RootSchema = serde_json::from_str(&param_schema.clone())
                .expect("Assuming (de)serializating by libraries works.");
            param_schema.meta_schema = None;
            properties_for_plugin.insert(param.into(), Object(param_schema.schema));
        });
    schema_for_plugin.schema.object().required = required_properties_for_plugin;
    schema_for_plugin.schema.object().properties = properties_for_plugin;
    (plugin_path, schema_for_plugin)
}

impl YamlSchemaGenerationPlugin {
    fn process_cluster_with_writer(
        &self,
        cluster: &domain::Cluster,
        writer: &mut impl std::io::Write,
    ) -> Result<HashSet<ArtifactMapping>, anyhow::Error> {
        let mut overall_schema = schema_for!(deserialization::ClusterForSerialization);
        let mut plugin_schema = schemars::schema_for!(deserialization::PluginForSerialization);
        plugin_schema.meta_schema = None;
        let mut node_schema = schemars::schema_for!(deserialization::Node);
        node_schema.meta_schema = None;
        cluster.node_plugins.iter().for_each(|node_plugin| {
            let extension_field_schema = node_plugin.get_extension_field_schema();
            extension_field_schema
                .iter()
                .for_each(|((field, required), field_schema)| {
                    if *required {
                        node_schema.schema.object().required.insert(field.into());
                    }
                    let mut field_schema: RootSchema = serde_json::from_value(field_schema.clone())
                        .expect("Assuming (de)serializating by libraries works.");
                    field_schema.meta_schema = None;
                    node_schema
                        .schema
                        .object()
                        .properties
                        .insert(field.into(), Object(field_schema.schema));
                    field_schema
                        .definitions
                        .iter()
                        .for_each(|(ref_string, schema)| {
                            overall_schema
                                .definitions
                                .insert(ref_string.into(), schema.clone());
                        });
                });
        });
        // seems like iterating over the node plugins is causing an issue
        let mut plugin_paths_to_schemas: HashMap<&String, RootSchema> = cluster
            .node_plugins
            .iter()
            // filtering is not necessary but simplifies the eventual schema
            .filter(|plugin| !plugin.get_params_schema().is_empty())
            .map(|plugin| {
                plugin_to_paths_to_schemas_entry(
                    plugin.get_path(),
                    plugin.get_params_schema(),
                    plugin_schema.clone(),
                )
            })
            .collect();
        cluster.pre_cluster_plugins.iter().for_each(|plugin| {
            if !plugin.get_params_schema().is_empty() {
                let (key, value) = plugin_to_paths_to_schemas_entry(
                    plugin.get_path(),
                    plugin.get_params_schema(),
                    plugin_schema.clone(),
                );
                plugin_paths_to_schemas.insert(key, value);
            }
        });
        cluster.pre_zip_plugins.iter().for_each(|plugin| {
            if !plugin.get_params_schema().is_empty() {
                let (key, value) = plugin_to_paths_to_schemas_entry(
                    plugin.get_path(),
                    plugin.get_params_schema(),
                    plugin_schema.clone(),
                );
                plugin_paths_to_schemas.insert(key, value);
            }
        });
        plugin_paths_to_schemas.values().for_each(|root_schema| {
            root_schema
                .definitions
                .iter()
                .for_each(|(ref_string, schema)| {
                    overall_schema
                        .definitions
                        .insert(ref_string.into(), schema.clone());
                });
        });
        let mut sorted_plugin_paths_to_schemas = plugin_paths_to_schemas.iter().collect::<Vec<_>>();
        sorted_plugin_paths_to_schemas.sort_by(|a, b| a.0.cmp(b.0));
        let conditional_schema = sorted_plugin_paths_to_schemas.iter().fold(
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
                    Some(Box::new(Object(plugin_schema_object.schema.clone())));
                conditional_subschemas.else_schema = Some(Box::new(Object(acc)));
                conditional
            },
        );
        overall_schema
            .definitions
            .insert("PluginForSerialization".into(), Object(conditional_schema));
        overall_schema
            .definitions
            .insert("Node".into(), Object(node_schema.schema));

        writer.write(&serde_json::to_string_pretty(&overall_schema)?.as_bytes())?;
        Ok(HashSet::new())
    }
}

#[cfg(test)]
mod tests {
    use logic_based_learning_paths::deserialization::ClusterForSerialization;
    use pretty_assertions::assert_eq;

    use super::*;

    // note: this is written out as a macro so it can be expanded
    macro_rules! format_cluster_schema {
    ($cluster_for_serialization_definition:expr, $node_definition:expr) => {
        format!(r###"
{{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "ClusterForSerialization",
  "description": "A representation of a `Cluster` which is more suitable for (de)serialization.\n\nIt does not require a namespace prefix, as that is assumed to match the name of the file to which it is serialized. It uses disjoint, optional sets of edges because that saves a lot of repetition when writing in a data format.",
  "type": "object",
  "required": [
    "nodes"
  ],
  "properties": {{
    "all_type_edges": {{
      "description": "Strict dependencies. A non-root `Node` can only be accessed if all of its dependencies of this type have been marked complete, along with one interchangeable dependency of this `Node` or of a `Node` which is strictly dependent on this `Node`.",
      "type": [
        "array",
        "null"
      ],
      "items": {{
        "$ref": "#/definitions/Edge"
      }}
    }},
    "any_type_edges": {{
      "description": "Interchangeable dependencies. A non-root `Node` can only be accessed if one dependency of this type has been marked complete for this node or for a `Node` which is strictly dependent on this `Node`. Furthermore, all strict dependencies must still be marked complete.",
      "type": [
        "array",
        "null"
      ],
      "items": {{
        "$ref": "#/definitions/Edge"
      }}
    }},
    "node_plugins": {{
      "type": [
        "array",
        "null"
      ],
      "items": {{
        "$ref": "#/definitions/PluginForSerialization"
      }}
    }},
    "nodes": {{
      "description": "Units of information inside this `Cluster`.",
      "type": "array",
      "items": {{
        "$ref": "#/definitions/Node"
      }}
    }},
    "pre_cluster_plugins": {{
      "type": [
        "array",
        "null"
      ],
      "items": {{
        "$ref": "#/definitions/PluginForSerialization"
      }}
    }},
    "pre_zip_plugins": {{
      "type": [
        "array",
        "null"
      ],
      "items": {{
        "$ref": "#/definitions/PluginForSerialization"
      }}
    }},
    "roots": {{
      "description": "IDs of `Node`s with no dependencies whatsoever, i.e. the only `Node`s which can be accessed unconditionally.",
      "type": [
        "array",
        "null"
      ],
      "items": {{
        "type": "string"
      }}
    }}
  }},
  "additionalProperties": false,
  "definitions": {{
    "Edge": {{
      "type": "object",
      "required": [
        "end_id",
        "start_id"
      ],
      "properties": {{
        "end_id": {{
          "type": "string"
        }},
        "start_id": {{
          "type": "string"
        }}
      }},
      "additionalProperties": false
    }},
    "Node": {},
    "PluginForSerialization": {}
  }}
}}
"###
,
$cluster_for_serialization_definition, $node_definition)
    };
}

    const NODE_DEFINITION_WITHOUT_EXTENSION_FIELDS: &str = r###"{
      "title": "Node",
      "description": "Deserialization counterpart for the domain concept `Node`.",
      "type": "object",
      "required": [
        "id",
        "title"
      ],
      "properties": {
        "id": {
          "description": "An ID should be locally unique inside a `Cluster` and is used to refer to a node inside its `Cluster`.\n\nThe ID also be used to refer to the node from outside its `Cluster`, if it is preceded by the `Cluster`'s namespace prefix.",
          "type": "string"
        },
        "title": {
          "description": "Human-readable title for this unit of knowledge.\n\nThis is not required to be unique at any level.",
          "type": "string"
        }
      },
      "additionalProperties": false
    }"###;

    const FLAKE_DIR: &str = env!("FLAKE_DIR");

    fn template(
        cluster_name: &str,
        number_of_node_plugins: usize,
        number_of_pre_cluster_plugins: usize,
        number_of_pre_zip_plugins: usize,
        expected_schema_contents: &str,
    ) {
        // normally, the plugin would be part of this cluster
        // but testing that would require much more scaffolding
        // specifically, downcasting to YamlGenerationPlugin
        // this requires an external crate
        // so here we *apply* the plugin to a cluster that does not actually have this plugin
        let current_dir = std::env::current_dir().expect("Should be accessible.");
        let cluster_contents = current_dir.join(format!("tests/{}/contents.lc.yaml", cluster_name));
        let cluster: ClusterForSerialization = serde_yaml::from_str(
            &std::fs::read_to_string(cluster_contents)
                .expect("File should be there and should be readable."),
        )
        .expect("Should be able to deserialize.");
        let node_namespace = cluster_name.into();
        let cluster = cluster.build(node_namespace);
        assert!(cluster.is_ok());
        let cluster = cluster.unwrap();
        assert_eq!(cluster.node_plugins.iter().len(), number_of_node_plugins);
        assert_eq!(
            cluster.pre_cluster_plugins.iter().len(),
            number_of_pre_cluster_plugins
        );
        assert_eq!(
            cluster.pre_zip_plugins.iter().len(),
            number_of_pre_zip_plugins
        );
        let plugin = YamlSchemaGenerationPlugin {
            path: "fake_path_because_plugin_was_not_dynamically_loaded_in_test".into(),
        };
        let mut writer = Vec::new();
        let _ = plugin
            .process_cluster_with_writer(&cluster, &mut writer)
            .expect("There should be a processing result.");
        let schema = String::from_utf8(writer).expect("Should have a valid schema string.");
        assert_eq!(schema.trim(), expected_schema_contents.trim());
    }

    #[test]
    fn cluster_without_plugins() {
        template(
            "dummycluster_without_plugins",
            0,
            0,
            0,
            &format_cluster_schema!(
                NODE_DEFINITION_WITHOUT_EXTENSION_FIELDS,
                r###"{
      "title": "PluginForSerialization",
      "type": "object",
      "required": [
        "path"
      ],
      "properties": {
        "path": {
          "type": "string"
        }
      },
      "additionalProperties": false
    }"###
            ),
        );
    }

    #[test]
    fn cluster_with_parameterized_node_plugin() {
        let mut expected_output_builder = String::new();
        // doing it this way because { would need to become {{ to use format!
        expected_output_builder.push_str(
            r###"{
      "if": {
        "type": "object",
        "required": [
          "path"
        ],
        "properties": {
          "path": {
            "pattern": "^"###,
        );
        expected_output_builder.push_str(&regex::escape(FLAKE_DIR));
        expected_output_builder.push_str(
            r###"/rust\\-workspace/target/debug/liblblp_dummy_node_plugin\\.so$"
          }
        }
      },
      "then": {
        "title": "PluginForSerialization",
        "type": "object",
        "required": [
          "param1",
          "param2",
          "path"
        ],
        "properties": {
          "param1": {
            "title": "uint64",
            "type": "integer",
            "format": "uint64",
            "minimum": 0.0
          },
          "param2": {
            "title": "Boolean",
            "type": "boolean"
          },
          "path": {
            "type": "string"
          }
        },
        "additionalProperties": false
      },
      "else": {
        "title": "PluginForSerialization",
        "type": "object",
        "required": [
          "path"
        ],
        "properties": {
          "path": {
            "type": "string"
          }
        },
        "additionalProperties": false
      }
    }"###,
        );
        template(
            "dummycluster_with_parameterized_node_plugin",
            1,
            0,
            0,
            &format_cluster_schema!(
                NODE_DEFINITION_WITHOUT_EXTENSION_FIELDS,
                &expected_output_builder
            ),
        );
    }

    #[test]
    fn cluster_with_parameterized_pre_cluster_plugin() {
        let mut expected_output_builder = String::new();
        expected_output_builder.push_str(
            r###"{
      "if": {
        "type": "object",
        "required": [
          "path"
        ],
        "properties": {
          "path": {
            "pattern": "^"###,
        );
        expected_output_builder.push_str(FLAKE_DIR);
        expected_output_builder.push_str(
            r###"/rust\\-workspace/target/debug/liblblp_dummy_cluster_plugin\\.so$"
          }
        }
      },
      "then": {
        "title": "PluginForSerialization",
        "type": "object",
        "required": [
          "param1",
          "param2",
          "path"
        ],
        "properties": {
          "param1": {
            "title": "uint64",
            "type": "integer",
            "format": "uint64",
            "minimum": 0.0
          },
          "param2": {
            "title": "Boolean",
            "type": "boolean"
          },
          "path": {
            "type": "string"
          }
        },
        "additionalProperties": false
      },
      "else": {
        "title": "PluginForSerialization",
        "type": "object",
        "required": [
          "path"
        ],
        "properties": {
          "path": {
            "type": "string"
          }
        },
        "additionalProperties": false
      }
    }"###,
        );
        template(
            "dummycluster_with_parameterized_pre_cluster_plugin",
            0,
            1,
            0,
            &format_cluster_schema!(
                NODE_DEFINITION_WITHOUT_EXTENSION_FIELDS,
                expected_output_builder
            ),
        );
    }

    #[test]
    fn cluster_with_parameterized_pre_zip_plugin() {
        let mut expected_output_builder = String::new();
        expected_output_builder.push_str(r###""###);
        expected_output_builder.push_str(FLAKE_DIR);
        expected_output_builder.push_str(r###""###);

        template(
            "dummycluster_with_parameterized_pre_zip_plugin",
            0,
            0,
            1,
            &format_cluster_schema!(
                NODE_DEFINITION_WITHOUT_EXTENSION_FIELDS,
                r###"{
      "if": {
        "type": "object",
        "required": [
          "path"
        ],
        "properties": {
          "path": {
            "pattern": "^/home/vincentn/Projects/logic_based_learning_paths/rust\\-workspace/target/debug/liblblp_dummy_zip_plugin\\.so$"
          }
        }
      },
      "then": {
        "title": "PluginForSerialization",
        "type": "object",
        "required": [
          "param1",
          "param2",
          "path"
        ],
        "properties": {
          "param1": {
            "title": "uint64",
            "type": "integer",
            "format": "uint64",
            "minimum": 0.0
          },
          "param2": {
            "title": "Boolean",
            "type": "boolean"
          },
          "path": {
            "type": "string"
          }
        },
        "additionalProperties": false
      },
      "else": {
        "title": "PluginForSerialization",
        "type": "object",
        "required": [
          "path"
        ],
        "properties": {
          "path": {
            "type": "string"
          }
        },
        "additionalProperties": false
      }
    }"###
            ),
        );
    }

    #[test]
    fn cluster_with_each_type_of_plugin() {
        let mut expected_output_builder = String::new();
        expected_output_builder.push_str(r###""###);
        expected_output_builder.push_str(FLAKE_DIR);
        expected_output_builder.push_str(r###""###);

        template(
            "dummycluster_with_each_type_of_plugin",
            1,
            1,
            1,
            &format_cluster_schema!(
                NODE_DEFINITION_WITHOUT_EXTENSION_FIELDS,
                r###"{
      "if": {
        "type": "object",
        "required": [
          "path"
        ],
        "properties": {
          "path": {
            "pattern": "^/home/vincentn/Projects/logic_based_learning_paths/rust\\-workspace/target/debug/liblblp_dummy_zip_plugin\\.so$"
          }
        }
      },
      "then": {
        "title": "PluginForSerialization",
        "type": "object",
        "required": [
          "param1",
          "param2",
          "path"
        ],
        "properties": {
          "param1": {
            "title": "uint64",
            "type": "integer",
            "format": "uint64",
            "minimum": 0.0
          },
          "param2": {
            "title": "Boolean",
            "type": "boolean"
          },
          "path": {
            "type": "string"
          }
        },
        "additionalProperties": false
      },
      "else": {
        "if": {
          "type": "object",
          "required": [
            "path"
          ],
          "properties": {
            "path": {
              "pattern": "^/home/vincentn/Projects/logic_based_learning_paths/rust\\-workspace/target/debug/liblblp_dummy_node_plugin\\.so$"
            }
          }
        },
        "then": {
          "title": "PluginForSerialization",
          "type": "object",
          "required": [
            "param1",
            "param2",
            "path"
          ],
          "properties": {
            "param1": {
              "title": "uint64",
              "type": "integer",
              "format": "uint64",
              "minimum": 0.0
            },
            "param2": {
              "title": "Boolean",
              "type": "boolean"
            },
            "path": {
              "type": "string"
            }
          },
          "additionalProperties": false
        },
        "else": {
          "if": {
            "type": "object",
            "required": [
              "path"
            ],
            "properties": {
              "path": {
                "pattern": "^/home/vincentn/Projects/logic_based_learning_paths/rust\\-workspace/target/debug/liblblp_dummy_cluster_plugin\\.so$"
              }
            }
          },
          "then": {
            "title": "PluginForSerialization",
            "type": "object",
            "required": [
              "param1",
              "param2",
              "path"
            ],
            "properties": {
              "param1": {
                "title": "uint64",
                "type": "integer",
                "format": "uint64",
                "minimum": 0.0
              },
              "param2": {
                "title": "Boolean",
                "type": "boolean"
              },
              "path": {
                "type": "string"
              }
            },
            "additionalProperties": false
          },
          "else": {
            "title": "PluginForSerialization",
            "type": "object",
            "required": [
              "path"
            ],
            "properties": {
              "path": {
                "type": "string"
              }
            },
            "additionalProperties": false
          }
        }
      }
    }"###
            ),
        );
    }

    #[test]
    fn cluster_with_nodes_with_extension_fields_from_multiple_plugins() {
        let mut expected_output_builder = String::new();
        expected_output_builder.push_str(r###""###);
        expected_output_builder.push_str(FLAKE_DIR);
        expected_output_builder.push_str(r###""###);
        template(
            "dummycluster_with_extension_fields_from_multiple_plugins",
            2,
            0,
            0,
            &format_cluster_schema!(
                r###"{
      "title": "Node",
      "description": "Deserialization counterpart for the domain concept `Node`.",
      "type": "object",
      "required": [
        "bar",
        "foo",
        "id",
        "title"
      ],
      "properties": {
        "bar": {
          "title": "uint16",
          "type": "integer",
          "format": "uint16",
          "minimum": 0.0
        },
        "foo": {
          "title": "uint32",
          "type": "integer",
          "format": "uint32",
          "minimum": 0.0
        },
        "id": {
          "description": "An ID should be locally unique inside a `Cluster` and is used to refer to a node inside its `Cluster`.\n\nThe ID also be used to refer to the node from outside its `Cluster`, if it is preceded by the `Cluster`'s namespace prefix.",
          "type": "string"
        },
        "title": {
          "description": "Human-readable title for this unit of knowledge.\n\nThis is not required to be unique at any level.",
          "type": "string"
        }
      },
      "additionalProperties": false
    }"###,
                r###"{
      "if": {
        "type": "object",
        "required": [
          "path"
        ],
        "properties": {
          "path": {
            "pattern": "^/home/vincentn/Projects/logic_based_learning_paths/rust\\-workspace/target/debug/liblblp_dummy_node_plugin_with_extension_field_2\\.so$"
          }
        }
      },
      "then": {
        "title": "PluginForSerialization",
        "type": "object",
        "required": [
          "param1",
          "param2",
          "path"
        ],
        "properties": {
          "param1": {
            "title": "uint64",
            "type": "integer",
            "format": "uint64",
            "minimum": 0.0
          },
          "param2": {
            "title": "Boolean",
            "type": "boolean"
          },
          "path": {
            "type": "string"
          }
        },
        "additionalProperties": false
      },
      "else": {
        "if": {
          "type": "object",
          "required": [
            "path"
          ],
          "properties": {
            "path": {
              "pattern": "^/home/vincentn/Projects/logic_based_learning_paths/rust\\-workspace/target/debug/liblblp_dummy_node_plugin_with_extension_field_1\\.so$"
            }
          }
        },
        "then": {
          "title": "PluginForSerialization",
          "type": "object",
          "required": [
            "param1",
            "param2",
            "path"
          ],
          "properties": {
            "param1": {
              "title": "uint64",
              "type": "integer",
              "format": "uint64",
              "minimum": 0.0
            },
            "param2": {
              "title": "Boolean",
              "type": "boolean"
            },
            "path": {
              "type": "string"
            }
          },
          "additionalProperties": false
        },
        "else": {
          "title": "PluginForSerialization",
          "type": "object",
          "required": [
            "path"
          ],
          "properties": {
            "path": {
              "type": "string"
            }
          },
          "additionalProperties": false
        }
      }
    }"###
            ),
        );
    }
}

impl ClusterProcessingPlugin for YamlSchemaGenerationPlugin {
    fn process_cluster(
        &self,
        cluster_path: &Path,
        cluster: &domain::Cluster,
    ) -> Result<HashSet<ArtifactMapping>, anyhow::Error> {
        let mut file = std::fs::File::open(cluster_path.join("cluster_schema.json"))?;
        self.process_cluster_with_writer(cluster, &mut file)
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn ClusterProcessingPlugin {
    let plugin = Box::new(YamlSchemaGenerationPlugin { path: "".into() });
    Box::into_raw(plugin)
}
