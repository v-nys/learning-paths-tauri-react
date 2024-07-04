use lazy_regex::regex;
use learning_paths_tauri_react::plugins::{
    load_cluster_processing_plugins, load_node_processing_plugins,
};
use schemars::JsonSchema;
use serde::de::{self, MapAccess, Visitor};
use serde::Deserialize;
use serde::Deserializer;
use serde_yaml::Value;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::rc::Rc;
use std::str::FromStr;

use crate::domain;

/// Deserialization counterpart for the domain concept `Node`.
#[derive(Clone, Debug)]
struct Node {
    /// An ID should be locally unique inside a `Cluster` and is used to refer to a node inside its `Cluster`.
    ///
    /// The ID also be used to refer to the node from outside its `Cluster`, if it is preceded by the `Cluster`'s namespace prefix.
    id: String,
    /// Human-readable title for this unit of knowledge.
    ///
    /// This is not required to be unique at any level.
    title: String,
    extension_fields: HashMap<String, Value>,
}

impl<'de> Deserialize<'de> for Node {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct NodeVisitor;

        impl<'de> Visitor<'de> for NodeVisitor {
            type Value = Node;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Node")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Node, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut id = None;
                let mut title = None;
                let mut extension_fields = HashMap::new();

                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "id" => {
                            if id.is_some() {
                                return Err(de::Error::duplicate_field("id"));
                            }
                            id = Some(map.next_value()?);
                        }
                        "title" => {
                            if title.is_some() {
                                return Err(de::Error::duplicate_field("title"));
                            }
                            title = Some(map.next_value()?);
                        }
                        _ => {
                            if extension_fields.contains_key(&key) {
                                // can't use provided duplicate_field function here
                                // that expects a &'static str and &key does not have that lifetime
                                return Err(de::Error::custom(format!(
                                    "duplicate field: {}",
                                    &key
                                )));
                            }
                            extension_fields.insert(key, map.next_value()?);
                        }
                    }
                }

                let id = id.ok_or_else(|| de::Error::missing_field("id"))?;
                let title = title.ok_or_else(|| de::Error::missing_field("title"))?;
                Ok(Node {
                    id,
                    title,
                    extension_fields,
                })
            }
        }

        deserializer.deserialize_map(NodeVisitor)
    }
}

impl Node {
    fn build(&self, namespace: &str) -> Result<domain::Node, anyhow::Error> {
        let effective_id = format!("{}__{}", namespace, self.id);
        let id = domain::NodeID::from_two_part_string(&effective_id)?;
        Ok(domain::Node {
            node_id: id,
            title: self.title.clone(),
            extension_fields: self.extension_fields.clone(),
        })
    }
}

#[derive(Deserialize, Clone, JsonSchema)]
struct Edge {
    // TODO: rename to UntypedEdge?
    start_id: String,
    end_id: String,
}

impl Edge {
    fn build(
        &self,
        namespace: &str,
        kind: domain::EdgeType,
    ) -> Result<domain::TypedEdge, anyhow::Error> {
        let identifier_regex = regex!("[a-z][a-z_]*");
        let mut start_components: Vec<_> = self.start_id.split("__").collect();
        let mut end_components: Vec<_> = self.end_id.split("__").collect();
        let invalid_part = start_components
            .iter()
            .chain(end_components.iter())
            .find(|p| !identifier_regex.is_match(p));
        if let Some(part) = invalid_part {
            Err(domain::StructuralError::InvalidIdentifierError(part.to_string()).into())
        } else if start_components.len() > 2 {
            Err(domain::StructuralError::EdgeMultipleNamespace(
                self.start_id.to_string(),
                self.end_id.to_string(),
                self.start_id.to_string(),
            )
            .into())
        } else if end_components.len() > 2 {
            Err(domain::StructuralError::EdgeMultipleNamespace(
                self.start_id.to_string(),
                self.end_id.to_string(),
                self.end_id.to_string(),
            )
            .into())
        } else if start_components.len() == 2 && start_components[0] == namespace {
            Err(domain::StructuralError::EdgeMultipleNamespace(
                self.start_id.to_string(),
                self.end_id.to_string(),
                self.start_id.to_string(),
            )
            .into())
        } else if end_components.len() == 2 && end_components[0] == namespace {
            Err(domain::StructuralError::EdgeMultipleNamespace(
                self.start_id.to_string(),
                self.end_id.to_string(),
                self.end_id.to_string(),
            )
            .into())
        } else {
            if start_components.len() == 1 {
                start_components.insert(0, namespace);
            }
            if end_components.len() == 1 {
                end_components.insert(0, namespace);
            }
            Ok(domain::TypedEdge {
                start_id: domain::NodeID {
                    namespace: start_components[0].to_owned(),
                    local_id: start_components[1].to_owned(),
                },
                end_id: domain::NodeID {
                    namespace: end_components[0].to_owned(),
                    local_id: end_components[1].to_owned(),
                },
                kind,
            })
        }
    }
}

/// A representation of a `Cluster` which is more suitable for (de)serialization.
///
/// It does not require a namespace prefix, as that is assumed to match the name of the file to
/// which it is serialized.
/// It uses disjoint, optional sets of edges because that saves a lot of repetition when writing in
/// a data format.
#[derive(Deserialize, Clone)]
#[serde(deny_unknown_fields)]
pub struct ClusterForSerialization {
    /// Units of information inside this `Cluster`.
    nodes: Vec<self::Node>,
    /// Strict dependencies. A non-root `Node` can only be accessed if all of its dependencies of this type have been marked complete, along with one interchangeable dependency of this `Node` or of a `Node` which is strictly dependent on this `Node`.
    all_type_edges: Option<Vec<Edge>>,
    /// Interchangeable dependencies. A non-root `Node` can only be accessed if one dependency of this type has been marked complete for this node or for a `Node` which is strictly dependent on this `Node`. Furthermore, all strict dependencies must still be marked complete.
    any_type_edges: Option<Vec<Edge>>,
    /// IDs of `Node`s with no dependencies whatsoever, i.e. the only `Node`s which can be accessed unconditionally.
    roots: Option<Vec<String>>,
    pre_cluster_plugin_paths: Option<Vec<PluginForSerialization>>,
    node_plugin_paths: Option<Vec<PluginForSerialization>>,
    pre_zip_plugin_paths: Option<Vec<PluginForSerialization>>,
}

#[derive(Clone)]
struct PluginForSerialization {
    path: String,
    parameters: HashMap<String, Value>,
}

impl<'de> Deserialize<'de> for PluginForSerialization {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct PluginVisitor;

        impl<'de> Visitor<'de> for PluginVisitor {
            type Value = PluginForSerialization;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct PluginForSerialization")
            }

            fn visit_map<V>(self, mut map: V) -> Result<PluginForSerialization, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut path = None;
                let mut parameters = HashMap::new();

                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "path" => {
                            if path.is_some() {
                                return Err(de::Error::duplicate_field("path"));
                            }
                            path = Some(map.next_value()?);
                        }
                        _ => {
                            if parameters.contains_key(&key) {
                                return Err(de::Error::custom(format!(
                                    "duplicate field: {}",
                                    &key
                                )));
                            }
                            parameters.insert(key, map.next_value()?);
                        }
                    }
                }
                let path = path.ok_or_else(|| de::Error::missing_field("path"))?;
                Ok(PluginForSerialization { path, parameters })
            }
        }
        deserializer.deserialize_map(PluginVisitor)
    }
}

impl ClusterForSerialization {
    pub fn build(self, folder_name: String) -> Result<domain::Cluster, anyhow::Error> {
        // this gives a vector of results
        let nodes: Vec<_> = self.nodes.iter().map(|n| n.build(&folder_name)).collect();
        // turn it into a result for a vector
        let nodes: Result<Vec<_>, _> = nodes.into_iter().collect();
        println!("about to create new ClusterForSerialization");
        let node_plugins = load_node_processing_plugins(
            self.node_plugin_paths
                .unwrap_or_default()
                .into_iter()
                .map(|pfs| domain::UnloadedPlugin {
                    path: pfs.path,
                    parameters: pfs.parameters,
                })
                .collect(),
        );
        println!("Outcome 1: {:#?}", node_plugins);
        let node_plugins = node_plugins?;
        println!("Outcome 2: {:#?}", node_plugins);
        let node_plugins = Rc::new(node_plugins);
        println!("Done loading node plugins");
        Ok(domain::Cluster {
            namespace_prefix: folder_name.clone(),
            nodes: nodes?,
            edges: self
                .all_type_edges
                .unwrap_or_default()
                .into_iter()
                .map(|e| e.build(&folder_name, domain::EdgeType::All))
                .chain(
                    self.any_type_edges
                        .unwrap_or_default()
                        .into_iter()
                        .map(|e| e.build(&folder_name, domain::EdgeType::AtLeastOne)),
                )
                .collect::<Result<Vec<_>, _>>()?,
            // FIXME: don't think error is spotted if root is namespaced via __ or has invalid
            // symbols in it
            roots: self
                .roots
                .unwrap_or_default()
                .into_iter()
                .map(|root_string| domain::NodeID {
                    namespace: folder_name.clone(),
                    local_id: root_string,
                })
                .collect(),
            // pre_cluster_... is een Vec<PluginForSerialization>
            pre_cluster_plugins: Rc::new(load_cluster_processing_plugins(
                self.pre_cluster_plugin_paths
                    .unwrap_or_default()
                    .into_iter()
                    .map(|pfs| domain::UnloadedPlugin {
                        path: pfs.path,
                        parameters: pfs.parameters,
                    })
                    .collect(),
            )?),
            node_plugins,
            pre_zip_plugin_paths: self
                .pre_zip_plugin_paths
                .unwrap_or_default()
                .into_iter()
                .map(|pfs| domain::UnloadedPlugin {
                    path: pfs.path,
                    parameters: pfs.parameters,
                })
                .collect(),
        })
    }
}
