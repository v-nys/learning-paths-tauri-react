use schemars::JsonSchema;
use serde::Deserialize;

use crate::domain;

/// Deserialization counterpart for the domain concept `Node`.
#[derive(Deserialize, Clone, Debug, JsonSchema)]
struct Node {
    /// An ID should be locally unique inside a `Cluster` and is used to refer to a node inside its `Cluster`.
    ///
    /// The ID also be used to refer to the node from outside its `Cluster`, if it is preceded by the `Cluster`'s namespace prefix.
    id: String,
    /// Human-readable title for this unit of knowledge.
    ///
    /// This is not required to be unique at any level.
    title: String,
}

impl Node {
    fn build(&self, namespace: &str) -> Result<domain::Node, String> {
        let parts = self.id.split("__").collect::<Vec<_>>();
        if parts.len() > 1 {
            Err(format!(
                "Declared node {} specifies explicit namespace",
                self.id
            ))
        } else {
            Ok(domain::Node {
                node_id: domain::NodeID {
                    namespace: namespace.to_owned(),
                    local_id: self.id.clone(),
                },
                title: self.title.clone(),
            })
        }
    }
}

#[derive(Deserialize, Clone, JsonSchema)]
struct Edge {
    // TODO: rename to UntypedEdge?
    start_id: String,
    end_id: String,
}

impl Edge {
    fn build(&self, namespace: &str, kind: domain::EdgeType) -> Result<domain::TypedEdge, String> {
        let mut start_components: Vec<_> = self.start_id.split("__").collect();
        let mut end_components: Vec<_> = self.end_id.split("__").collect();
        if start_components.len() > 2 {
            Err(format!(
                "{} is not a valid start for an edge (too many explicit namespaces)",
                self.start_id
            ))
        } else if end_components.len() > 2 {
            Err(format!(
                "{} is not a valid end for an edge (too many explicit namespaces)",
                self.end_id
            ))
        } else if start_components.len() == 2 && start_components[0] == namespace {
            Err(format!(
                "{} should not be explicitly namespaced in this context",
                self.start_id
            ))
        } else if end_components.len() == 2 && end_components[0] == namespace {
            Err(format!(
                "{} should not be explicitly namespaced in this context",
                self.end_id
            ))
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
#[derive(Deserialize, Clone, JsonSchema)]
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
}

impl ClusterForSerialization {
    pub fn build(self, folder_name: String) -> Result<domain::Cluster, String> {
        // this gives a vector of results
        let nodes: Vec<_> = self.nodes.iter().map(|n| n.build(&folder_name)).collect();
        // turn it into a result for a vector
        let nodes: Result<Vec<_>, _> = nodes.into_iter().collect();
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
            // FIXME: don't think error is spotted if root is namespaced via __
            roots: self
                .roots
                .unwrap_or_default()
                .into_iter()
                .map(|root_string| domain::NodeID {
                    namespace: folder_name.clone(),
                    local_id: root_string,
                })
                .collect(),
        })
    }
}
