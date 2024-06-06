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
    fn build(&self) -> domain::Node {
        domain::Node {
            id: self.id.clone(),
            title: self.title.clone(),
        }
    }
}

#[derive(Deserialize, Clone, JsonSchema)]
struct Edge {
    // TODO: rename to UntypedEdge?
    start_id: String,
    end_id: String,
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
    pub fn build(self, folder_name: String) -> domain::Cluster {
        domain::Cluster {
            namespace_prefix: folder_name,
            // TODO: need to turn deserialization nodes into domain nodes
            nodes: self.nodes.iter().map(|n| { n.build() }).collect(),
            edges: self
                .all_type_edges
                .unwrap_or_default()
                .into_iter()
                .map(|e| domain::TypedEdge {
                    start_id: e.start_id,
                    end_id: e.end_id,
                    kind: domain::EdgeType::All,
                })
                .chain(
                    self.any_type_edges
                        .unwrap_or_default()
                        .into_iter()
                        .map(|e| domain::TypedEdge {
                            start_id: e.start_id,
                            end_id: e.end_id,
                            kind: domain::EdgeType::AtLeastOne,
                        }),
                )
                .collect::<Vec<_>>(),
            roots: self.roots.unwrap_or_default(),
        }
    }
}
