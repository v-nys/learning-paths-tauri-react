use serde::Serialize;
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum EdgeType {
    All,
    AtLeastOne,
}

#[derive(Clone, Debug)]
pub struct TypedEdge {
    pub start_id: NodeID,
    pub end_id: NodeID,
    pub kind: EdgeType,
}

#[derive(Debug, Serialize)]
pub struct UnlockingCondition {
    pub all_of: HashSet<NodeID>,
    pub one_of: HashSet<NodeID>,
}

/// An namespaced collection of `Node`s which may link to `Node`s in different namespaces.
///
/// A `Cluster` can represent a thematic clustering (nodes are related to the same topic such as a common technology).
/// It can also represent a practical clustering (nodes make up a single project).
/// A `Cluster` has a (non-nested) namespace prefix, which can be used to refer to nodes in the `Cluster`.
/// E.g. if a `Cluster's` namespace prefix is `"foo"` and the `Cluster` contains a `Node` whose ID is `bar`, this node can be referred to as `foo__bar`.
/// The namespace and node ID are always separated by `"__"`.
#[derive(Clone, Debug)]
pub struct Cluster {
    pub namespace_prefix: String,
    pub nodes: Vec<Node>,
    pub edges: Vec<TypedEdge>,
    pub roots: Vec<NodeID>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct NodeID {
    /// Each node is namespaced according to its `Cluster`.
    pub namespace: String,
    /// An ID should be locally unique inside a `Cluster` and is used to refer to a node inside its `Cluster`.
    ///
    /// The ID also be used to refer to the node from outside its `Cluster`, if it is preceded by the `Cluster`'s namespace prefix.
    pub local_id: String,
}

impl std::fmt::Display for NodeID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}__{}", self.namespace, self.local_id)
    }
}

/// A single unit of learning material.
///
/// A `Node` represents knowledge that can be processed as one whole.
/// It does not need to be entirely standalone, as it can have dependencies in the form of `Edge` values.
#[derive(Clone, Debug)]
pub struct Node {
    pub node_id: NodeID,
    /// Human-readable title for this unit of knowledge.
    ///
    /// This is not required to be unique at any level.
    pub title: String,
}
