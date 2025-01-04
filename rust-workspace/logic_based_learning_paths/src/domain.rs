use crate::plugins::NodeProcessingPlugin;

pub use crate::domain_without_loading::*;

/// An namespaced collection of `Node`s which may link to `Node`s in different namespaces.
///
/// A `Cluster` can represent a thematic clustering (nodes are related to the same topic such as a common technology).
/// It can also represent a practical clustering (nodes make up a single project).
/// A `Cluster` has a (non-nested) namespace prefix, which can be used to refer to nodes in the `Cluster`.
/// E.g. if a `Cluster's` namespace prefix is `"foo"` and the `Cluster` contains a `Node` whose ID is `bar`, this node can be referred to as `foo__bar`.
/// The namespace and node ID are always separated by `"__"`.
#[derive(Debug)]
pub struct Cluster {
    pub namespace_prefix: String,
    pub nodes: Vec<Node>,
    pub edges: Vec<TypedEdge>,
    pub roots: Vec<NodeID>,
    pub node_plugins: Vec<NodeProcessingPlugin>,
}
