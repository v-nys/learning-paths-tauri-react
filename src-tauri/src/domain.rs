use super::plugins::{ClusterProcessingPluginContainer, NodeProcessingPluginContainer};
use lazy_regex::regex;
use serde::Serialize;
use serde_yaml::Value;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::rc::Rc;

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
    pub pre_cluster_plugins: Rc<VecDeque<ClusterProcessingPluginContainer>>, // Rc makes it possible to derive Clone
    pub node_plugins: Rc<VecDeque<NodeProcessingPluginContainer>>,
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

impl NodeID {
    pub fn from_two_part_string(string: &str) -> Result<NodeID, StructuralError> {
        let identifier_regex = regex!("[a-z][a-z_]*");
        let parts = string.split("__").collect::<Vec<_>>();
        let invalid_part = parts.iter().find(|p| !identifier_regex.is_match(p));
        if let Some(part) = invalid_part {
            Err(StructuralError::InvalidIdentifierError(part.to_string()).into())
        } else if parts.len() == 1 {
            Err(StructuralError::NodeMissingNamespace(string.to_string()).into())
        } else if parts.len() > 2 {
            Err(StructuralError::NodeMultipleNamespace(string.to_string()).into())
        } else {
            Ok(NodeID {
                namespace: parts[0].to_owned(),
                local_id: parts[1].to_owned(),
            })
        }
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
    pub extension_fields: HashMap<String, Value>,
}

/// An error related to the internal structure of a (syntactically valid, semantically invalid) `Cluster`.
#[derive(Debug)]
pub enum StructuralError {
    DoubleNode(NodeID),                              // creating two nodes with same ID
    MissingInternalEndpoint(NodeID, NodeID, NodeID), // referring to non-existent node
    NodeMissingNamespace(String),
    NodeMultipleNamespace(String),
    EdgeMultipleNamespace(String, String, String), // edge from / to internal node with ...
    ClusterBoundary(String, NodeID),               // cluster, reference
    InvalidComponentGraph,
    Cycle(NodeID),
    DependentRootNode(NodeID, NodeID),
    UndeclaredRoot(NodeID),
    IncomingAnyEdge(NodeID, NodeID),
    OutgoingAllEdge(NodeID, NodeID),
    InvalidIdentifierError(String),
}

impl fmt::Display for StructuralError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DoubleNode(id) => write!(f, "Node defined multiple times: {id}"),
            Self::MissingInternalEndpoint(start_id, end_id, missing_id) => write!(f, "Node {missing_id} mentioned in edge {start_id} → {end_id} does not exist"),
            Self::NodeMissingNamespace(id) => write!(f, "Node lacks a namespace: {id}"),
            Self::NodeMultipleNamespace(id) => write!(f, "Node has multiple namespaces: {id}"),
            Self::EdgeMultipleNamespace(start_id, end_id, namespaced_id) => write!(f, "Node {namespaced_id} mentioned in edge {start_id} → {end_id} is incorrectly namespaced. There should only be one namespace and it should only be explicit if it is not that of the defining cluster."),
            Self::ClusterBoundary(cluster,reference) => write!(f, "Cluster {} refers to non-existent external node {}", cluster, reference),
            Self::InvalidComponentGraph => write!(f, "At least one component graph is invalid"),
            Self::Cycle(id) => write!(f, "Node {} is involved in a cycle", id),
            Self::DependentRootNode(id, start_id) => write!(f, "Node {} is declared as a root and has at least one incoming edge (from {}). Roots should not have incoming edges.", id, start_id),
            Self::UndeclaredRoot(id) => write!(f, "Root {} is not declared as a node in the cluster.", id),
            Self::IncomingAnyEdge(start_id,end_id) => write!(f, "\"At least one\" type edge from {} to {}. These edges can only connect to other clusters in the \"out\" direction.", start_id, end_id),
            Self::OutgoingAllEdge(start_id,end_id) => write!(f, "\"All\" type edge from {} to {}. These edges can only connect to other clusters in the \"in\" direction.", start_id, end_id),
            Self::InvalidIdentifierError(identifier) => write!(f, "Invalid identifier {}.", identifier)
        }
    }
}

impl std::error::Error for StructuralError {}

/// The data associated with a Petgraph node.
pub type NodeData = (NodeID, String);

/// The data ssociated with a Petgraph edge.
pub type EdgeData = EdgeType;

/// The specific type of Petgraph graph for this application.
pub type Graph = petgraph::Graph<NodeData, EdgeData>;
