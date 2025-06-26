use crate::plugins::{ClusterProcessingPlugin, LBLPPlugin, NodeProcessingPlugin, PreArchivePlugin};

pub use logic_based_learning_paths::domain_without_loading::*;

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
    // don't think I need pre_node_cluster_plugins here
    // those have already run before the cluster is constructed
    pub post_node_node_plugins: Vec<NodeProcessingPlugin>,
    pub post_node_cluster_plugins: Vec<ClusterProcessingPlugin>,
    pub post_merge_node_plugins: Vec<NodeProcessingPlugin>,
    pub post_merge_cluster_plugins: Vec<ClusterProcessingPlugin>,
    pub pre_archive_plugins: Option<Vec<PreArchivePlugin>>,
}

#[derive(Debug)]
pub struct UnpopulatedCluster {
    pub namespace_prefix: String,
    pub pre_node_cluster_plugins: Vec<ClusterProcessingPlugin>,
    pub post_node_node_plugins: Vec<NodeProcessingPlugin>,
    pub post_node_cluster_plugins: Vec<ClusterProcessingPlugin>,
    pub post_merge_node_plugins: Vec<NodeProcessingPlugin>,
    pub post_merge_cluster_plugins: Vec<ClusterProcessingPlugin>,
    pub pre_archive_plugins: Option<Vec<PreArchivePlugin>>,
}

impl UnpopulatedCluster {
    pub fn node_plugins_mut(&mut self) -> impl Iterator<Item = &mut NodeProcessingPlugin> {
        let ponnp = self.post_node_node_plugins.iter_mut();
        let pmnp = self.post_merge_node_plugins.iter_mut();
        ponnp.chain(pmnp)
    }

    pub fn all_plugins_mut(&mut self) -> impl Iterator<Item = &mut dyn LBLPPlugin> {
        // node plugins
        let prnnp = self
            .pre_node_cluster_plugins
            .iter_mut()
            .map(|p| p.as_lblp_plugin_mut());
        let ponnp = self
            .post_node_node_plugins
            .iter_mut()
            .map(|p| p.as_lblp_plugin_mut());
        let pmnp = self
            .post_merge_node_plugins
            .iter_mut()
            .map(|p| p.as_lblp_plugin_mut());
        // cluster plugins
        let poncp = self
            .post_node_cluster_plugins
            .iter_mut()
            .map(|p| p.as_lblp_plugin_mut());
        let pomcp = self
            .post_merge_cluster_plugins
            .iter_mut()
            .map(|p| p.as_lblp_plugin_mut());
        let pap = self
            .pre_archive_plugins
            .iter_mut()
            .flatten()
            .map(|p| p.as_lblp_plugin_mut());
        prnnp
            .chain(ponnp)
            .chain(pmnp)
            .chain(poncp)
            .chain(pomcp)
            .chain(pap)
    }
}
