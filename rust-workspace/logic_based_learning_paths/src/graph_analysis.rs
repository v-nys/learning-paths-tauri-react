use crate::domain_without_loading::{EdgeData, EdgeType, Graph, RootedSupercluster};
use petgraph::{
    adj::List,
    algo::{
        toposort,
        tred::{dag_to_toposorted_adjacency_list, dag_transitive_reduction_closure},
    },
    graph::NodeIndex,
    visit::{EdgeRef, IntoNodeReferences},
};
use std::collections::HashMap;

pub fn subgraph_with_edges(parent: &Graph, predicate: impl Fn(&EdgeData) -> bool) -> Graph {
    let mut subgraph = Graph::new();
    let node_map = parent
        .node_references()
        .map(|(index_in_parent, node_data)| (index_in_parent, subgraph.add_node(node_data.clone())))
        .collect::<HashMap<_, _>>();

    parent
        .edge_references()
        .filter(|edge| predicate(edge.weight()))
        .for_each(|edge| {
            let new_source = node_map[&edge.source()];
            let new_target = node_map[&edge.target()];
            subgraph.add_edge(new_source, new_target, edge.weight().clone());
        });

    subgraph
}

// factored out because it is needed both for checking learning path and for building zip
pub fn dependency_helpers(
    supercluster_with_roots: &RootedSupercluster,
) -> (
    (Graph, List<(), NodeIndex>, Vec<NodeIndex>, Vec<NodeIndex>),
    (Graph, List<(), NodeIndex>, Vec<NodeIndex>, Vec<NodeIndex>),
    Graph,
) {
    let supercluster = &supercluster_with_roots.graph;
    let is_any_type = |edge: &EdgeData| edge == &EdgeType::AtLeastOne;
    let is_all_type = |edge: &EdgeData| edge == &EdgeType::All;

    let motivations_graph = subgraph_with_edges(&supercluster, is_any_type);
    let dependency_to_dependent_graph = subgraph_with_edges(&supercluster, is_all_type);
    let mut dependent_to_dependency_graph = dependency_to_dependent_graph.clone();
    dependent_to_dependency_graph.reverse();
    let dependent_to_dependency_toposort_order = toposort(&dependent_to_dependency_graph, None)
        .expect(
            "This function should only be called for graphs which have already been cycle-checked.",
        );
    let dependency_to_dependent_toposort_order = toposort(&dependency_to_dependent_graph, None)
        .expect(
            "This function should only be called for graphs which have already been cycle-checked.",
        );
    let (dependent_to_dependency_res, dependent_to_dependency_revmap) =
        dag_to_toposorted_adjacency_list(
            &dependent_to_dependency_graph,
            &dependent_to_dependency_toposort_order,
        );
    let (_, dependent_to_dependency_tc) =
        dag_transitive_reduction_closure(&dependent_to_dependency_res);

    let (dependency_to_dependent_res, dependency_to_dependent_revmap) =
        dag_to_toposorted_adjacency_list(
            &dependency_to_dependent_graph,
            &dependency_to_dependent_toposort_order,
        );
    let (_, dependency_to_dependent_tc) =
        dag_transitive_reduction_closure(&dependency_to_dependent_res);

    return (
        (
            dependent_to_dependency_graph,
            dependent_to_dependency_tc,
            dependent_to_dependency_revmap,
            dependent_to_dependency_toposort_order,
        ),
        (
            dependency_to_dependent_graph,
            dependency_to_dependent_tc,
            dependency_to_dependent_revmap,
            dependency_to_dependent_toposort_order,
        ),
        motivations_graph,
    );
}
