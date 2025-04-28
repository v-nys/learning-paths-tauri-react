use crate::domain_without_loading::Graph;
use petgraph::{
    algo::{
        toposort,
        tred::{dag_to_toposorted_adjacency_list, dag_transitive_reduction_closure},
    },
    graph::NodeIndex,
    visit::{IntoNeighbors, IntoNodeReferences},
};
use std::collections::HashSet;
use std::ops::Index;

// TODO: document
pub fn purge_nodes_not_leading_to_project(
    supercluster: &Graph,
    main_project_namespace: &str,
) -> Result<Graph, String> {
    // using index operators quite heavily in this code
    // if the code is properly tested, there shouldn't be any panics
    // because the mappings are produced here
    // so if the examples work, the mappings can always be reversed
    let supercluster_toposort_order =
        toposort(supercluster, None).map_err(|_| "Supercluster contains a cycle.".to_string())?;
    let (supercluster_toposorted_graph, supercluster_revmap) =
        dag_to_toposorted_adjacency_list(supercluster, &supercluster_toposort_order);
    let (_, supercluster_tc) =
        dag_transitive_reduction_closure::<(), NodeIndex>(&supercluster_toposorted_graph);
    let discarded_nodes: HashSet<NodeIndex> = supercluster_toposorted_graph
        .node_references()
        .filter(|mapped_node_index| {
            let matching_index_from_supercluster = supercluster_revmap[mapped_node_index.index()];
            let original_node_weight = supercluster.index(matching_index_from_supercluster);
            if original_node_weight.0.namespace == main_project_namespace {
                false
            } else {
                supercluster_tc
                    .neighbors(*mapped_node_index)
                    .map(|ix: NodeIndex| {
                        let toposorted_index = supercluster_revmap[ix.index()];
                        let original_index = supercluster_toposort_order[toposorted_index.index()];
                        let original = supercluster.index(original_index);
                        (original.0.clone(), original_index)
                    })
                    .all(|(node_id, _node_index)| node_id.namespace != main_project_namespace)
            }
        })
        .collect();
    let discarded_nodes = discarded_nodes
        .into_iter()
        .map(|toposorted_index| supercluster_toposort_order[toposorted_index.index()]);
    let mut cleaned = supercluster.clone();
    discarded_nodes.for_each(|discarded| {
        cleaned.remove_node(discarded);
    });
    Ok(cleaned)
}

#[cfg(test)]
mod tests {

    use crate::domain_without_loading::{EdgeType, Graph, NodeID};

    use super::purge_nodes_not_leading_to_project;

    #[test]
    fn cyclical_graph_produces_error_1() {
        let mut graph = Graph::new();
        let index1 = graph.add_node((
            NodeID {
                namespace: "mainproject".into(),
                local_id: "node1".into(),
            },
            "Node1".into(),
        ));
        let index2 = graph.add_node((
            NodeID {
                namespace: "mainproject".into(),
                local_id: "node2".into(),
            },
            "Node2".into(),
        ));
        graph.add_edge(index1, index2, EdgeType::All);
        graph.add_edge(index2, index1, EdgeType::All);
        let purge_result = purge_nodes_not_leading_to_project(&graph, "mainproject");
        assert!(
            purge_result.is_err(),
            "Graph with cycle should produce an error."
        );
    }

    #[test]
    fn disconnected_node_is_purged_1() {
        let mut graph = Graph::new();
        graph.add_node((
            NodeID {
                namespace: "otherproject".into(),
                local_id: "node1".into(),
            },
            "Node1".into(),
        ));
        graph.add_node((
            NodeID {
                namespace: "mainproject".into(),
                local_id: "node2".into(),
            },
            "Node2".into(),
        ));
        let purge_result = purge_nodes_not_leading_to_project(&graph, "mainproject");
        assert!(purge_result.is_ok());
        let purged_graph = purge_result.unwrap();
        assert!(purged_graph.node_count() == 1);
        let first_remaining_node = purged_graph.node_weights().next();
        assert!(first_remaining_node.is_some());
        let (_, title) = first_remaining_node.unwrap();
        assert!(
            title == "Node2",
            "Was expecting Node2 to remain, got {}",
            title
        );
    }

    #[test]
    fn disconnected_subgraph_is_purged_1() {
        let mut graph = Graph::new();
        let indices: Vec<_> = (1..=6)
            .map(|number| {
                graph.add_node((
                    NodeID {
                        namespace: (if number <= 3 {
                            "otherproject"
                        } else {
                            "mainproject"
                        })
                        .into(),
                        local_id: format!("node{}", number),
                    },
                    format!("Node{}", number),
                ))
            })
            .collect();
        graph.add_edge(indices[0], indices[1], EdgeType::All);
        graph.add_edge(indices[0], indices[2], EdgeType::All);

        graph.add_edge(indices[3], indices[4], EdgeType::All);
        graph.add_edge(indices[3], indices[5], EdgeType::All);

        let purge_result = purge_nodes_not_leading_to_project(&graph, "mainproject");
        assert!(purge_result.is_ok());
        let purged_graph = purge_result.unwrap();
        purged_graph.node_weights().for_each(|(node_id, title)| {
            assert!(
                node_id.namespace == "mainproject",
                "no otherproject nodes should lead to mainproject but {} was not filtered out",
                title
            );
        });
        assert!(
            purged_graph.node_count() == 3,
            "expecting graph count of 3, got {}",
            purged_graph.node_count()
        );
    }

    #[test]
    fn dead_ends_are_purged() {
        let mut graph = Graph::new();
        let indices: Vec<_> = (1..=9)
            .map(|number| {
                graph.add_node((
                    NodeID {
                        namespace: (if number <= 4 {
                            "otherproject"
                        } else {
                            "mainproject"
                        })
                        .into(),
                        local_id: format!("node{}", number),
                    },
                    format!("Node{}", number),
                ))
            })
            .collect();
        // all within otherproject
        graph.add_edge(indices[0], indices[1], EdgeType::All);
        graph.add_edge(indices[0], indices[2], EdgeType::All);
        graph.add_edge(indices[2], indices[3], EdgeType::All);
        // crossing boundary
        graph.add_edge(indices[3], indices[4], EdgeType::All);
        let purge_result = purge_nodes_not_leading_to_project(&graph, "mainproject");
        assert!(purge_result.is_ok());
        let purged_graph = purge_result.unwrap();
        purged_graph.node_weights().for_each(|(_, title)| {
            assert!(
                title != "Node2",
                "Node2 should have been filtered out because it has no forward path to mainproject"
            );
        });
        assert!(
            purged_graph.node_count() == 8,
            "expecting graph count of 8, but retained {}",
            (purged_graph
                .node_weights()
                .map(|(_, title)| { title.clone() }))
            .collect::<Vec<String>>()
            .join(", ")
        );
    }
}
