use crate::domain::{EdgeType, Graph, NodeData};
use graphviz_rust::{cmd::Format, exec, printer::PrinterContext};
use petgraph::dot::{Config, Dot};
use petgraph::graph::NodeIndex;

/// Compute the Graphviz rendering attributes for a specific node in a graph.
fn node_dot_attributes(_: &Graph, node_ref: (NodeIndex, &NodeData)) -> String {
    // label specified last is used, so this overrides the auto-generated one
    format!(
        "label=\"{}\" tooltip=\"{}\" shape=\"box\" fontname=\"Courier New\"",
        node_ref.1 .1, node_ref.1 .0
    )
}

/// Render a Graph to SVG source code.
pub fn svgify(graph: &Graph) -> String {
    let dot = dbg!(format!(
        "digraph {{
            {:?}
        }}",
        Dot::with_attr_getters(
            graph,
            &[Config::GraphContentOnly, Config::EdgeNoLabel],
            &|_g, g_edge_ref| match g_edge_ref.weight() {
                EdgeType::All => {
                    "style=\"solid\"".to_owned()
                }
                EdgeType::AtLeastOne => {
                    "style=\"dashed\"".to_owned()
                }
            },
            &node_dot_attributes
        )
    ));
    let g = graphviz_rust::parse(&dot).expect("Assuming petgraph generated valid dot syntax.");
    exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])
        .expect("Assuming valid graph can be rendered into SVG.")
}
