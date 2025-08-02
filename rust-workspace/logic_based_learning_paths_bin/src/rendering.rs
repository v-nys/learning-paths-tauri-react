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
pub fn svgify(graph: &Graph) -> anyhow::Result<String> {
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
    let graph_result = graphviz_rust::parse(&dot);
    let dot_syntax = graph_result.map_err(|e| anyhow::anyhow!(e))?;
    let svg_result = exec(
        dot_syntax,
        &mut PrinterContext::default(),
        vec![Format::Svg.into()],
    );
    svg_result.map_err(|e| anyhow::anyhow!(format!("{}", e)))
}
