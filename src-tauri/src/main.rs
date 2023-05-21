// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use graphviz_rust::{cmd::Format, exec, printer::PrinterContext};
use petgraph::{dot::Config, dot::Dot, graph::NodeIndex, Directed, Graph};
use serde::Deserialize;
use std::{collections::HashMap, path::Path};

/* Maybe more use of references would be more idiomatic here. */
#[derive(Deserialize)]
struct Node {
    id: String,
    title: String,
}

#[derive(Deserialize)]
struct Edge {
    start_id: String,
    end_id: String,
}

#[derive(Deserialize)]
struct Cluster {
    namespace_prefix: String,
    nodes: Vec<Node>,
    edges: Vec<Edge>,
}

#[derive(Debug)]
enum ReadError {
    Deserialization(serde_yaml::Error),
    IO(std::io::Error),
}

#[derive(Debug)]
enum StructuralError {
    DoubleNodeError(String),
    MissingEndpointError(String, String, String),
}

enum Dependency {
    Requirement(Directed),
    Motivation(Directed),
}

fn get_node_attributes(graph: &Graph<String, &str>, node_ref: (NodeIndex, &String)) -> String {
    // label specified last is used, so this overrides the auto-generated one
    format!("label=\"{}\"", node_ref.1)
}

#[tauri::command]
fn read_contents(
    paths: &str,
) -> Vec<(
    &str,
    Result<Result<(String, Vec<String>), Vec<String>>, String>,
)> {
    /* currently working on turning main Ok type (SVG code) from String into (String,Vec<String>).
     * idea is to add remarks, e.g. regarding missing attachments, redundant edges,... */
    eprintln!("read_contents was invoked!");
    let paths = paths.split(";");
    let read_results = paths.clone().map(std::fs::read_to_string);
    let deserialized_graphs: Vec<Result<Cluster, ReadError>> = read_results
        .map(|r| match r {
            Ok(ref text) => serde_yaml::from_str(text).map_err(|e| ReadError::Deserialization(e)),
            Err(e) => Err(ReadError::IO(e)),
        })
        .collect();
    eprintln!("Graphs have been deserialized.");
    let dots = deserialized_graphs.into_iter().map(|r| {
        r.map(|c| {
            let mut map = std::collections::HashMap::new();
            let mut graph = Graph::new();
            let mut structural_errors: Vec<StructuralError> = vec![];
            for node in c.nodes {
                let map_key = node.id.clone();
                if !map.contains_key(&map_key) {
                    let idx = graph.add_node(node.title);
                    map.insert(map_key, idx);
                } else {
                    structural_errors.push(StructuralError::DoubleNodeError(map_key));
                }
            }
            for edge in &c.edges {
                match (map.get(&edge.start_id), map.get(&edge.end_id)) {
                    (Some(idx1), Some(idx2)) => {
                        // don't need edge labels, so just using ""
                        graph.add_edge(*idx1, *idx2, "");
                    }
                    (Some(_), None) => {
                        structural_errors.push(StructuralError::MissingEndpointError(
                            edge.start_id.to_owned(),
                            edge.end_id.to_owned(),
                            edge.end_id.to_owned(),
                        ));
                    }
                    (None, Some(_)) => {
                        structural_errors.push(StructuralError::MissingEndpointError(
                            edge.start_id.to_owned(),
                            edge.end_id.to_owned(),
                            edge.start_id.to_owned(),
                        ));
                    }
                    (None, None) => {
                        structural_errors.push(StructuralError::MissingEndpointError(
                            edge.start_id.to_owned(),
                            edge.end_id.to_owned(),
                            edge.start_id.to_owned(),
                        ));
                        structural_errors.push(StructuralError::MissingEndpointError(
                            edge.start_id.to_owned(),
                            edge.end_id.to_owned(),
                            edge.end_id.to_owned(),
                        ));
                    }
                }
            }
            if structural_errors.is_empty() {
                Ok(graph)
            } else {
                Err(structural_errors)
            }
        })
        .map(|g| {
            g.map(|cluster| {
                let remarks = vec![
                    "This needs work".to_owned(),
                    "This other thing needs work".to_owned(),
                    "Another placeholder".to_owned(),
                ];
                (
                    format!(
                        "{:?}",
                        Dot::with_attr_getters(
                            &cluster,
                            &[Config::EdgeNoLabel],
                            &|_g, _g_edge_ref| "".to_owned(),
                            &get_node_attributes
                        )
                    ),
                    remarks,
                )
            })
        })
    });
    eprintln!("Dots have been generated and remarks have been added.");
    let svgs = dots.map(|dot_result| {
        dot_result.map(|dot_with_remarks| {
            dot_with_remarks.map(|(ref dot_src, remarks)| {
                eprintln!("Dot syntax is:\n{}", dot_src);
                let g = graphviz_rust::parse(dot_src)
                    .expect("Assuming petgraph generated valid dot syntax.");
                (
                    exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])
                        .expect("Assuming valid graph can be rendered into SVG."),
                    remarks,
                )
            })
        })
    });
    eprintln!("SVGs have been rendered.");
    let serializable = svgs.map(|svg_with_remarks| {
        svg_with_remarks
            .map(|ok| ok.map_err(|ses| ses.into_iter().map(|se| format!("{:#?}", se)).collect()))
            .map_err(|re| format!("{:#?}", re))
    });
    eprintln!("Errors have been converted into strings.");
    std::iter::zip(paths, serializable).collect()
}

#[tauri::command]
fn associate(paths: &str) -> Result<HashMap<&Path, Vec<&Path>>, &Path> {
    let paths = paths.split(";").map(|p| Path::new(p));
    let mut map = HashMap::new();
    for path in paths {
        let parent = path.parent();
        match parent {
            None => return Err(path),
            Some(parent) => {
                let value = map.entry(parent).or_insert(vec![]);
                value.push(path);
            }
        }
    }
    Ok(map)
}

fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_fs_watch::init())
        .invoke_handler(tauri::generate_handler![read_contents, associate])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
