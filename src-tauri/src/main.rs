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
    DoubleNode(String),                              // creating two nodes with same ID
    MissingInternalEndpoint(String, String, String), // referring to non-existent node
    NodeMultipleNamespace(String),                   // creating a node with explicit namespace
    EdgeMultipleNamespace(String, String, String),   // edge from / to internal node with
                                                     // explicit namespace
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
    check_redundant_edges: bool,
    check_cluster_boundaries: bool,
    check_missing_files: bool
) -> Vec<(
    &str,
    Result<Result<(String, Vec<String>), Vec<String>>, String>,
)> {
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
            // these should only be internal nodes
            for node in c.nodes {
                let maybe_namespaced_key = node.id.clone();
                if maybe_namespaced_key.contains("__") {
                    structural_errors
                        .push(StructuralError::NodeMultipleNamespace(maybe_namespaced_key));
                } else {
                    let definitely_namespaced_key =
                        format!("{}__{maybe_namespaced_key}", c.namespace_prefix);
                    if !map.contains_key(&definitely_namespaced_key) {
                        let idx = graph.add_node(node.title);
                        map.insert(definitely_namespaced_key, idx);
                    } else {
                        structural_errors
                            .push(StructuralError::DoubleNode(definitely_namespaced_key));
                    }
                }
            }

            for Edge { start_id, end_id } in &c.edges {
                let mut can_add = true;
                if start_id.starts_with(&format!("{}__", &c.namespace_prefix)) {
                    structural_errors.push(StructuralError::EdgeMultipleNamespace(
                        start_id.to_owned(),
                        end_id.to_owned(),
                        start_id.to_owned(),
                    ));
                    can_add = false;
                }
                if end_id.starts_with(&format!("{}__", &c.namespace_prefix)) {
                    structural_errors.push(StructuralError::EdgeMultipleNamespace(
                        start_id.to_owned(),
                        end_id.to_owned(),
                        start_id.to_owned(),
                    ));
                    can_add = false;
                }
                if can_add {
                    let mut start_id = start_id.to_owned();
                    let mut end_id = end_id.to_owned();
                    if start_id.contains("__") {
                        if !map.contains_key(&start_id) {
                            let idx = graph.add_node(start_id.clone());
                            map.insert(start_id.clone(), idx);
                        }
                    } else {
                        start_id = format!("{}__{start_id}", c.namespace_prefix);
                    }
                    if end_id.contains("__") {
                        if !map.contains_key(&end_id) {
                            let idx = graph.add_node(end_id.clone());
                            map.insert(end_id.clone(), idx);
                        }
                    } else {
                        end_id = format!("{}__{end_id}", c.namespace_prefix);
                    }
                    match (map.get(&start_id), map.get(&end_id)) {
                        (Some(idx1), Some(idx2)) => {
                            // don't need edge labels, so just using ""
                            graph.add_edge(*idx1, *idx2, "");
                        }
                        (Some(_), None) => {
                            structural_errors.push(StructuralError::MissingInternalEndpoint(
                                start_id.to_owned(),
                                end_id.to_owned(),
                                end_id.to_owned(),
                            ));
                        }
                        (None, Some(_)) => {
                            structural_errors.push(StructuralError::MissingInternalEndpoint(
                                start_id.to_owned(),
                                end_id.to_owned(),
                                start_id.to_owned(),
                            ));
                        }
                        (None, None) => {
                            structural_errors.push(StructuralError::MissingInternalEndpoint(
                                start_id.to_owned(),
                                end_id.to_owned(),
                                start_id.to_owned(),
                            ));
                            structural_errors.push(StructuralError::MissingInternalEndpoint(
                                start_id.to_owned(),
                                end_id.to_owned(),
                                end_id.to_owned(),
                            ));
                        }
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
                let mut remarks: Vec<String> = vec![];
                if check_redundant_edges {
                    remarks.push("Can't check for redundant edges yet.".to_owned());
                }
                if check_cluster_boundaries {
                    remarks.push("Can't check cluster boundaries yet.".to_owned());
                }
                if check_missing_files {
                    remarks.push("Can't check for missing files yet.".to_owned());
                }
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

    // to enable serialization and cloning
    let serializable = dots.map(|dot_with_remarks| {
        dot_with_remarks
            .map(|ok| ok.map_err(|ses| ses.into_iter().map(|se| format!("{:#?}", se)).collect()))
            .map_err(|re| format!("{:#?}", re))
    });
    eprintln!("Errors have been converted into strings.");


    let mut dots: Vec<_> = serializable.collect();
    let graph_result: Result<Vec<_>,_> = dots.clone().into_iter().collect();
    let mut paths: Vec<&str> = paths.collect();
    paths.push("complete graph");
    match graph_result {
        Ok(v) => {
            let graph_result: Result<Vec<_>,_> = v.into_iter().collect();
            match graph_result {
                Ok(v) => {
                    // TODO: (try to) build and add complete graph
                }
                Err(e) => {
                    dots.push(Ok(Err(e)));
                }
            }
        }
        Err(e) => {
            dots.push(Err(e));
        }
    }

    eprintln!("Dots have been generated and remarks have been added.");
    let svgs = dots.into_iter().map(|dot_result| {
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

    std::iter::zip(paths, svgs).collect()
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
