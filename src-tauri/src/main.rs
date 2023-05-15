// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::{collections::HashMap, path::Path};

use graphviz_rust::{cmd::Format, exec, printer::PrinterContext};
use petgraph::{dot::Dot, Directed, Graph};
use serde::{Deserialize};

/* Maybe more use of references would be more idiomatic here. */
#[derive(Deserialize)]
struct Node {
    id: String,
    title: String,
}

#[derive(Deserialize)]
struct Edge {
    start_id: String,
    end_id: String
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
enum SemanticError {
    StructuralErrors, // TODO: add detail? maybe split into separate error types ReadError and InterpretationError?
}

enum Dependency {
    Requirement(Directed),
    Motivation(Directed),
}

#[tauri::command]
fn read_contents(paths: &str) -> Vec<(&str, Result<Result<String, String>, String>)> {
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
            for node in c.nodes {
                let map_key = node.id.clone();
                if !map.contains_key(&map_key) {
                    let idx = graph.add_node(node.id);
                    map.insert(map_key, idx);
                } else {
                    // TODO: be more specific, add to vector first...
                    return Err(SemanticError::StructuralErrors);
                }
            }
            for edge in &c.edges {
                match (map.get(&edge.start_id), map.get(&edge.end_id)) {
                    (Some(idx1), Some(idx2)) => {
                        graph.add_edge(*idx1, *idx2, 1);
                    }
                    // TODO: be more specific, add to vector first
                    _ => return Err(SemanticError::StructuralErrors),
                }
            }
            Ok(graph)
        })
        .map(|g| g.map(|c| format!("{:?}", Dot::new(&c))))
    });
    eprintln!("Dots have been generated.");
    let svgs = dots.map(|dot_result| {
        dot_result.map(|dot| {
            dot.map(|ref src| {
                let g = graphviz_rust::parse(src)
                    .expect("Assuming petgraph generated valid dot syntax.");
                exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])
                    .expect("Assuming valid graph can be rendered into SVG.")
            })
        })
    });
    eprintln!("SVGs have been rendered.");
    let serializable = svgs.map(|svg| {
        svg.map(|ok| ok.map_err(|se| format!("{:#?}", se)))
            .map_err(|re| format!("{:#?}", re))
    });
    eprintln!("Errors have been converted into strings.");
    std::iter::zip(paths, serializable).collect()
}

#[tauri::command]
fn associate(paths: &str) -> Result<HashMap<&Path,Vec<&Path>>,&Path> {
    eprintln!("associate was invoked!");
    let paths = paths.split(";").map(|p| {Path::new(p)});
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
        .invoke_handler(tauri::generate_handler![read_contents,associate])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
