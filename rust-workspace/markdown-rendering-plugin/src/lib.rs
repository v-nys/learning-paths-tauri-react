#![allow(improper_ctypes_definitions)]
#![allow(deprecated)]

extern crate learning_paths_tauri_react;

use base64::encode;
use comrak::{markdown_to_html, ComrakOptions};
use learning_paths_tauri_react::domain;
use learning_paths_tauri_react::plugins::{ArtifactMapping, ClusterProcessingPlugin, Plugin};
use regex;
use schemars::JsonSchema;
use serde_json;
use serde_yaml::Value;
use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::time::SystemTime;
use std::{
    cmp::Ordering,
    fs,
    path::{Path, PathBuf},
};
use walkdir::WalkDir;

pub struct MarkdownRenderingPlugin {
    path: String
}

#[derive(JsonSchema)]
pub struct PluginParameters {}

fn find_md_files(dir: &Path) -> Vec<PathBuf> {
    let mut md_files = Vec::new();

    for entry in WalkDir::new(dir).into_iter().filter_map(Result::ok) {
        let path = entry.path();
        if path.is_file() {
            if let Some(extension) = path.extension() {
                if extension == "md" {
                    md_files.push(path.to_path_buf());
                }
            }
        }
    }

    md_files
}

fn markdown_to_html_with_inlined_images(markdown: &str) -> String {
    let options = ComrakOptions::default();
    let original_html = markdown_to_html(markdown, &options);
    let mut substituted_html = original_html.clone();

    // Find all image tags and inline the images
    let re = regex::Regex::new(r#"!\[.*?\]\((.*?)\)"#).unwrap();
    for cap in re.captures_iter(&original_html) {
        let img_path = &cap[1];
        if let Ok(inlined_img) = inline_image(img_path) {
            substituted_html = substituted_html.replace(&cap[0], &inlined_img);
        }
    }

    substituted_html
}

fn inline_image(path: &str) -> Result<String, std::io::Error> {
    let path = Path::new(path);
    let mut file = fs::File::open(path)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;
    let base64_img = encode(&buf);
    let ext = path
        .extension()
        .and_then(std::ffi::OsStr::to_str)
        .unwrap_or("png");
    let mime_type = match ext {
        "jpg" | "jpeg" => "image/jpeg",
        "gif" => "image/gif",
        "png" => "image/png",
        _ => "application/octet-stream",
    };

    Ok(format!(
        r#"<img src="data:{};base64,{}" />"#,
        mime_type, base64_img
    ))
}

fn get_modification_date(path: &PathBuf) -> Option<SystemTime> {
    match fs::metadata(path) {
        Ok(metadata) => metadata.modified().ok(),
        Err(_) => None,
    }
}

// fn file_is_readable(file_path: &Path) -> bool {
//     file_path.is_file() && File::open(file_path).is_ok()
// }

impl Plugin for MarkdownRenderingPlugin {


    fn set_path(&mut self, path: String) {
        self.path = path;
    }

    fn get_path(&self) -> &String {
        &self.path
    }

    fn set_params(&mut self, params: HashMap<String, Value>) -> Result<(), String> {
        if params.is_empty() {
            Ok(())
        } else {
            Err("This plugin does not currently support any parameters.".into())
        }
    }

    fn get_params_schema(&self) -> HashMap<(String, bool), serde_json::Value> {
        HashMap::new()
    }

    fn get_name(&self) -> &str {
        "Markdown rendering"
    }

    fn get_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }
}

impl ClusterProcessingPlugin for MarkdownRenderingPlugin {
    fn process_cluster(
        &self,
        cluster_path: &Path,
        _cluster: &domain::Cluster,
    ) -> Result<HashSet<ArtifactMapping>, anyhow::Error> {
        let md_files = find_md_files(cluster_path);
        let empty_set = HashSet::new();
        md_files.iter().try_fold(empty_set, |empty_set, md_file| {
            let html_counterpart = md_file.with_extension("html");
            let md_modification_date = get_modification_date(md_file);
            let html_modification_date = get_modification_date(&html_counterpart);
            let relation = md_modification_date
                .zip(html_modification_date)
                .map(|(md_time, html_time)| md_time.cmp(&html_time));
            match relation {
                None | Some(Ordering::Equal) | Some(Ordering::Greater) => {
                    let file_contents = std::fs::read_to_string(md_file);
                    match file_contents {
                        Err(e) => Err(e.into()),
                        Ok(file_contents) => {
                            let html_output = markdown_to_html_with_inlined_images(&file_contents);
                            std::fs::write(html_counterpart, &html_output)
                                .map(|_| empty_set)
                                .map_err(|e| e.into())
                        }
                    }
                }
                Some(Ordering::Less) => Ok(empty_set),
            }
        })
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn ClusterProcessingPlugin {
    let plugin = Box::new(MarkdownRenderingPlugin {
        path: "".into()
    });
    Box::into_raw(plugin)
}
