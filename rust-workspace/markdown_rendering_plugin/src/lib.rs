#![allow(improper_ctypes_definitions)]
#![allow(deprecated)]

use base64::encode;
use comrak::nodes::NodeValue;
use comrak::{parse_document, Arena, ComrakOptions};
use logic_based_learning_paths::domain;
use logic_based_learning_paths::plugins::{ArtifactMapping, ClusterProcessingPlugin, Plugin};
use regex;
use logic_based_learning_paths::prelude::{anyhow, serde_json, serde_yaml, schemars};
use schemars::JsonSchema;
use serde_yaml::Value;
use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::str::FromStr;
use std::time::SystemTime;
use std::{
    cmp::Ordering,
    fs,
    path::{Path, PathBuf},
};
use walkdir::WalkDir;

pub struct MarkdownRenderingPlugin {
    path: String,
}

#[derive(JsonSchema)]
#[schemars(deny_unknown_fields)]
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

fn read_markdown_to_html_with_inlined_images(md_path: &PathBuf) -> anyhow::Result<String> {
    let protocol_re = regex::Regex::new(r#"[A-Za-z]+://.+"#)
        .expect("This regex has been tested. It won't fail to compile.");
    let markdown = std::fs::read_to_string(md_path)?;
    // TODO: get rid of tl dependency?
    let arena = Arena::new();
    let root = parse_document(&arena, &markdown, &ComrakOptions::default());
    for node in root.descendants() {
        if let NodeValue::Image(ref mut link) = node.data.borrow_mut().value {
            // see https://docs.rs/comrak/0.26.0/comrak/nodes/struct.NodeLink.html
            let existing_url = &link.url.clone();
            if !protocol_re.is_match(existing_url) {
                if existing_url.contains("\\") {
                    Err(anyhow::anyhow!(format!(
                        "Path {} contains backslash. Use forward slash, even on Windows.",
                        existing_url
                    )))?
                } else {
                    let url_path = std::path::PathBuf::from_str(existing_url)?;
                    if url_path.is_absolute() {
                        Err(anyhow::anyhow!(format!(
                            "Path {} is absolute. For portability reasons, this is not allowed.",
                            existing_url
                        )))?
                    } else {
                        let img_path = md_path.with_file_name(&url_path);
                        let ext = img_path
                            .extension()
                            .and_then(std::ffi::OsStr::to_str)
                            .ok_or(anyhow::anyhow!(
                                "Image lacks an extension: {}",
                                img_path.to_string_lossy()
                            ))?;
                        let mime_type = match ext {
                            "jpg" | "jpeg" => "image/jpeg",
                            "gif" => "image/gif",
                            "png" => "image/png",
                            _ => Err(anyhow::anyhow!(
                                "Unsupported extension for {}",
                                img_path.to_string_lossy()
                            ))?,
                        };
                        let mut file = fs::File::open(img_path)?;
                        let mut buf = Vec::new();
                        file.read_to_end(&mut buf)?;
                        let base64_img = encode(&buf);
                        link.url = format!(r#"data:{};base64,{}"#, mime_type, base64_img)
                    }
                }
            }
        }
    }
    let mut html = vec![];
    comrak::format_html(root, &comrak::Options::default(), &mut html)?;
    String::from_utf8(html).map_err(|_| {
      anyhow::anyhow!("Encoding error".to_owned())
    })
}

fn get_modification_date(path: &PathBuf) -> Option<SystemTime> {
    match fs::metadata(path) {
        Ok(metadata) => metadata.modified().ok(),
        Err(_) => None,
    }
}

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
                    // provide path, not just contents
                    // let file_contents = std::fs::read_to_string(md_file);
                    let html_output = read_markdown_to_html_with_inlined_images(md_file)?;
                    std::fs::write(html_counterpart, &html_output)
                        .map(|_| empty_set)
                        .map_err(|e| e.into())

                    /*match file_contents {
                        Err(e) => Err(e.into()),
                        Ok(file_contents) => {
                            let html_output = markdown_to_html_with_inlined_images(&file_contents)?;
                            std::fs::write(html_counterpart, &html_output)
                                .map(|_| empty_set)
                                .map_err(|e| e.into())
                        }
                    }*/
                }
                Some(Ordering::Less) => Ok(empty_set),
            }
        })
    }
}

#[no_mangle]
pub extern "C" fn create_plugin() -> *mut dyn ClusterProcessingPlugin {
    let plugin = Box::new(MarkdownRenderingPlugin { path: "".into() });
    Box::into_raw(plugin)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use std::str::FromStr;

    const FLAKE_DIR: &str = env!("FLAKE_DIR");

    use super::*;

    #[test]
    fn find_only_md_files_in_flat_dir() {
        let mut plugin_path = std::path::PathBuf::from_str(FLAKE_DIR).expect("Is infallible.");
        plugin_path.push("rust-workspace/markdown_rendering_plugin");
        let search_dir = plugin_path.join("tests/flat-dir/");
        let mut md_files = find_md_files(&search_dir);
        md_files.sort();
        let file2_path = plugin_path.join("tests/flat-dir/file2.md");
        let file3_path = plugin_path.join("tests/flat-dir/file3.md");
        assert_eq!(md_files, vec![file2_path, file3_path]);
    }

    #[test]
    fn find_only_md_files_in_dir_hierarchy() {
        let mut plugin_path = std::path::PathBuf::from_str(FLAKE_DIR).expect("Is infallible.");
        plugin_path.push("rust-workspace/markdown_rendering_plugin");
        let search_dir = plugin_path.join("tests/nested-dir/");
        let mut md_files = find_md_files(&search_dir);
        md_files.sort();
        let a_path = plugin_path.join("tests/nested-dir/a.md");
        let c_path = plugin_path.join("tests/nested-dir/subdir1/c.md");
        let d_path = plugin_path.join("tests/nested-dir/subdir1/subdir1A/d.md");
        let g_path = plugin_path.join("tests/nested-dir/subdir2/subdir2B/g.md");
        assert_eq!(md_files, vec![a_path, c_path, d_path, g_path]);
    }

    #[test]
    #[ignore]
    fn inline_image_in_missing_file() {
        let mut plugin_path = std::path::PathBuf::from_str(FLAKE_DIR).expect("Is infallible.");
        plugin_path.push("rust-workspace/markdown_rendering_plugin");
        let file_path = plugin_path.join("tests/folder-without-html/index.html");
        todo!("complete")
        // let inline_result = image_path_to_tag(&file_path.to_string_lossy());
        // assert!(inline_result.is_err());
    }

    #[test]
    #[ignore]
    fn inline_when_protocol_is_specified() {
        todo!("implement")
    }

    #[test]
    #[ignore]
    fn inline_unsupported_image_type() {
        todo!("implement")
    }

    #[test]
    #[ignore]
    fn inline_missing_image() {
        todo!("implement")
    }

    #[test]
    #[ignore]
    fn inline_jpgs_in_simple_page() {
        todo!("implement")
    }

    #[test]
    #[ignore]
    fn inline_gifs_in_simple_page() {
        todo!("implement")
    }

    #[test]
    fn inline_pngs_in_simple_page() {
        let mut plugin_path = std::path::PathBuf::from_str(FLAKE_DIR).expect("Is infallible.");
        plugin_path.push("rust-workspace/markdown_rendering_plugin");
        let file_path = plugin_path.join("tests/page-with-pngs/index.md");
        let inline_result = read_markdown_to_html_with_inlined_images(&file_path);
        assert!(inline_result.is_ok());
        let text = inline_result.unwrap();
        assert_eq!(
            text,
            r###"<p><img src="..." alt="" />
<img src="..." alt="" /></p>"###
        );
    }

    #[test]
    #[ignore]
    fn inline_webps_in_simple_page() {
        todo!("implement")
    }

    #[test]
    #[ignore]
    fn inline_svgs_in_simple_page() {
        todo!("implement")
    }

    #[test]
    #[ignore]
    fn full_md_transformation() {
        todo!("implement")
    }

    #[test]
    #[ignore]
    fn process_dummy_cluster() {
        todo!("implement")
    }
}
