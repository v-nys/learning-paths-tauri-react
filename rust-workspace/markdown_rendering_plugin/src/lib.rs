#![allow(improper_ctypes_definitions)]
#![allow(deprecated)]

use base64::encode;
use comrak::nodes::NodeValue;
use comrak::{parse_document, Arena, ComrakOptions};
use logic_based_learning_paths::domain;
use logic_based_learning_paths::plugins::{ArtifactMapping, ClusterProcessingPlugin, Plugin};
use logic_based_learning_paths::prelude::{anyhow, schemars, serde_json, serde_yaml};
use regex;
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
    let arena = Arena::new();
    let root = parse_document(&arena, &markdown, &ComrakOptions::default());
    for node in root.descendants() {
        let mut is_relative_svg = false;
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
                        if ext == "svg" {
                            is_relative_svg = true;
                        } else {
                            let mime_type = match ext {
                                "jpg" | "jpeg" => "image/jpeg",
                                "gif" => "image/gif",
                                "png" => "image/png",
                                "webp" => "image/webp",
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
        if is_relative_svg {
            // this is needed because alt text will otherwise appear as inline text
            /*node.children().for_each(|child| {
                child.detach();
            });*/
            let mut to_be_replaced = node.data.borrow_mut();
            to_be_replaced.value = NodeValue::HtmlInline(r#"<svg></svg>"#.into());
        }
    }
    let mut html = vec![];
    let mut render_options = comrak::Options::default();
    // needed to render inline SVGs, as there is no element for that
    render_options.render.unsafe_ = true;
    comrak::format_html(root, &render_options, &mut html)?;
    String::from_utf8(html).map_err(|_| anyhow::anyhow!("Encoding error".to_owned()))
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
                    let html_output = read_markdown_to_html_with_inlined_images(md_file)?;
                    std::fs::write(html_counterpart, &html_output)
                        .map(|_| empty_set)
                        .map_err(|e| e.into())
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
    fn inline_image_in_missing_file() {
        let mut plugin_path = std::path::PathBuf::from_str(FLAKE_DIR).expect("Is infallible.");
        plugin_path.push("rust-workspace/markdown_rendering_plugin");
        let file_path = plugin_path.join("tests/folder-without-html/index.html");
        todo!("complete")
        // let inline_result = image_path_to_tag(&file_path.to_string_lossy());
        // assert!(inline_result.is_err());
    }

    #[test]
    fn do_not_inline_when_protocol_is_specified() {
        todo!("implement")
    }

    #[test]
    fn inline_unsupported_image_type() {
        todo!("implement")
    }

    #[test]
    fn inline_missing_image() {
        todo!("implement")
    }

    #[test]
    fn inline_jpgs_in_simple_page() {
        let mut plugin_path = std::path::PathBuf::from_str(FLAKE_DIR).expect("Is infallible.");
        plugin_path.push("rust-workspace/markdown_rendering_plugin");
        let file_path = plugin_path.join("tests/page-with-jpgs/index.md");
        let inline_result = read_markdown_to_html_with_inlined_images(&file_path);
        assert!(inline_result.is_ok());
        let text = inline_result.unwrap();
        assert_eq!(
            text,
            r###"<p><img src="data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEBLAEsAAD//gATQ3JlYXRlZCB3aXRoIEdJTVD/4gKwSUNDX1BST0ZJTEUAAQEAAAKgbGNtcwRAAABtbnRyUkdCIFhZWiAH6AAIAAYACgApAA5hY3NwQVBQTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA9tYAAQAAAADTLWxjbXMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA1kZXNjAAABIAAAAEBjcHJ0AAABYAAAADZ3dHB0AAABmAAAABRjaGFkAAABrAAAACxyWFlaAAAB2AAAABRiWFlaAAAB7AAAABRnWFlaAAACAAAAABRyVFJDAAACFAAAACBnVFJDAAACFAAAACBiVFJDAAACFAAAACBjaHJtAAACNAAAACRkbW5kAAACWAAAACRkbWRkAAACfAAAACRtbHVjAAAAAAAAAAEAAAAMZW5VUwAAACQAAAAcAEcASQBNAFAAIABiAHUAaQBsAHQALQBpAG4AIABzAFIARwBCbWx1YwAAAAAAAAABAAAADGVuVVMAAAAaAAAAHABQAHUAYgBsAGkAYwAgAEQAbwBtAGEAaQBuAABYWVogAAAAAAAA9tYAAQAAAADTLXNmMzIAAAAAAAEMQgAABd7///MlAAAHkwAA/ZD///uh///9ogAAA9wAAMBuWFlaIAAAAAAAAG+gAAA49QAAA5BYWVogAAAAAAAAJJ8AAA+EAAC2xFhZWiAAAAAAAABilwAAt4cAABjZcGFyYQAAAAAAAwAAAAJmZgAA8qcAAA1ZAAAT0AAACltjaHJtAAAAAAADAAAAAKPXAABUfAAATM0AAJmaAAAmZwAAD1xtbHVjAAAAAAAAAAEAAAAMZW5VUwAAAAgAAAAcAEcASQBNAFBtbHVjAAAAAAAAAAEAAAAMZW5VUwAAAAgAAAAcAHMAUgBHAEL/2wBDAAMCAgMCAgMDAwMEAwMEBQgFBQQEBQoHBwYIDAoMDAsKCwsNDhIQDQ4RDgsLEBYQERMUFRUVDA8XGBYUGBIUFRT/2wBDAQMEBAUEBQkFBQkUDQsNFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBT/wgARCAABAAEDAREAAhEBAxEB/8QAFAABAAAAAAAAAAAAAAAAAAAAB//EABUBAQEAAAAAAAAAAAAAAAAAAAYI/9oADAMBAAIQAxAAAAE5C1T/AP/EABQQAQAAAAAAAAAAAAAAAAAAAAD/2gAIAQEAAQUCf//EABQRAQAAAAAAAAAAAAAAAAAAAAD/2gAIAQMBAT8Bf//EABQRAQAAAAAAAAAAAAAAAAAAAAD/2gAIAQIBAT8Bf//EABQQAQAAAAAAAAAAAAAAAAAAAAD/2gAIAQEABj8Cf//EABQQAQAAAAAAAAAAAAAAAAAAAAD/2gAIAQEAAT8hf//aAAwDAQACAAMAAAAQ/wD/xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oACAEDAQE/EH//xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oACAECAQE/EH//xAAUEAEAAAAAAAAAAAAAAAAAAAAA/9oACAEBAAE/EH//2Q==" alt="red dot" />
<img src="data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEBLAEsAAD//gATQ3JlYXRlZCB3aXRoIEdJTVD/4gKwSUNDX1BST0ZJTEUAAQEAAAKgbGNtcwRAAABtbnRyUkdCIFhZWiAH6AAIAAYACgApAA5hY3NwQVBQTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA9tYAAQAAAADTLWxjbXMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA1kZXNjAAABIAAAAEBjcHJ0AAABYAAAADZ3dHB0AAABmAAAABRjaGFkAAABrAAAACxyWFlaAAAB2AAAABRiWFlaAAAB7AAAABRnWFlaAAACAAAAABRyVFJDAAACFAAAACBnVFJDAAACFAAAACBiVFJDAAACFAAAACBjaHJtAAACNAAAACRkbW5kAAACWAAAACRkbWRkAAACfAAAACRtbHVjAAAAAAAAAAEAAAAMZW5VUwAAACQAAAAcAEcASQBNAFAAIABiAHUAaQBsAHQALQBpAG4AIABzAFIARwBCbWx1YwAAAAAAAAABAAAADGVuVVMAAAAaAAAAHABQAHUAYgBsAGkAYwAgAEQAbwBtAGEAaQBuAABYWVogAAAAAAAA9tYAAQAAAADTLXNmMzIAAAAAAAEMQgAABd7///MlAAAHkwAA/ZD///uh///9ogAAA9wAAMBuWFlaIAAAAAAAAG+gAAA49QAAA5BYWVogAAAAAAAAJJ8AAA+EAAC2xFhZWiAAAAAAAABilwAAt4cAABjZcGFyYQAAAAAAAwAAAAJmZgAA8qcAAA1ZAAAT0AAACltjaHJtAAAAAAADAAAAAKPXAABUfAAATM0AAJmaAAAmZwAAD1xtbHVjAAAAAAAAAAEAAAAMZW5VUwAAAAgAAAAcAEcASQBNAFBtbHVjAAAAAAAAAAEAAAAMZW5VUwAAAAgAAAAcAHMAUgBHAEL/2wBDAAMCAgMCAgMDAwMEAwMEBQgFBQQEBQoHBwYIDAoMDAsKCwsNDhIQDQ4RDgsLEBYQERMUFRUVDA8XGBYUGBIUFRT/2wBDAQMEBAUEBQkFBQkUDQsNFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBT/wgARCAABAAEDAREAAhEBAxEB/8QAFAABAAAAAAAAAAAAAAAAAAAABf/EABUBAQEAAAAAAAAAAAAAAAAAAAcI/9oADAMBAAIQAxAAAAF0PnD/xAAUEAEAAAAAAAAAAAAAAAAAAAAA/9oACAEBAAEFAn//xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oACAEDAQE/AX//xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oACAECAQE/AX//xAAUEAEAAAAAAAAAAAAAAAAAAAAA/9oACAEBAAY/An//xAAUEAEAAAAAAAAAAAAAAAAAAAAA/9oACAEBAAE/IX//2gAMAwEAAgADAAAAEL//xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oACAEDAQE/EH//xAAUEQEAAAAAAAAAAAAAAAAAAAAA/9oACAECAQE/EH//xAAUEAEAAAAAAAAAAAAAAAAAAAAA/9oACAEBAAE/EH//2Q==" alt="green dot" /></p>
"###
        );
    }

    #[test]
    fn inline_gifs_in_simple_page() {
        let mut plugin_path = std::path::PathBuf::from_str(FLAKE_DIR).expect("Is infallible.");
        plugin_path.push("rust-workspace/markdown_rendering_plugin");
        let file_path = plugin_path.join("tests/page-with-gifs/index.md");
        let inline_result = read_markdown_to_html_with_inlined_images(&file_path);
        assert!(inline_result.is_ok());
        let text = inline_result.unwrap();
        assert_eq!(
            text,
            r###"<p><img src="data:image/gif;base64,R0lGODlhAQABAIABAP8AAP///yH+EUNyZWF0ZWQgd2l0aCBHSU1QACwAAAAAAQABAAACAkQBADs=" alt="red dot" />
<img src="data:image/gif;base64,R0lGODlhAQABAIABAAD/AP///yH+EUNyZWF0ZWQgd2l0aCBHSU1QACwAAAAAAQABAAACAkQBADs=" alt="green dot" /></p>
"###
        );
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
            r###"<p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAABg2lDQ1BJQ0MgcHJvZmlsZQAAKJF9kT1Iw0AcxV9TS0upONhBxCFgdbKLijjWKhShQqgVWnUwuX5Ck4YkxcVRcC04+LFYdXBx1tXBVRAEP0CcHZwUXaTE/yWFFjEeHPfj3b3H3TtAaNWYavYlAFWzjEwqKebyq2LwFSEEEEYQozIz9TlJSsNzfN3Dx9e7OM/yPvfn6C8UTQb4ROIE0w2LeIN4ZtPSOe8TR1lFLhCfE08YdEHiR64rLr9xLjss8Myokc3ME0eJxXIPKz3MKoZKPE0cK6ga5Qs5lwuctzirtQbr3JO/MFLUVpa5TnMEKSxiCRJEKGigihosxGnVSDGRof2kh3/Y8UvkUshVBSPHAupQITt+8D/43a1Zmpp0kyJJIPBi2x9jQHAXaDdt+/vYttsngP8ZuNK6/noLmP0kvdnVYkfAwDZwcd3VlD3gcgcYetJlQ3YkP02hVALez+ib8sDgLRBec3vr7OP0AchSV+kb4OAQGC9T9rrHu0O9vf17ptPfDz/8cpI+ix9OAAAACXBIWXMAAC4jAAAuIwF4pT92AAAAB3RJTUUH6AgFCggeDiy6AQAAABl0RVh0Q29tbWVudABDcmVhdGVkIHdpdGggR0lNUFeBDhcAAAAMSURBVAjXY/jPwAAAAwEBABjdjbAAAAAASUVORK5CYII=" alt="red dot" />
<img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAABhGlDQ1BJQ0MgcHJvZmlsZQAAKJF9kT1Iw0AYht+mlpZScbCDiEOG6mSXKuJYq1CECqFWaNXB5NI/aNKSpLg4Cq4FB38Wqw4uzro6uAqC4A+Is4OToouU+F1SaBHjwd09vPe9L3ffAUK7xjRzIAloumVk0ykxX1gVg68IIYAwrQmZmY05ScrAc3zdw8f3uzjP8q77cwyqRZMBPpE4yRqGRbxBPLNpNTjvE0dZRVaJz4knDbog8SPXFZffOJcdFnhm1Mhl54mjxGK5j5U+ZhVDI54mjqmaTvlC3mWV8xZnrdZk3XvyF0aK+soy12mOIY1FLEGCCAVNVFGDhTjtOikmsnSe8vCPOn6JXAq5qmDkWEAdGmTHD/4Hv3trlqYSblIkBQRebPtjHAjuAp2WbX8f23bnBPA/A1d6z19vA7OfpLd6WuwIGNoGLq57mrIHXO4AI08N2ZAdyU9TKJWA9zP6pgIwfAuE19y+dc9x+gDkqFeZG+DgEJgoU/a6x7tD/X37t6bbvx9nznKid75KdgAAAAlwSFlzAAAuIwAALiMBeKU/dgAAAAd0SU1FB+gIBQoKA18ctFoAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJTVBXgQ4XAAAADElEQVQI12Ng+M8AAAICAQCqKp4nAAAAAElFTkSuQmCC" alt="green dot" /></p>
"###
        );
    }

    #[test]
    fn inline_webps_in_simple_page() {
        let mut plugin_path = std::path::PathBuf::from_str(FLAKE_DIR).expect("Is infallible.");
        plugin_path.push("rust-workspace/markdown_rendering_plugin");
        let file_path = plugin_path.join("tests/page-with-webps/index.md");
        let inline_result = read_markdown_to_html_with_inlined_images(&file_path);
        assert!(inline_result.is_ok());
        let text = inline_result.unwrap();
        assert_eq!(
            text,
            r###"<p><img src="data:image/webp;base64,UklGRvoCAABXRUJQVlA4WAoAAAAgAAAAAAAAAAAASUNDUKACAAAAAAKgbGNtcwRAAABtbnRyUkdCIFhZWiAH6AAIAAYACgApAA5hY3NwQVBQTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA9tYAAQAAAADTLWxjbXMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA1kZXNjAAABIAAAAEBjcHJ0AAABYAAAADZ3dHB0AAABmAAAABRjaGFkAAABrAAAACxyWFlaAAAB2AAAABRiWFlaAAAB7AAAABRnWFlaAAACAAAAABRyVFJDAAACFAAAACBnVFJDAAACFAAAACBiVFJDAAACFAAAACBjaHJtAAACNAAAACRkbW5kAAACWAAAACRkbWRkAAACfAAAACRtbHVjAAAAAAAAAAEAAAAMZW5VUwAAACQAAAAcAEcASQBNAFAAIABiAHUAaQBsAHQALQBpAG4AIABzAFIARwBCbWx1YwAAAAAAAAABAAAADGVuVVMAAAAaAAAAHABQAHUAYgBsAGkAYwAgAEQAbwBtAGEAaQBuAABYWVogAAAAAAAA9tYAAQAAAADTLXNmMzIAAAAAAAEMQgAABd7///MlAAAHkwAA/ZD///uh///9ogAAA9wAAMBuWFlaIAAAAAAAAG+gAAA49QAAA5BYWVogAAAAAAAAJJ8AAA+EAAC2xFhZWiAAAAAAAABilwAAt4cAABjZcGFyYQAAAAAAAwAAAAJmZgAA8qcAAA1ZAAAT0AAACltjaHJtAAAAAAADAAAAAKPXAABUfAAATM0AAJmaAAAmZwAAD1xtbHVjAAAAAAAAAAEAAAAMZW5VUwAAAAgAAAAcAEcASQBNAFBtbHVjAAAAAAAAAAEAAAAMZW5VUwAAAAgAAAAcAHMAUgBHAEJWUDggNAAAADACAJ0BKgEAAQAAwBIloAJ0ugH4AfgABGgAAP76IZf/d5qw03f1rf/1o5+uif60c/9ZWAA=" alt="red dot" />
<img src="data:image/webp;base64,UklGRvoCAABXRUJQVlA4WAoAAAAgAAAAAAAAAAAASUNDUKACAAAAAAKgbGNtcwRAAABtbnRyUkdCIFhZWiAH6AAIAAYACgApAA5hY3NwQVBQTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA9tYAAQAAAADTLWxjbXMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA1kZXNjAAABIAAAAEBjcHJ0AAABYAAAADZ3dHB0AAABmAAAABRjaGFkAAABrAAAACxyWFlaAAAB2AAAABRiWFlaAAAB7AAAABRnWFlaAAACAAAAABRyVFJDAAACFAAAACBnVFJDAAACFAAAACBiVFJDAAACFAAAACBjaHJtAAACNAAAACRkbW5kAAACWAAAACRkbWRkAAACfAAAACRtbHVjAAAAAAAAAAEAAAAMZW5VUwAAACQAAAAcAEcASQBNAFAAIABiAHUAaQBsAHQALQBpAG4AIABzAFIARwBCbWx1YwAAAAAAAAABAAAADGVuVVMAAAAaAAAAHABQAHUAYgBsAGkAYwAgAEQAbwBtAGEAaQBuAABYWVogAAAAAAAA9tYAAQAAAADTLXNmMzIAAAAAAAEMQgAABd7///MlAAAHkwAA/ZD///uh///9ogAAA9wAAMBuWFlaIAAAAAAAAG+gAAA49QAAA5BYWVogAAAAAAAAJJ8AAA+EAAC2xFhZWiAAAAAAAABilwAAt4cAABjZcGFyYQAAAAAAAwAAAAJmZgAA8qcAAA1ZAAAT0AAACltjaHJtAAAAAAADAAAAAKPXAABUfAAATM0AAJmaAAAmZwAAD1xtbHVjAAAAAAAAAAEAAAAMZW5VUwAAAAgAAAAcAEcASQBNAFBtbHVjAAAAAAAAAAEAAAAMZW5VUwAAAAgAAAAcAHMAUgBHAEJWUDggNAAAABACAJ0BKgEAAQAAwBIloAJ0ugH4AfoAA8gA/u69n/9y9p+ifD/uor/+UH4RiDb/5WIAAAA=" alt="green dot" /></p>
"###
        );
    }

    #[test]
    fn inline_svgs_in_simple_page() {
        let mut plugin_path = std::path::PathBuf::from_str(FLAKE_DIR).expect("Is infallible.");
        plugin_path.push("rust-workspace/markdown_rendering_plugin");
        let file_path = plugin_path.join("tests/page-with-svgs/index.md");
        let inline_result = read_markdown_to_html_with_inlined_images(&file_path);
        assert!(inline_result.is_ok());
        let text = inline_result.unwrap();
        assert_eq!(
            text,
            r###"<p><svg
   width="15.103628mm"
   height="15.103628mm"
   viewBox="0 0 15.103628 15.103628"
   version="1.1"
   id="svg1"
   inkscape:version="1.3.2 (091e20ef0f, 2023-11-25)"
   sodipodi:docname="1.svg"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:svg="http://www.w3.org/2000/svg">
  <sodipodi:namedview
     id="namedview1"
     pagecolor="#ffffff"
     bordercolor="#000000"
     borderopacity="0.25"
     inkscape:showpageshadow="2"
     inkscape:pageopacity="0.0"
     inkscape:pagecheckerboard="0"
     inkscape:deskcolor="#d1d1d1"
     inkscape:document-units="mm"
     inkscape:zoom="1.0378437"
     inkscape:cx="19.752492"
     inkscape:cy="135.37684"
     inkscape:window-width="3440"
     inkscape:window-height="1371"
     inkscape:window-x="0"
     inkscape:window-y="0"
     inkscape:window-maximized="1"
     inkscape:current-layer="layer1"
     inkscape:clip-to-page="false" />
  <defs
     id="defs1" />
  <g
     inkscape:label="Layer 1"
     inkscape:groupmode="layer"
     id="layer1"
     transform="translate(-99.615252,-112.73349)">
    <rect
       style="fill:#ff0000;stroke:#ffffff;stroke-width:0.264583"
       id="rect1"
       width="14.839046"
       height="14.839046"
       x="99.747543"
       y="112.86578" />
  </g>
</svg>
<svg
   width="15.103628mm"
   height="15.103628mm"
   viewBox="0 0 15.103628 15.103628"
   version="1.1"
   id="svg1"
   inkscape:version="1.3.2 (091e20ef0f, 2023-11-25)"
   sodipodi:docname="2.svg"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:svg="http://www.w3.org/2000/svg">
  <sodipodi:namedview
     id="namedview1"
     pagecolor="#ffffff"
     bordercolor="#000000"
     borderopacity="0.25"
     inkscape:showpageshadow="2"
     inkscape:pageopacity="0.0"
     inkscape:pagecheckerboard="0"
     inkscape:deskcolor="#d1d1d1"
     inkscape:document-units="mm"
     inkscape:zoom="1.0378437"
     inkscape:cx="19.752492"
     inkscape:cy="135.37684"
     inkscape:window-width="3440"
     inkscape:window-height="1371"
     inkscape:window-x="0"
     inkscape:window-y="0"
     inkscape:window-maximized="1"
     inkscape:current-layer="layer1"
     inkscape:clip-to-page="false" />
  <defs
     id="defs1" />
  <g
     inkscape:label="Layer 1"
     inkscape:groupmode="layer"
     id="layer1"
     transform="translate(-99.615252,-112.73349)">
    <rect
       style="fill:#00ff00;stroke:#ffffff;stroke-width:0.264583"
       id="rect1"
       width="14.839046"
       height="14.839046"
       x="99.747543"
       y="112.86578" />
  </g>
</svg>
</p>
"###
        );
    }

    #[test]
    #[ignore]
    fn process_dummy_cluster() {
        todo!("implement")
    }
}
