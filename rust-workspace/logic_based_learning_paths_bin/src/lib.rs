pub mod deserialization;
pub mod domain;

pub mod prelude {
    pub use anyhow;
    pub use schemars;
    pub use serde_json;
    pub use serde_yaml;
}

pub mod plugins {
    use crate::domain::{self, Node};
    use extism::{host_fn, Manifest, Plugin, PluginBuilder, UserData, Wasm};
    use logic_based_learning_paths::domain_without_loading::{
        BoolPayload, ExtensionFieldProcessingPayload, ExtensionFieldProcessingResult,
        NodeProcessingError, NodeProcessingPayload,
    };
    use serde_yaml;
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
        str::FromStr,
    };

    host_fn!(file_exists(user_data: PathBuf; relative_path: String) -> BoolPayload {
      dbg!(&user_data);
      let mut joined_path = user_data.get()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to get inner value.")
          .lock()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to lock eventually.")
          .clone();
      joined_path.push(relative_path);
      // TODO: also make sure joined_path is an EXTENSION of the cluster path
      Ok(BoolPayload { value: joined_path.is_file() })
    });

    #[derive(Debug)]
    pub struct NodeProcessingPlugin {
        extism_plugin: Plugin,
        parameter_values: HashMap<String, serde_yaml::Value>,
    }

    impl NodeProcessingPlugin {
        // TODO: am I actually invoking this anywhere?
        pub fn run(&mut self, node: &Node, cluster_path: &Path) -> anyhow::Result<()> {
            let payload = NodeProcessingPayload {
                parameter_values: self.parameter_values.clone(),
                node: node.clone(),
                cluster_path: cluster_path.to_owned(),
            };
            // FIXME: should just expect the result to be () in the future
            // but a String is useful for testing whether WASM code is running properly
            let res = self.extism_plugin.call("process_node", payload);
            res.map(|representation: String| {
                println!("{representation}");
                ()
            })
        }

        pub fn process_extension_field(
            &mut self,
            cluster_path: &Path,
            node: &domain::Node,
            field_name: &str,
            value: &serde_yaml::Value,
        ) -> ExtensionFieldProcessingResult {
            let node_processing_payload = NodeProcessingPayload {
                parameter_values: self.parameter_values.clone(),
                node: node.clone(),
                cluster_path: cluster_path.to_owned(),
            };
            let payload = ExtensionFieldProcessingPayload {
                node_processing_payload,
                field_name: field_name.to_owned(),
                value: value.to_owned(),
            };
            let res: Result<ExtensionFieldProcessingResult, _> =
                self.extism_plugin.call("process_extension_field", payload);
            match res {
                Ok(inner_result) => inner_result,
                Err(e) => ExtensionFieldProcessingResult {
                    result: Err(NodeProcessingError::Remarks(vec![
                        "Plugin does not implement process_extension_field as expected.".to_owned(),
                    ])),
                },
            }
        }
    }

    pub fn load_node_processing_plugins(
        unloaded_plugins: Vec<domain::UnloadedPlugin>,
    ) -> Vec<anyhow::Result<NodeProcessingPlugin>> {
        unloaded_plugins
            .into_iter()
            .map(|unloaded_plugin| {
                let url = Wasm::file(unloaded_plugin.path);
                let manifest = Manifest::new([url]);
                dbg!("Still have a TODO in with_function below.");
                let plugin = PluginBuilder::new(manifest)
                    .with_wasi(true)
                    .with_function(
                        "file_exists",
                        [extism::PTR],
                        [extism::PTR],
                        // actually not sure if this is correct wrt lifetimes etc.
                        // but not using it, so won't get dangling ref...
                        // TODO: get rid of hardcoded value
                        // should be the cluster's (or node's) path instead
                        UserData::new(PathBuf::from_str("/home/vincentn/Documents/Lesmateriaal/2024-2025/clusters/PA/cluster-python-concepten").unwrap()),
                        file_exists,
                    )
                    .build();
                plugin.map(|plugin| NodeProcessingPlugin {
                    extism_plugin: plugin,
                    parameter_values: unloaded_plugin.parameters,
                })
            })
            .collect()
    }
}
