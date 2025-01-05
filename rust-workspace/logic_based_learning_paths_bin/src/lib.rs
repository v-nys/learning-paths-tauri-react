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
        ExtensionFieldProcessingPayload, ExtensionFieldProcessingResult, NodeProcessingError,
        NodeProcessingPayload,
    };
    use serde_yaml;
    use std::{collections::HashMap, path::Path, path::PathBuf};

    struct BogusUserData {}

    // first parameter with type, followed by semicolon ⇒ UserData
    // TODO: can I provide cluster_path instead?
    // that way I could check that the plugin is only accessing part of the file system
    host_fn!(file_exists(_user_data: BogusUserData; relative_path: String) -> () {
      // bool isn't a supported type, so u32
      // TODO: check that resulting path is still a descendant of cluster_path!
      // also note that plugin really only sees relative path
      // so really need to know location of the cluster and node...
      // Ok(1);
      Ok(())
    });

    // want this as a host fn...
    // fn file_is_readable(file_path: &Path) -> bool {
    //     file_path.is_file() && File::open(file_path).is_ok()
    // }

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
                let plugin = PluginBuilder::new(manifest)
                    .with_wasi(true)
                    .with_function(
                        "file_exists",
                        [extism::PTR],
                        [],
                        // actually not sure if this is correct wrt lifetimes etc.
                        // but not using it, so won't get dangling ref...
                        UserData::new(BogusUserData {}),
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
