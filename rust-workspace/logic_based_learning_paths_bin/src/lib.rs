pub mod deserialization;
pub mod domain;

pub mod prelude {
    pub use anyhow;
    pub use schemars;
    pub use serde_json;
    pub use serde_yaml;
}

pub mod plugins {
    use crate::domain::{self,Node};
    use extism::{Manifest, Plugin, Wasm};
    use serde_yaml;
    use std::{collections::HashMap, path::Path};
    use logic_based_learning_paths::domain_without_loading::NodeProcessingPayload;

    #[derive(Debug)]
    pub struct NodeProcessingPlugin {
        extism_plugin: Plugin,
        parameter_values: HashMap<String, serde_yaml::Value>,
    }

    impl NodeProcessingPlugin {
        pub fn run(&mut self, node: &Node, cluster_path: &Path) -> anyhow::Result<()> {
            let payload = NodeProcessingPayload {
                parameter_values: self.parameter_values.clone(),
                node: node.clone(),
                cluster_path: cluster_path.to_owned(),
            };
            // FIXME: should just expect the result to be () in the future
            // but a String is useful for testing whether WASM code is running properly
            let res = self.extism_plugin.call("process_node", payload);
            res.map(|representation: String| { println!("{representation}"); () })
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
                let plugin = Plugin::new(&manifest, [], true);
                plugin.map(|plugin| NodeProcessingPlugin {
                    extism_plugin: plugin,
                    parameter_values: unloaded_plugin.parameters,
                })
            })
            .collect()
    }
}
