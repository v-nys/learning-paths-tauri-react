pub mod deserialization;
pub mod domain;

pub mod prelude {
    pub use anyhow;
    pub use schemars;
    pub use serde_json;
    pub use serde_yaml;
}

pub mod plugins {
    use crate::domain::{self, ClusterProcessingResult, Node};
    use extism::{host_fn, Manifest, Plugin, PluginBuilder, UserData, Wasm};
    use logic_based_learning_paths::domain_without_loading::{
        BoolPayload, ClusterProcessingPayload, DirectoryStructurePayload, DummyPayload,
        ExtensionFieldProcessingPayload, ExtensionFieldProcessingResult, FileEntry,
        FileWriteOperationPayload, NodeProcessingError, NodeProcessingPayload, ParamsSchema,
        SystemTimePayload,
    };
    use serde_yaml;
    use std::collections::HashSet;
    use std::time::SystemTime;
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };

    fn get_dir_contents<P: AsRef<Path>>(path: P) -> Result<Vec<FileEntry>, std::io::Error> {
        let mut entries = Vec::new();
        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            let metadata = entry.metadata()?;
            let file_entry = FileEntry {
                name: entry.file_name().to_string_lossy().to_string(),
                is_dir: metadata.is_dir(),
                size: metadata.len(),
                permissions: format!("{:?}", metadata.permissions()),
                modified: metadata
                    .modified()
                    .ok()
                    .and_then(|t| t.elapsed().ok())
                    .map(|e| format!("{:?}", e)),
                created: metadata
                    .created()
                    .ok()
                    .and_then(|t| t.elapsed().ok())
                    .map(|e| format!("{:?}", e)),
            };
            entries.push(file_entry);
        }
        Ok(entries)
    }

    // TODO: add plugin trait with method get_params_schema as before
    // possibly other methods, too
    pub trait LBLPPlugin {
        fn get_path(&self) -> &String;
        // TODO: consider adding default impl of get_params_schema here?
        fn get_params_schema(
            &mut self,
        ) -> anyhow::Result<HashMap<String, (bool, serde_json::Value)>>;
    }

    host_fn!(file_exists(user_data: PathBuf; relative_path: String) -> BoolPayload {
      // '/' should work as a separator under Windows and Linux
      let base_path = user_data.get()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to get inner value.")
          .lock()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to lock eventually.")
          .clone();
      let mut joined_path = base_path.clone();
      joined_path.push(relative_path);
      Ok(BoolPayload { value: joined_path.is_file() && joined_path.starts_with(base_path) })
    });

    host_fn!(get_system_time() -> SystemTimePayload {
        let now = SystemTime::now();
        Ok(SystemTimePayload {
            value: now
        })
    });

    host_fn!(get_last_modification_time(user_data: PathBuf; relative_path: String) -> SystemTimePayload {
      // '/' should work as a separator under Windows and Linux
      let base_path = user_data.get()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to get inner value.")
          .lock()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to lock eventually.")
          .clone();
      let mut joined_path = base_path.clone();
      joined_path.push(relative_path);
      let metadata = std::fs::metadata(joined_path)?;
      let modification_time = metadata.modified()?;
      Ok(SystemTimePayload {
          value: modification_time
        })
    });

    host_fn!(write_text_file(user_data: PathBuf; payload: FileWriteOperationPayload) -> () {
      let FileWriteOperationPayload { relative_path, contents } = payload;
      let base_path = user_data.get()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to get inner value.")
          .lock()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to lock eventually.")
          .clone();
      let mut joined_path = base_path.clone();
      joined_path.push(relative_path);
      Ok(std::fs::write(joined_path, contents)?)
    });

    host_fn!(get_cluster_structure(user_data: PathBuf;) -> DirectoryStructurePayload {
      let base_path = user_data.get()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to get inner value.")
          .lock()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to lock eventually.")
          .clone();
        let serializable_representation = get_dir_contents(base_path)?;
        Ok(DirectoryStructurePayload {
            entries: serializable_representation
        })
    });

    #[derive(Debug)]
    pub struct NodeProcessingPlugin {
        extism_plugin: Plugin,
        parameter_values: HashMap<String, serde_yaml::Value>,
        path: String,
    }

    impl LBLPPlugin for NodeProcessingPlugin {
        fn get_path(&self) -> &String {
            &self.path
        }

        fn get_params_schema(
            &mut self,
        ) -> anyhow::Result<HashMap<String, (bool, serde_json::Value)>> {
            let call_result: Result<ParamsSchema, _> =
                self.extism_plugin.call("get_params_schema", ());
            call_result.map(|s| s.schema)
        }
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

        pub fn get_extension_field_schema(
            &mut self,
        ) -> anyhow::Result<HashMap<String, (bool, serde_json::Value)>> {
            // TODO: consider passing plugin parameter values in the call?
            // could affect the schema
            let call_result: Result<ParamsSchema, _> = dbg!(self
                .extism_plugin
                .call("get_extension_field_schema", DummyPayload {}));
            call_result.map(|s| s.schema)
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
                Err(_e) => ExtensionFieldProcessingResult {
                    result: Err(NodeProcessingError::Remarks(vec![
                        "Plugin does not implement process_extension_field as expected.".to_owned(),
                    ])),
                },
            }
        }
    }

    pub fn load_node_processing_plugins(
        unloaded_plugins: Vec<domain::UnloadedPlugin>,
        cluster_path: &PathBuf,
    ) -> Vec<anyhow::Result<NodeProcessingPlugin>> {
        unloaded_plugins
            .into_iter()
            .map(|unloaded_plugin| {
                let url = Wasm::file(&unloaded_plugin.path);
                let manifest = Manifest::new([url]);
                let plugin = PluginBuilder::new(manifest)
                    .with_wasi(true)
                    .with_function(
                        "file_exists",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        file_exists,
                    )
                    .build();
                plugin.map(|plugin| NodeProcessingPlugin {
                    extism_plugin: plugin,
                    parameter_values: unloaded_plugin.parameters,
                    path: unloaded_plugin.path,
                })
            })
            .collect()
    }

    #[derive(Debug)]
    pub struct ClusterProcessingPlugin {
        extism_plugin: Plugin,
        parameter_values: HashMap<String, serde_yaml::Value>,
        path: String,
    }

    impl LBLPPlugin for ClusterProcessingPlugin {
        fn get_path(&self) -> &String {
            &self.path
        }
        fn get_params_schema(
            &mut self,
        ) -> anyhow::Result<HashMap<String, (bool, serde_json::Value)>> {
            let call_result: Result<ParamsSchema, _> =
                self.extism_plugin.call("get_params_schema", ());
            call_result.map(|s| s.schema)
        }
    }

    impl ClusterProcessingPlugin {
        pub fn process_cluster(
            &mut self,
            cluster_path: &Path,
        ) -> anyhow::Result<HashSet<domain::ArtifactMapping>> {
            // TODO: create payload(s), invoke plugin function, deal with result
            // see node processing counterpart
            dbg!("Might make sense to pass Cluster struct, but problem is that that has loaded plugins.");
            let payload = ClusterProcessingPayload {
                cluster_path: cluster_path.to_path_buf(),
                parameter_values: self.parameter_values.clone(),
            };
            let res: Result<ClusterProcessingResult, _> =
                self.extism_plugin.call("process_cluster", payload);
            res.map(|cpr| cpr.hash_set)
        }
    }

    pub fn load_cluster_processing_plugins(
        unloaded_plugins: Vec<domain::UnloadedPlugin>,
        cluster_path: &PathBuf,
    ) -> Vec<anyhow::Result<ClusterProcessingPlugin>> {
        unloaded_plugins
            .into_iter()
            .map(|unloaded_plugin| {
                let url = Wasm::file(&unloaded_plugin.path);
                let manifest = Manifest::new([url]);
                // TODO: examine whether these can be trimmed
                let plugin = PluginBuilder::new(manifest)
                    .with_wasi(true)
                    .with_function(
                        "file_exists",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        file_exists,
                    )
                    .with_function(
                        "write_text_file",
                        [extism::PTR],
                        [],
                        UserData::new(cluster_path.to_owned()),
                        file_exists,
                    )
                    .with_function(
                        "get_system_time",
                        [],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        file_exists,
                    )
                    .with_function(
                        "get_last_modification_time",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        file_exists,
                    )
                    .with_function(
                        "get_cluster_structure",
                        [],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        file_exists,
                    )
                    .build();
                plugin.map(|plugin| ClusterProcessingPlugin {
                    extism_plugin: plugin,
                    parameter_values: unloaded_plugin.parameters,
                    path: unloaded_plugin.path,
                })
            })
            .collect()
    }
}
