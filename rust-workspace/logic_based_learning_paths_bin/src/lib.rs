pub mod deserialization;
pub mod domain;
pub mod graph_processing;

pub mod prelude {
    pub use anyhow;
    pub use schemars;
    pub use serde_json;
    pub use serde_yaml;
}

pub mod plugins {
    use crate::domain::{self, ArtifactMapping, ClusterProcessingResult, Node};
    use base64::{engine::general_purpose::STANDARD as BASE64_ENGINE, Engine};
    use extism::{host_fn, Manifest, Plugin, PluginBuilder, UserData, Wasm};
    use logic_based_learning_paths::domain_without_loading::{
        ArchivePayload, BoolPayload, ClusterProcessingPayload, DirectoryStructurePayload,
        DummyPayload, ExtensionFieldProcessingPayload, ExtensionFieldProcessingResult, FileEntry,
        FileReadBase64OperationInPayload, FileReadBase64OperationOutPayload,
        FileReadOperationInPayload, FileReadOperationOutPayload, FileWriteBase64OperationInPayload,
        FileWriteOperationPayload, NodeProcessingError, NodeProcessingPayload, ParamsSchema,
        RootedSupercluster, SystemTimePayload, WorkflowStepProcessingResult,
    };
    use serde_yaml;
    use std::collections::HashSet;
    use std::io::Read;
    use std::time::SystemTime;
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };
    use walkdir::WalkDir;

    fn get_dir_contents<P: AsRef<Path>>(path: P) -> Result<Vec<FileEntry>, std::io::Error> {
        let mut entries = Vec::new();
        for entry in WalkDir::new(&path) {
            let entry = entry?;
            let metadata = entry.metadata()?;
            let relative_path = entry
                .path()
                .strip_prefix(&path)
                .expect("The entry's path has to extend the base path due to use of walkdir.");
            let file_entry = FileEntry {
                relative_path: relative_path.to_string_lossy().to_string(),
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

    pub trait LBLPPlugin {
        fn get_path(&self) -> &String;
        // TODO: consider adding default impl of get_params_schema here?
        fn get_params_schema(
            &mut self,
        ) -> anyhow::Result<HashMap<String, (bool, serde_json::Value)>>;
        // note: cannot provide default implementation here, even if it is the same everywhere
        // need to know concrete type
        fn as_lblp_plugin_mut(&mut self) -> &mut dyn LBLPPlugin;
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
      if joined_path.starts_with(&base_path) {
          Ok(BoolPayload { value: joined_path.is_file() && joined_path.starts_with(base_path) })
      }
      else {
          Err(anyhow::anyhow!(format!("Host function is restricted to extensions of {base_path:?}")))
      }
    });

    host_fn!(get_system_time(_payload: DummyPayload) -> SystemTimePayload {
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
      let metadata = std::fs::metadata(&joined_path)?;
      let modification_time = metadata.modified()?;
      if joined_path.starts_with(&base_path) {
      Ok(SystemTimePayload {
          value: modification_time
        })
      }
      else {
          Err(anyhow::anyhow!(format!("Host function is restricted to extensions of {base_path:?}")))
      }
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
      if joined_path.starts_with(&base_path) {
          std::fs::write(joined_path, contents)?;
          Ok(())
      }
      else {
          Err(anyhow::anyhow!(format!("Host function is restricted to extensions of {base_path:?}")))
      }
    });

    host_fn!(read_text_file(user_data: PathBuf; payload: FileReadOperationInPayload) -> FileReadOperationOutPayload {
       let FileReadOperationInPayload { relative_path } = payload;
       let base_path = user_data.get()
           // TODO: under what circumstances would this fail?
           .expect("Should be able to get inner value.")
           .lock()
           // TODO: under what circumstances would this fail?
           .expect("Should be able to lock eventually.")
           .clone();
       let mut joined_path = base_path.clone();
       joined_path.push(relative_path);
       let read_result = std::fs::read_to_string(&joined_path)?;
       if joined_path.starts_with(&base_path) {
           Ok(FileReadOperationOutPayload { contents: read_result })
       }
        else {
           Err(anyhow::anyhow!(format!("Host function is restricted to extensions of {base_path:?}")))
       }
    });

    // TODO: FOR ALL HOST FUNCTIONS: check that joined_path extends user_data path
    host_fn!(write_binary_file_base64(user_data: PathBuf; payload: FileWriteBase64OperationInPayload) -> () {
      let FileWriteBase64OperationInPayload { relative_path, base64_text } = payload;
      let base_path = user_data.get()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to get inner value.")
          .lock()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to lock eventually.")
          .clone();
      let mut joined_path = base_path.clone();
      joined_path.push(relative_path);
      if joined_path.starts_with(&base_path) {
          let base64_as_bytes = BASE64_ENGINE.decode(&base64_text)?;
          // FIXME: should use result!
          std::fs::write(joined_path, base64_as_bytes);
      Ok(())
      }
      else {
          // Err()
          Ok(())
      }
    });

    // TODO: FOR ALL HOST FUNCTIONS: check that joined_path extends user_data path
    host_fn!(read_binary_file_base64(user_data: PathBuf; payload: FileReadBase64OperationInPayload) -> FileReadBase64OperationOutPayload {
      let FileReadBase64OperationInPayload { relative_path } = payload;
      let base_path = user_data.get()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to get inner value.")
          .lock()
          // TODO: under what circumstances would this fail?
          .expect("Should be able to lock eventually.")
          .clone();
      let mut joined_path = base_path.clone();
      joined_path.push(relative_path);
       if joined_path.starts_with(&base_path) {
       let mut file = std::fs::File::open(joined_path.clone())?;
      let mut buf = Vec::new();
      file.read_to_end(&mut buf)?;
      let base64 = BASE64_ENGINE.encode(&buf);
      Ok(FileReadBase64OperationOutPayload { contents: base64 })

       }
        else {
           Err(anyhow::anyhow!(format!("Host function is restricted to extensions of {base_path:?}")))
       }
    });

    host_fn!(get_cluster_structure(user_data: PathBuf; _payload: DummyPayload) -> DirectoryStructurePayload {
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
    pub struct PreArchivePlugin {
        extism_plugin: Plugin,
        parameter_values: HashMap<String, serde_yaml::Value>,
        path: String,
    }

    impl LBLPPlugin for PreArchivePlugin {
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

        fn as_lblp_plugin_mut(&mut self) -> &mut dyn LBLPPlugin {
            self
        }
    }

    impl PreArchivePlugin {
        pub fn run(
            &mut self,
            cluster_paths: Vec<&Path>,
            artifact_mapping: HashSet<ArtifactMapping>,
            rooted_supercluster: &mut RootedSupercluster,
        ) -> anyhow::Result<WorkflowStepProcessingResult> {
            println!("creating the archive payload");

            let payload = ArchivePayload {
                cluster_paths: cluster_paths.iter().map(|p| p.to_path_buf()).collect(),
                parameter_values: self.parameter_values.clone(),
                artifact_mapping: artifact_mapping.clone(),
                rooted_supercluster: rooted_supercluster.clone(),
            };
            println!("calling the Extism plugin");
            let res: Result<WorkflowStepProcessingResult, anyhow::Error> =
                self.extism_plugin.call("process_paths", payload);

            println!("called the Extism plugin");
            res
        }
    }

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

        fn as_lblp_plugin_mut(&mut self) -> &mut dyn LBLPPlugin {
            self
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
                let joined_path = cluster_path.join(&unloaded_plugin.path);
                let url = Wasm::file(&joined_path);
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
                    .with_function(
                        "write_text_file",
                        [extism::PTR],
                        [extism::PTR],
                        // TODO: should this be node_path?
                        // these plugins are still defined cluster-wide...
                        UserData::new(cluster_path.to_owned()),
                        write_text_file,
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

        fn as_lblp_plugin_mut(&mut self) -> &mut dyn LBLPPlugin {
            self
        }
    }

    impl ClusterProcessingPlugin {
        pub fn process_cluster(
            &mut self,
            cluster_path: &Path,
        ) -> anyhow::Result<HashSet<domain::ArtifactMapping>> {
            // TODO: create payload(s), invoke plugin function, deal with result
            // see node processing counterpart
            // dbg!("Might make sense to pass Cluster struct, but problem is that that has loaded plugins.");
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
                let joined_path = cluster_path.join(&unloaded_plugin.path);
                let url = Wasm::file(&joined_path);
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
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        write_text_file,
                    )
                    .with_function(
                        "read_binary_file_base64",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        read_binary_file_base64,
                    )
                    .with_function(
                        "read_text_file",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        read_text_file,
                    )
                    .with_function(
                        "get_system_time",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(()),
                        get_system_time,
                    )
                    .with_function(
                        "get_last_modification_time",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        get_last_modification_time,
                    )
                    .with_function(
                        "get_cluster_structure",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        get_cluster_structure,
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

    pub fn load_pre_archive_plugins(
        unloaded_plugins: Vec<domain::UnloadedPlugin>,
        cluster_path: &PathBuf,
    ) -> Vec<anyhow::Result<PreArchivePlugin>> {
        unloaded_plugins
            .into_iter()
            .map(|unloaded_plugin| {
                let joined_path = cluster_path.join(&unloaded_plugin.path);
                let url = Wasm::file(&joined_path);
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
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        write_text_file,
                    )
                    .with_function(
                        "read_binary_file_base64",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        read_binary_file_base64,
                    )
                    .with_function(
                        "write_binary_file_base64",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        write_binary_file_base64,
                    )
                    .with_function(
                        "read_text_file",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        read_text_file,
                    )
                    .with_function(
                        "get_system_time",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(()),
                        get_system_time,
                    )
                    .with_function(
                        "get_last_modification_time",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        get_last_modification_time,
                    )
                    .with_function(
                        "get_cluster_structure",
                        [extism::PTR],
                        [extism::PTR],
                        UserData::new(cluster_path.to_owned()),
                        get_cluster_structure,
                    )
                    .build();
                plugin.map(|plugin| PreArchivePlugin {
                    extism_plugin: plugin,
                    parameter_values: unloaded_plugin.parameters,
                    path: unloaded_plugin.path,
                })
            })
            .collect()
    }
}
