pub mod domain;

pub mod plugins {
    use crate::domain;
    use libloading::{Library, Symbol};
    use paste;
    use serde_yaml::Value;
    use std::collections::VecDeque;
    use std::collections::{HashMap, HashSet};
    use std::fmt;
    use std::fmt::Debug;
    use std::ops::Deref;
    use std::path::{Path, PathBuf};

    #[derive(PartialEq, Eq, Hash, Debug)]
    pub struct ArtifactMapping {
        pub local_file: PathBuf,
        pub root_relative_target_dir: PathBuf,
    }

    pub trait Plugin {
        fn get_name(&self) -> &str;
        fn get_version(&self) -> &str;
        fn set_params(&mut self, params: HashMap<String, Value>) -> anyhow::Result<()>;
    }

    #[derive(Debug)]
    pub enum NodeProcessingError {
        CannotProcessFieldType,
        Remarks(Vec<String>),
    }

    impl NodeProcessingError {
        // this is here because I cannot derive Eq on NodeProcessingError
        pub fn indicates_inability_to_process_field(&self) -> bool {
            match self {
                Self::CannotProcessFieldType => true,
                _ => false,
            }
        }
    }

    pub trait NodeProcessingPlugin: Plugin {
        fn process_extension_field(
            &self,
            cluster_path: &Path,
            node: &domain::Node,
            field_name: &str,
            value: &Value,
        ) -> Result<HashSet<ArtifactMapping>, NodeProcessingError>;
        fn get_mandatory_fields(&self) -> HashSet<String>;
    }

    pub trait ClusterProcessingPlugin: Plugin {
        fn process_cluster(
            &self,
            cluster_path: &Path,
        ) -> Result<HashSet<ArtifactMapping>, anyhow::Error>;
    }

    pub trait PreZipPlugin: Plugin {
        fn process_project(
            &self,
            cluster_paths: Vec<&Path>,
        ) -> Result<HashSet<ArtifactMapping>, anyhow::Error>;
    }

    macro_rules! define_plugin_container {
        ($plugin_trait:ident) => {
            paste::paste! {
                pub struct [<$plugin_trait Container>] {
                    plugin: Box<dyn $plugin_trait>,
                    _lib: Library // Holds the library to ensure it lives as long as the plugin
                }
            }
        };
    }

    macro_rules! implement_debug_for_container {
        ($plugin_trait:ident) => {
            paste::paste! {
                impl Debug for [<$plugin_trait Container>] {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.debug_struct("[<$plugin_trait Container>]")
                        .field("plugin_name", &self.plugin.get_name())
                        .field("plugin_version", &self.plugin.get_version())
                        .finish()
                    }
                }
            }
        };
    }

    macro_rules! implement_deref_for_container {
        ($plugin_trait:ident) => {
            paste::paste! {
                    impl Deref for [<$plugin_trait Container>] {
                        type Target = Box<dyn $plugin_trait>;

            fn deref(&self) -> &Self::Target {
                &self.plugin
            }

                    }
                }
        };
    }

    macro_rules! define_load_function {
        ($plugin_trait:ident, $load_function_name:ident) => {
            paste::paste! {
                // Q: why VecDeque, specifically?
                pub fn $load_function_name(
                    unloaded_plugins: Vec<domain::UnloadedPlugin>,
                ) -> anyhow::Result<VecDeque<[<$plugin_trait Container>]>> {
                    let mut plugins = VecDeque::new();
                    for domain::UnloadedPlugin { path, parameters } in unloaded_plugins {
                        unsafe {
                            let lib = Library::new(path)?;
                            let constructor: Symbol<unsafe extern "C" fn() -> *mut dyn $plugin_trait> =
                                lib.get(b"create_plugin")?; // failed to find symbol
                            let mut plugin = Box::from_raw(constructor());
                            plugin.set_params(parameters)?;
                            plugins.push_back([<$plugin_trait Container>] {
                                plugin,
                                _lib: lib,
                            });
                        }
                    }
                   Ok(plugins)
                }
            }
        };
    }

    macro_rules! add_plugin_boilerplate {
        // TODO: could do without second parameter via proc macro
        ($plugin_trait:ident, $load_function_name:ident) => {
            paste::paste! {
                define_plugin_container!($plugin_trait);
                implement_debug_for_container!($plugin_trait);
                implement_deref_for_container!($plugin_trait);
                define_load_function!($plugin_trait,$load_function_name);
            }
        };
    }

    add_plugin_boilerplate!(NodeProcessingPlugin, load_node_processing_plugins);
    add_plugin_boilerplate!(ClusterProcessingPlugin, load_cluster_processing_plugins);
    add_plugin_boilerplate!(PreZipPlugin, load_pre_zip_plugins);
}
