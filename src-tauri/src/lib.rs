pub mod plugins {
    use libloading::{Library, Symbol};
    use serde_yaml::Value;
    use std::collections::VecDeque;
    use std::fmt;
    use std::fmt::Debug;
    use std::ops::Deref;
    use std::path::Path;
    use paste;

    pub trait Plugin {
        fn get_name(&self) -> &str;
        fn get_version(&self) -> &str;
    }

    pub trait NodeProcessingPlugin: Plugin {
        // TODO: consider dropping this method
        // could arguably signal inability to do this via distinct error
        fn can_process_extension_field(&self, field_name: &str) -> bool;
        fn process_extension_field(
            &self,
            cluster_path: &Path,
            node_id: &str,
            field_name: &str,
            value: &Value,
            remarks: &mut Vec<String>,
        );
    }

    pub trait ClusterProcessingPlugin: Plugin {
        fn process_cluster(&self, cluster_path: &Path);
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

    macro_rules! add_plugin_boilerplate {
        ($plugin_trait:ident) => {
            paste::paste! {
                define_plugin_container!($plugin_trait);
                implement_debug_for_container!($plugin_trait);
                implement_deref_for_container!($plugin_trait);
            }
        };
    }

    add_plugin_boilerplate!(NodeProcessingPlugin);
    add_plugin_boilerplate!(ClusterProcessingPlugin);

    // Q: why VecDeque, specifically?
    pub fn load_node_processing_plugins(
        paths: Vec<String>,
    ) -> VecDeque<NodeProcessingPluginContainer> {
        let mut plugins = VecDeque::new();
        paths.iter().for_each(|path| unsafe {
            let lib = Library::new(path).expect("Failed to load library");
            let constructor: Symbol<unsafe extern "C" fn() -> *mut dyn NodeProcessingPlugin> =
                lib.get(b"create_plugin").expect("Failed to find symbol");
            let plugin = constructor();
            plugins.push_back(NodeProcessingPluginContainer {
                plugin: Box::from_raw(plugin),
                _lib: lib,
            });
        });
        plugins
    }

    pub fn load_cluster_processing_plugins(
        paths: Vec<String>,
    ) -> VecDeque<ClusterProcessingPluginContainer> {
        let mut plugins = VecDeque::new();
        paths.iter().for_each(|path| unsafe {
            let lib = Library::new(path).expect("Failed to load library");
            let constructor: Symbol<unsafe extern "C" fn() -> *mut dyn ClusterProcessingPlugin> =
                lib.get(b"create_plugin").expect("Failed to find symbol");
            let plugin = constructor();
            plugins.push_back(ClusterProcessingPluginContainer {
                plugin: Box::from_raw(plugin),
                _lib: lib,
            });
        });
        plugins
    }
}
