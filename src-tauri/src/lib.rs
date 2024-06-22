pub mod plugins {
    use serde_yaml::Value;
    use libloading::{Library, Symbol};
    use std::collections::VecDeque;
    use std::path::Path;
    use std::fmt;
    use std::ops::Deref;

    pub trait Plugin {
        fn get_name(&self) -> &str;
        fn get_version(&self) -> &str;
    }

    pub trait NodeProcessingPlugin : Plugin {
        // TODO: consider dropping this method
        // could arguably signal inability to do this via distinct error
        fn can_process_extension_field(&self, field_name: &str) -> bool;
        fn process_extension_field(&self, cluster_path: &Path, node_id: &str, field_name: &str, value: &Value, remarks: &mut Vec<String>);
    }

    pub trait ClusterProcessingPlugin : Plugin {
        fn process_cluster(&self, cluster_path: &Path);
    }

    pub struct NodeProcessingPluginContainer {
        // TODO: can I implement Deref or something so methods can be called directly on the
        // container? Think so.
        pub plugin: Box<dyn NodeProcessingPlugin>,
        _lib: Library, // Holds the library to ensure it lives as long as the plugin
    }

    pub struct ClusterProcessingPluginContainer {
        // TODO: see above
        pub plugin: Box<dyn ClusterProcessingPlugin>,
        _lib: Library,
    }

    impl fmt::Debug for NodeProcessingPluginContainer {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("NodeProcessingPluginContainer")
                .field("plugin_name", &self.plugin.get_name())
                .field("plugin_version", &self.plugin.get_version())
                .finish()
        }
    }

    impl fmt::Debug for ClusterProcessingPluginContainer {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("ClusterProcessingPluginContainer")
                .field("plugin_name", &self.plugin.get_name())
                .field("plugin_version", &self.plugin.get_version())
                .finish()
        }
    }

    impl Deref for NodeProcessingPluginContainer {
        type Target = Box<dyn NodeProcessingPlugin>;
        
        fn deref(&self) -> &Self::Target {
            &self.plugin
        }
    }

    impl Deref for ClusterProcessingPluginContainer {
        type Target = Box<dyn ClusterProcessingPlugin>;
        
        fn deref(&self) -> &Self::Target {
            &self.plugin
        }
    }



    // Q: why VecDeque, specifically?
    pub fn load_node_processing_plugins(paths: Vec<String>) -> VecDeque<NodeProcessingPluginContainer> {
        let mut plugins = VecDeque::new();
        paths.iter().for_each(|path| {
        unsafe {
            let lib = Library::new(path)
                          .expect("Failed to load library");
            let constructor: Symbol<unsafe extern "C" fn() -> *mut dyn NodeProcessingPlugin> =
                lib.get(b"create_plugin").expect("Failed to find symbol");
            let plugin = constructor();
            plugins.push_back(NodeProcessingPluginContainer {
                plugin: Box::from_raw(plugin),
                _lib: lib,
            });
        }});
        plugins
    }

    pub fn load_cluster_processing_plugins(paths: Vec<String>) -> VecDeque<ClusterProcessingPluginContainer> {
        let mut plugins = VecDeque::new();
        paths.iter().for_each(|path| {
        unsafe {
            let lib = Library::new(path)
                          .expect("Failed to load library");
            let constructor: Symbol<unsafe extern "C" fn() -> *mut dyn ClusterProcessingPlugin> =
                lib.get(b"create_plugin").expect("Failed to find symbol");
            let plugin = constructor();
            plugins.push_back(ClusterProcessingPluginContainer {
                plugin: Box::from_raw(plugin),
                _lib: lib,
            });
        }});
        plugins
    }

}
