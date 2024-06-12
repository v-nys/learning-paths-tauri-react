pub mod plugins {
    use libloading::{Library, Symbol};
    use std::collections::VecDeque;
    use std::fmt;
    pub trait Plugin {
        fn get_name(&self) -> &str;
        fn get_version(&self) -> &str;
        fn can_process_extension_field(&self, field_name: &str) -> bool;
    }

    pub struct PluginContainer {
        pub plugin: Box<dyn Plugin>,
        _lib: Library, // Holds the library to ensure it lives as long as the plugin
    }

    impl fmt::Debug for PluginContainer {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("PluginContainer")
                .field("plugin_name", &self.plugin.get_name())
                .field("plugin_version", &self.plugin.get_version())
                .finish()
        }
    }

    // Q: why VecDeque, specifically?
    pub fn load_plugins(paths: Vec<String>) -> VecDeque<PluginContainer> {
        let mut plugins = VecDeque::new();
        paths.iter().for_each(|path| {
        unsafe {
            let lib = Library::new(path)
                          .expect("Failed to load library");
            let constructor: Symbol<unsafe extern "C" fn() -> *mut dyn Plugin> =
                lib.get(b"create_plugin").expect("Failed to find symbol");
            let plugin = constructor();
            plugins.push_back(PluginContainer {
                plugin: Box::from_raw(plugin),
                _lib: lib,
            });
        }});
        plugins
    }
}
