pub mod plugins {
    use libloading::{Library, Symbol};
    use std::collections::VecDeque;
    pub trait Plugin {
        fn get_name(&self) -> &str;
        fn get_version(&self) -> &str;
        fn can_process_extension_field(&self, field_name: &str) -> bool;
    }

    pub struct PluginContainer {
        pub plugin: Box<dyn Plugin>,
        _lib: Library, // Holds the library to ensure it lives as long as the plugin
    }

    // Q: why VecDeque, specifically?
    pub fn load_plugins() -> VecDeque<PluginContainer> {
        let mut plugins = VecDeque::new();
        /*TODO: have plugins declared somewhere that makes sense
        clusters can assume the use of specific plugins (name (possibly with alias) and exact version) in specific order?
        so each cluster should declare its own list of plugins, and this should work on a per-cluster basis (but there are "whole-course" plugins, too, which should be run later) */
        unsafe {
            let lib = Library::new("/home/vincentn/Projects/poc-assignments-plugin/target/release/libpoc_assignments_plugin.so")
                          .expect("Failed to load library");
            let constructor: Symbol<unsafe extern "C" fn() -> *mut dyn Plugin> =
                lib.get(b"create_plugin").expect("Failed to find symbol");
            let plugin = constructor();
            plugins.push_back(PluginContainer {
                plugin: Box::from_raw(plugin),
                _lib: lib,
            });
        }
        plugins
    }
}
