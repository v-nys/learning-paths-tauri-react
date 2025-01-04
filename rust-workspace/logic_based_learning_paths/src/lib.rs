pub mod deserialization;
pub mod domain;

pub mod prelude {
    pub use anyhow;
    pub use schemars;
    pub use serde_json;
    pub use serde_yaml;
}

pub mod plugins {
    use crate::domain;
    use extism::{Manifest, Plugin, Wasm};
    pub fn load_node_processing_plugins(
        _unloaded_plugins: Vec<domain::UnloadedPlugin>,
    ) -> Vec<Plugin> {
        let url = Wasm::url(
            "https://github.com/extism/plugins/releases/latest/download/count_vowels.wasm",
        );
        let manifest = Manifest::new([url]);
        // FIXME: rather than returning a Vec<extism::Plugin>
        // probably want some kind of wrapper, containing it and the arguments
        let plugin = Plugin::new(&manifest, [], true).unwrap();
        vec![plugin]
    }
}
