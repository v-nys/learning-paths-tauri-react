pub mod domain;
pub mod deserialization;

pub mod prelude {
    pub use serde_yaml;
    pub use serde_json;
    pub use schemars;
    pub use anyhow;
}

pub mod plugins {
    use crate::domain;
    use serde_yaml::Value;
    use std::collections::{HashMap, HashSet};
    use std::fmt::Debug;
    use std::path::{Path, PathBuf};

    #[derive(PartialEq, Eq, Hash, Debug, Clone)]
    pub struct ArtifactMapping {
        pub local_file: PathBuf,
        pub root_relative_target_dir: PathBuf,
    }

    pub trait Plugin {
        fn get_name(&self) -> &str;
        fn get_version(&self) -> &str;
        fn set_params(&mut self, params: HashMap<String, Value>) -> Result<(), String>;
        fn set_path(&mut self, path: String);
        fn get_path(&self) -> &String;
        // bool is to indicate whether the property is required
        fn get_params_schema(&self) -> HashMap<(String, bool), serde_json::Value>;
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

    pub trait NodeProcessingPlugin: Plugin + Send + Sync {

        fn process_extension_field(
            &self,
            cluster_path: &Path,
            node: &domain::Node,
            field_name: &str,
            value: &Value,
        ) -> Result<HashSet<ArtifactMapping>, NodeProcessingError>;

        fn get_extension_field_schema(&self) -> HashMap<(String, bool), serde_json::Value>;
    }

    pub trait ClusterProcessingPlugin: Plugin + Send + Sync {
        fn process_cluster(
            &self,
            cluster_path: &Path,
            cluster: &domain::Cluster
        ) -> Result<HashSet<ArtifactMapping>, anyhow::Error>;
    }

    pub trait PreZipPlugin: Plugin + Send + Sync {
        fn process_project(
            &self,
            cluster_paths: Vec<&Path>,
            artifacts: &mut HashSet<ArtifactMapping>,
        ) -> Result<(), anyhow::Error>;
    }

}
