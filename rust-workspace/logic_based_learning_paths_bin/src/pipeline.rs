use anyhow;
use crate::readers;

use std::path::PathBuf;

use logic_based_learning_paths_bin::domain::UnpopulatedCluster;
use logic_based_learning_paths_bin::deserialization;

#[derive(Debug)]
/// The result of reading a Path, along with that Path.
struct ReadResultForPath(Result<String, std::io::Error>, PathBuf);

pub struct Pipeline<T> {
    state: T,
}

#[derive(Default)]
pub struct NoDataLoaded {}

impl Pipeline<NoDataLoaded> {
    pub fn new() -> Pipeline<NoDataLoaded> {
        Pipeline {
            state: NoDataLoaded::default(),
        }
    }
}

#[derive(Debug)]
pub struct UnpopulatedClusterWithMetadata {
    cluster_path: PathBuf,
    unpopulated_cluster: UnpopulatedCluster,
    contents_file_contents: String,
}

#[derive(Debug)]
pub struct UnpopulatedClusterResultWithMetadata {
    cluster_path: PathBuf,
    unpopulated_cluster_with_contents_file_contents: anyhow::Result<(UnpopulatedCluster, String)>,
}

pub enum UnpopulatedClustersResult {
    ZeroIssues(Vec<UnpopulatedClusterWithMetadata>),
    Issues(Vec<UnpopulatedClusterResultWithMetadata>),
}

impl Pipeline<NoDataLoaded> {

    pub fn load_unpopulated_clusters<'a, T: readers::FileReader>(
        self,
        paths: &'a str,
        reader: &mut T,
    ) -> Pipeline<UnpopulatedClustersResult> {
        let paths = paths.split(";").map(|p| PathBuf::from(p));
        let read_results = paths.clone().map(|p| {
            let yaml_location = p.join("contents.lc.yaml");
            ReadResultForPath(reader.read_to_string(yaml_location.as_path()), p)
        });
        let read_results = read_results
            .map(|ReadResultForPath(r, p)| {
                (
                    p,
                    match r {
                        Ok(ref text) => serde_yaml::from_str::<
                            deserialization::UnpopulatedClusterForSerialization,
                        >(text)
                        .map(|ucfs| (ucfs, text.to_owned()))
                        .map_err(anyhow::Error::new),
                        Err(e) => Err(anyhow::Error::new(e)),
                    },
                )
            })
            .map(|(p, res)| UnpopulatedClusterResultWithMetadata {
                cluster_path: p.clone(),
                unpopulated_cluster_with_contents_file_contents: res
                    .and_then(|(ucfs, text)| ucfs.build(&p).map(|uc| (uc, text))),
            })
            .collect::<Vec<_>>();
        if read_results.iter().all(
            |UnpopulatedClusterResultWithMetadata {
                 unpopulated_cluster_with_contents_file_contents,
                 ..
             }| unpopulated_cluster_with_contents_file_contents.is_ok(),
        ) {
            let total_result = read_results
                .into_iter()
                .map(
                    |UnpopulatedClusterResultWithMetadata {
                         cluster_path,
                         unpopulated_cluster_with_contents_file_contents,
                     }| {
                        let tup = unpopulated_cluster_with_contents_file_contents
                            .expect("Just checked this via .all.");
                        UnpopulatedClusterWithMetadata {
                            cluster_path,
                            unpopulated_cluster: tup.0,
                            contents_file_contents: tup.1,
                        }
                    },
                )
                .collect();
            Pipeline {
                state: UnpopulatedClustersResult::ZeroIssues(total_result),
            }
        } else {
            Pipeline {
                state: UnpopulatedClustersResult::Issues(read_results),
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::readers;
    use super::{Pipeline, UnpopulatedClustersResult};

    #[test]
    fn simple_unpopulated_clusters() {
        let mut reader = readers::RealFileReader {};
        let base_path = std::fs::canonicalize(
            PathBuf::from("tests/pipeline-tests/loading-of-unpopulated-clusters/simple").as_path(),
        );
        let base_path = base_path.expect("If this panics, the test fails, which is fine.");
        let cluster_1_path = base_path
            .join("simpleproject")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let cluster_2_path = base_path
            .join("technicalinfo")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let combined_paths = vec![cluster_1_path, cluster_2_path].join(";");
        // not sure if there is all that much to test for this scenario
        // there are no plugins involved
        let pipeline = Pipeline::new().load_unpopulated_clusters(&combined_paths, &mut reader);
        match pipeline.state {
            UnpopulatedClustersResult::ZeroIssues(_) => {}
            UnpopulatedClustersResult::Issues(issues) => {
                dbg!(issues);
                panic!("Unpopulated clusters have issues when they shouldn't.")
            }
        }
    }

    #[test]
    fn unpopulated_clusters_with_noop_plugins() {
        let mut reader = readers::RealFileReader {};
        let base_path = std::fs::canonicalize(
            PathBuf::from("tests/pipeline-tests/loading-of-unpopulated-clusters/with-noop-plugins")
                .as_path(),
        );
        let base_path = base_path.expect("If this panics, the test fails, which is fine.");
        let cluster_1_path = base_path
            .join("simpleproject")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let cluster_2_path = base_path
            .join("technicalinfo")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let combined_paths = vec![cluster_1_path, cluster_2_path].join(";");
        let pipeline = Pipeline::new().load_unpopulated_clusters(&combined_paths, &mut reader);
        match pipeline.state {
            UnpopulatedClustersResult::ZeroIssues(ucwms) => {
                assert!(ucwms.len() == 2);
                let simpleproject_cluster = &ucwms[0].unpopulated_cluster;
                let technicalinfo_cluster = &ucwms[1].unpopulated_cluster;
                assert!(simpleproject_cluster.pre_node_node_plugins.len() == 1);
                assert!(simpleproject_cluster.post_node_node_plugins.len() == 1);
                assert!(simpleproject_cluster.post_node_cluster_plugins.len() == 0);
                assert!(simpleproject_cluster.post_merge_node_plugins.len() == 1);
                assert!(simpleproject_cluster.post_merge_cluster_plugins.len() == 0);
                assert!(simpleproject_cluster
                    .pre_archive_plugins
                    .as_ref()
                    .is_some_and(|ps| ps.len() == 1));
                assert!(technicalinfo_cluster.pre_node_node_plugins.len() == 0);
                assert!(technicalinfo_cluster.post_node_cluster_plugins.len() == 1);
                assert!(technicalinfo_cluster.post_merge_cluster_plugins.len() == 1);
                assert!(technicalinfo_cluster.pre_archive_plugins.is_none());
            }
            UnpopulatedClustersResult::Issues(issues) => {
                dbg!(issues);
                panic!("Unpopulated clusters have issues when they shouldn't.")
            }
        }
    }

    #[test]
    fn unpopulated_clusters_with_missing_plugins() {
        let mut reader = readers::RealFileReader {};
        let base_path = std::fs::canonicalize(
            PathBuf::from(
                "tests/pipeline-tests/loading-of-unpopulated-clusters/with-missing-plugins",
            )
            .as_path(),
        );
        let base_path = base_path.expect("If this panics, the test fails, which is fine.");
        let cluster_1_path = base_path
            .join("simpleproject")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let cluster_2_path = base_path
            .join("technicalinfo")
            .to_str()
            .expect("If this panics, the test fails, which is fine.")
            .to_owned();
        let combined_paths = vec![cluster_1_path, cluster_2_path].join(";");
        let pipeline = Pipeline::new().load_unpopulated_clusters(&combined_paths, &mut reader);
        match pipeline.state {
            UnpopulatedClustersResult::ZeroIssues(_) => {
                panic!("Missing plugins should cause an issue but are not doing so.")
            }
            UnpopulatedClustersResult::Issues(issues) => {
                assert!(issues.len() == 2);
                assert!(format!("{:#?}", issues[0]).contains("Unable to load Wasm file"));
                assert!(format!("{:#?}", issues[1]).contains("Unable to load Wasm file"));
            }
        }
    }
}
