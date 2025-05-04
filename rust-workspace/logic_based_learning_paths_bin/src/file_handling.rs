use anyhow::Context;
use std::path::{Path, PathBuf};

// TODO: rename to cluster_file_reading?

/// The result of reading a Path, along with that Path.
#[derive(Debug)]
pub struct ReadResultForPath(pub anyhow::Result<String>, pub PathBuf);

pub trait FileReader {
    fn read_to_string(&mut self, path: &Path) -> std::io::Result<String>;
}

pub struct RealFileReader;

impl FileReader for RealFileReader {
    fn read_to_string(&mut self, path: &Path) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
}

pub fn read_interpolated_yaml<T: FileReader, U: FileReader>(
    p: PathBuf,
    yaml_reader: &mut T,
    env_reader: &mut U,
) -> ReadResultForPath {
    // this feels off
    // I am computing the full path to the metadata file and the env file here
    // but I am also assuming a reader is supplied
    // will need some restructuring to make more sense
    // but cannot be done in isolation
    // need to look at the entire call chain
    let var_regex = regex::Regex::new(r"^[[:alnum:]_]+$").expect("Regex tested beforehand.");
    let interpolation_regex =
        regex::Regex::new(r"\$\{(?P<var_name>[[:alnum:]_]+)\}").expect("Regex tested beforehand.");
    let yaml_location = p.join("contents.lc.yaml");
    let env_location = p.join(".env");
    let env_variables: anyhow::Result<_> = env_reader
        .read_to_string(&env_location)
        .and_then(|s| env_file_reader::read_str(&s))
        .with_context(|| format!("Failed to read env variables at {:?}", env_location.clone()));
    let interpolated_yaml =
                env_variables.and_then(|env_variables| {
                    if let Some((k, _)) = env_variables.iter().find(|(k, _)| !var_regex.is_match(k))
                    {
                        Err(anyhow::anyhow!(format!(
                            "Invalid environment variable name: {}. Only alphanumeric characters and underscores are allowed.",
                            k
                        )))
                    } else {
                        let uninterpolated_yaml: anyhow::Result<_, _> = yaml_reader
                            .read_to_string(yaml_location.as_path())
                            .with_context(|| {
                                format!("Failed to read YAML file at {:?}.", yaml_location)
                            });
                        uninterpolated_yaml.and_then(|uninterpolated_yaml| {
                            let mut interpolated_yaml = uninterpolated_yaml.clone();
                            while let Some(captures) =
                                interpolation_regex.captures(&interpolated_yaml)
                            {
                                let var_name = &captures["var_name"];
                                if let Some(matching_value) = env_variables.get(var_name) {
                                    interpolated_yaml =
                                        interpolated_yaml.replace(&captures[0], matching_value);
                                } else {
                                    return Err(anyhow::anyhow!(
                                        "Missing binding for env variable {} in file {:?}. Variable must be defined there for interpolation to take place.",
                                        &var_name,
                                        &env_location
                                    ));
                                }
                            }
                            Ok(interpolated_yaml)
                        })
                    }
                });
    ReadResultForPath(interpolated_yaml, p)
}

#[cfg(test)]
mod tests {
    use super::{read_interpolated_yaml, FileReader, ReadResultForPath};

    use std::{
        path::{Path, PathBuf},
        str::FromStr,
    };

    struct LiteralReader {
        literal: String,
    }

    impl LiteralReader {
        fn new(literal: String) -> Self {
            Self { literal }
        }
    }

    impl FileReader for LiteralReader {
        fn read_to_string(&mut self, _: &Path) -> std::io::Result<String> {
            Ok(self.literal.clone())
        }
    }

    #[test]
    fn happy_path() {
        let raw_yaml = "foo: 1
bar:
  - ${QUUX}
  - ${BAZ}";
        let expected_yaml = "foo: 1
bar:
  - 7
  - 9";
        let env = "QUUX=7
BAZ=9";
        let p =
            PathBuf::from_str("/home/mycluster").expect("Path is not actually used, won't fail.");
        let mut yaml_reader = LiteralReader::new(raw_yaml.to_string());
        let mut env_reader = LiteralReader::new(env.to_string());
        let ReadResultForPath(result, _) =
            read_interpolated_yaml(p, &mut yaml_reader, &mut env_reader);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_yaml.to_owned());
    }
}
