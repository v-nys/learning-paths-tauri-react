use std::path::Path;

pub struct RealFileReader;

pub trait FileReader {
    fn read_to_string(&mut self, path: &Path) -> std::io::Result<String>;
}

impl FileReader for RealFileReader {
    fn read_to_string(&mut self, path: &Path) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
}

pub struct MockFileReader<'a> {
    // TODO: getters
    pub paths: Vec<&'a Path>,
    pub calls_made: usize,
}

impl<'a> FileReader for MockFileReader<'a> {
    fn read_to_string(&mut self, _path: &Path) -> std::io::Result<String> {
        let path_option = self.paths.get(self.calls_made);
        self.calls_made += 1;
        match path_option {
            Some(p) => std::fs::read_to_string(p),
            None => panic!("Incorrect use of mock object"),
        }
    }
}

impl<'a> MockFileReader<'a> {
    // pub fn new(paths: Vec<&'a Path>) -> Self {
    //     Self {
    //         paths,
    //         calls_made: 0,
    //     }
    // }
}
