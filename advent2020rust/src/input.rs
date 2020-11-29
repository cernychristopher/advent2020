pub mod input {
    use std::path::Path;
    use std::io;
    use std::fs::File;
    use std::io::BufRead;

    fn lines_from_file(filename: impl AsRef<Path>) -> io::Result<Vec<String>> {
        io::BufReader::new(File::open(filename)?).lines().collect()
    }

    pub fn read_input(num: u32) -> Vec<String> {
        let file_name = format!("../inputs/{}", num.to_string());

        lines_from_file(file_name).expect("Cannot read from file")
    }
}

