use std::fs;

use regex::Regex;

use crate::structure::{Error, Location};

pub struct Preprocessor<'a> {
    file_name: &'a str,
    source: &'a str,
    errors: &'a mut Vec<Error>,
    include_regex: Regex,
}

impl<'a> Preprocessor<'a> {
    pub fn new(
        file_name: &'a str,
        source: &'a str,
        errors: &'a mut Vec<Error>,
    ) -> Preprocessor<'a> {
        Preprocessor {
            file_name,
            source,
            errors,
            include_regex: Regex::new("^[[:space:]]*#include[[:space:]]+(\"|<)(.+)(\"|>)").unwrap(),
        }
    }

    pub fn run(&mut self) -> Result<Vec<(String, usize, String)>, ()> {
        let mut lines: Vec<_> = self
            .source
            .lines()
            .enumerate()
            .map(|(line_index, line)| (self.file_name.to_string(), line_index, line.to_string()))
            .collect();
        let mut modification = true;
        while modification {
            modification = false;
            lines = lines
                .into_iter()
                .map(|line| self.include(&mut modification, line))
                .flatten()
                .collect();
        }
        if self.errors.is_empty() {
            Ok(lines)
        } else {
            Err(())
        }
    }

    fn push_error(&mut self, message: &str, location: Location) {
        self.errors.push(Error::Preprocessing {
            message: message.to_string(),
            location,
        });
    }

    fn include(
        &mut self,
        modification: &mut bool,
        (file_name, line_index, line): (String, usize, String),
    ) -> Vec<(String, usize, String)> {
        if let Some(caps) = self.include_regex.captures(&line) {
            *modification = true;
            let left_delimiter = caps.get(1).unwrap().as_str();
            let included_file_name = caps.get(2).unwrap().as_str();
            let right_delimiter = caps.get(3).unwrap().as_str();
            if left_delimiter != right_delimiter {
                self.push_error(
                    "Expect `\"file_name\"` or `<file_name>`.",
                    Location::new(&file_name, line_index, 0),
                );
                vec![(file_name, line_index, String::new())]
            } else {
                match fs::read_to_string(included_file_name) {
                    Ok(included_source) => included_source
                        .lines()
                        .enumerate()
                        .map(|(included_line_index, line)| {
                            (
                                included_file_name.to_string(),
                                included_line_index,
                                line.to_string(),
                            )
                        })
                        .collect::<Vec<_>>(),
                    Err(err) => {
                        let message = format!("Fail to read the included file: {}.", err);
                        self.push_error(&message, Location::new(&file_name, line_index, 0));
                        vec![(file_name, line_index, String::new())]
                    }
                }
            }
        } else {
            vec![(file_name, line_index, line)]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn include() {
        let mut file_2 = NamedTempFile::new().expect("Unable to create file_2.");
        let path_2 = format!("{}", file_2.path().display());
        let mut file_3 = NamedTempFile::new().expect("Unable to create file_3.");
        let path_3 = format!("{}", file_3.path().display());
        write!(
            file_2,
            "file_2 start\n#include \"{}\"\nfile_2 end\n",
            path_3
        )
        .expect("Unable to write to file_2.");
        write!(file_3, "file_3 start\nfile_3 end").expect("Unable to write to file_3.");
        let source_1 = format!("file_1 start\n#include \"{}\"\nfile_1 end", path_2);
        let expected_errors = vec![];
        let expected_lines = vec![
            ("file_1".to_string(), 0, "file_1 start".to_string()),
            (path_2.clone(), 0, "file_2 start".to_string()),
            (path_3.clone(), 0, "file_3 start".to_string()),
            (path_3.clone(), 1, "file_3 end".to_string()),
            (path_2.clone(), 2, "file_2 end".to_string()),
            ("file_1".to_string(), 2, "file_1 end".to_string()),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file_1", &source_1, &mut errors)
            .run()
            .expect("Fail to include.");
        assert_eq!(errors, expected_errors);
        assert_eq!(lines, expected_lines);
    }

    #[test]
    fn unbalanced_include() {
        let source = "#include \"lib>";
        let expected_errors = vec![Error::Preprocessing {
            message: "Expect `\"file_name\"` or `<file_name>`.".to_string(),
            location: Location::default(),
        }];
        let mut errors = Vec::new();
        Preprocessor::new("source", source, &mut errors)
            .run()
            .expect_err("Succeed to include.");
        assert_eq!(errors, expected_errors);
    }
}
