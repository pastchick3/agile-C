use std::collections::HashSet;
use std::fs;

use regex::Regex;

use crate::cstdlib;
use crate::structure::{Error, Location};

/// The preprocessor breaks a source into a vector of lines and expand `#include` properly.
pub struct Preprocessor<'a> {
    file_name: &'a str,
    source: &'a str,
    errors: &'a mut Vec<Error>,
    include_regex: Regex, // regular expression that matches `#include`
    included: HashSet<(String, usize)>, // `#include <*>` that has been processed
}

impl<'a> Preprocessor<'a> {
    pub fn new(file_name: &'a str, source: &'a str, errors: &'a mut Vec<Error>) -> Self {
        Preprocessor {
            file_name,
            source,
            errors,
            include_regex: Regex::new("^\\s*#include\\s+(\"|<)(.+)(\"|>)").unwrap(),
            included: HashSet::new(),
        }
    }

    /// Break the source string into a triple (file_name, line_index, line).
    pub fn run(&mut self) -> Result<Vec<(String, usize, String)>, ()> {
        // Break the input file into lines.
        let mut lines: Vec<_> = self
            .source
            .lines()
            .enumerate()
            .map(|(line_index, line)| (self.file_name.to_string(), line_index, line.to_string()))
            .collect();

        // Recursively include all required files.
        let mut modification = true;
        while modification {
            modification = false;
            lines = lines
                .into_iter()
                .map(|line| self.include(&mut modification, line))
                .flatten()
                .collect();
        }

        // Return the result.
        if self.errors.is_empty() {
            Ok(lines)
        } else {
            Err(())
        }
    }

    /// A helper function to construct preprocessing errors.
    fn push_error(&mut self, message: &str, location: Location) {
        self.errors.push(Error::Preprocessing {
            message: message.to_string(),
            location,
        });
    }

    /// Check if a line contains "#include" directive. If so, expand the
    /// included file into a vector of new lines. If not, wrap the original
    /// line into a vector to keep a uniform return type.
    fn include(
        &mut self,
        modification: &mut bool,
        (file_name, line_index, line): (String, usize, String),
    ) -> Vec<(String, usize, String)> {
        if self.included.contains(&(file_name.clone(), line_index)) {
            // Return a line that has been included without modifications.
            vec![(file_name, line_index, line)]
        } else if let Some(caps) = self.include_regex.captures(&line) {
            *modification = true;
            // Extract the file name.
            let left_delimiter = caps.get(1).unwrap().as_str();
            let included_file_name = caps.get(2).unwrap().as_str();
            let right_delimiter = caps.get(3).unwrap().as_str();
            if left_delimiter == "<" && right_delimiter == ">" {
                // Include a C standard library.
                // Also notice we do not remove the original line.
                match cstdlib::get_library(included_file_name) {
                    Some(source) => {
                        self.included.insert((file_name.clone(), line_index));
                        let mut lines = vec![(file_name, line_index, line.to_string())];
                        let mut extended: Vec<_> = source
                            .lines()
                            .enumerate()
                            .map(|(included_line_index, line)| {
                                (
                                    included_file_name.to_string(),
                                    included_line_index,
                                    line.to_string(),
                                )
                            })
                            .collect();
                        lines.append(&mut extended);
                        lines
                    }
                    None => {
                        let message = format!(
                            "The standard library `{}` is not available.",
                            included_file_name
                        );
                        self.push_error(&message, Location::new(&file_name, line_index, 0));
                        Vec::new()
                    }
                }
            } else if left_delimiter == "\"" && right_delimiter == "\"" {
                // Include a normal C file.
                match fs::read_to_string(included_file_name) {
                    Ok(source) => source
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
                        Vec::new()
                    }
                }
            } else {
                // Reject an unbalanced include directive.
                self.push_error(
                    "Expect `\"file_name\"` or `<file_name>`.",
                    Location::new(&file_name, line_index, 0),
                );
                Vec::new()
            }
        } else {
            // Return a normal line without modifications.
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
    fn include_userlib() {
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
        write!(file_3, "file_3 start\nfile_3 end\n").expect("Unable to write to file_3.");
        let source_1 = format!("file_1 start\n#include \"{}\"\nfile_1 end\n", path_2);
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
            .unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(lines, expected_lines);
    }

    #[test]
    fn include_cstdlib() {
        let source = "#include <_test>";
        let expected_errors = vec![];
        let expected_lines = vec![
            ("file".to_string(), 0, "#include <_test>".to_string()),
            ("_test".to_string(), 0, "//".to_string()),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
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
            .expect_err("Succeed to unbalanced_include.");
        assert_eq!(errors, expected_errors);
    }
}
