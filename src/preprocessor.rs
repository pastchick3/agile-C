use std::fs;

use regex::Regex;

use crate::cstdlib;
use crate::structure::{Error, Location};

pub struct Preprocessor<'a> {
    file_name: &'a str,
    source: &'a str,
    errors: &'a mut Vec<Error>,
    include_regex: Regex,
    define_regex: Regex,
    define_pairs: Vec<(Regex, String)>,
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
            define_regex: Regex::new(
                "^[[:space:]]*#define[[:space:]]+([[:word:]]+)[[:space:]]+([[:word:]]+)",
            )
            .unwrap(),
            define_pairs: Vec::new(),
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
        lines = lines
            .into_iter()
            .filter_map(|line| self.define(line))
            .collect();
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
            if left_delimiter == "<" && right_delimiter == ">" {
                match cstdlib::get_library(included_file_name) {
                    Some(source) => source
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
                self.push_error(
                    "Expect `\"file_name\"` or `<file_name>`.",
                    Location::new(&file_name, line_index, 0),
                );
                Vec::new()
            }
        } else {
            vec![(file_name, line_index, line)]
        }
    }

    fn define(
        &mut self,
        (file_name, line_index, line): (String, usize, String),
    ) -> Option<(String, usize, String)> {
        if let Some(caps) = self.define_regex.captures(&line) {
            let name = caps.get(1).unwrap().as_str();
            let value = caps.get(2).unwrap().as_str();
            let name_regex = match Regex::new(&format!("([[:space:]])({})([[:space:]]?)", name)) {
                Ok(name_regex) => name_regex,
                Err(err) => {
                    self.push_error(
                        &format!("Invalid #define name: {}.", err),
                        Location::new(&file_name, line_index, 0),
                    );
                    return None;
                }
            };
            let replace_value = format!("${{1}}{}${{3}}", value);
            self.define_pairs.push((name_regex, replace_value));
            None
        } else {
            let line = self
                .define_pairs
                .iter()
                .fold(line, |line, (name_regex, replace_value)| {
                    name_regex
                        .replace_all(&line, replace_value.as_str())
                        .to_string()
                });
            Some((file_name, line_index, line))
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
    fn include_cstdlib() {
        let source = "#include <_test>";
        let expected_errors = vec![];
        let expected_lines = vec![("_test".to_string(), 0, "_test".to_string())];
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

    #[test]
    fn define() {
        let source = "\t#define N 100\nint a_N =\tN;";
        let expected_errors = vec![];
        let expected_lines = vec![("file".to_string(), 1, "int a_N =\t100;".to_string())];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(lines, expected_lines);
    }
}
