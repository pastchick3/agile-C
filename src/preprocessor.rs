use std::fs;

use regex::Regex;

use crate::structure::{Error, Location};

pub struct Preprocessor<'a> {
    source: &'a str,
    errors: Option<Vec<Error>>,
    include_regex: Regex,
}

impl<'a> Preprocessor<'a> {
    pub fn new(source: &'a str, errors: Vec<Error>) -> Preprocessor {
        Preprocessor {
            source,
            errors: Some(errors),
            include_regex: Regex::new("^#include[[:space:]]+(\"|<)(.+)(\"|>)").unwrap(),
        }
    }

    pub fn run(&mut self) -> (String, Vec<Error>) {
        let mut lines: Vec<_> = self.source.lines().map(|line| line.to_string()).collect();
        let mut modification = true;
        while modification {
            modification = false;
            lines = lines
                .into_iter()
                .map(|line| {
                    if let Some(caps) = self.include_regex.captures(&line) {
                        modification = true;
                        let left_delimiter = caps.get(1).unwrap().as_str();
                        let filename = caps.get(2).unwrap().as_str();
                        let right_delimiter = caps.get(3).unwrap().as_str();
                        if left_delimiter != right_delimiter {
                            self.push_error("Expect `\"filename\"` or `<filename>`.");
                            vec!["\n".to_string()]
                        } else {
                            match fs::read_to_string(filename) {
                                Ok(included_source) => included_source
                                    .lines()
                                    .map(|line| line.to_string())
                                    .collect::<Vec<_>>(),
                                Err(err) => {
                                    let message =
                                        format!("Fail to read the included file: {}.", err);
                                    self.push_error(&message);
                                    vec!["\n".to_string()]
                                }
                            }
                        }
                    } else {
                        vec![line]
                    }
                })
                .flatten()
                .collect();
        }
        println!("{:#?}", lines.join("\n"));
        (lines.join("\n"), self.errors.take().unwrap())
    }

    fn push_error(&mut self, message: &str) {
        let location = Location::empty();
        self.errors.as_mut().unwrap().push(Error::Preprocessing {
            message: message.to_string(),
            location,
        });
    }
}
