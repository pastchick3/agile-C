//! A lexer producing a vector of tokens.

use regex::Regex;

use crate::structure::{Error, Location, Token};
use Token::*;

/// A lexer producing a vector of tokens for the parser.
///
/// The lexer first records the start location (`start_line_index` and`start_char_index`)
/// and then scans the source (`line_index` and `char_index`). When a token is produced,
/// the lexer will slice the source from the start position to the current position.
pub struct Lexer<'a> {
    lines: Vec<&'a str>,
    start_line_index: usize,
    line_index: usize,
    start_char_index: usize,
    char_index: usize,
    eof: bool,
    errors: Option<Vec<Error>>,
    tokens: Option<Vec<Token<'a>>>,
    raw_num_regex: Regex,
    word_regex: Regex,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, errors: Vec<Error>) -> Lexer {
        Lexer {
            lines: source.lines().collect(),
            start_line_index: 0,
            line_index: 0,
            start_char_index: 0,
            char_index: 0,
            eof: false,
            errors: Some(errors),
            tokens: Some(Vec::new()),
            raw_num_regex: Regex::new(r"\d|\.").unwrap(),
            word_regex: Regex::new(r"[[:word:]]").unwrap(),
        }
    }

    pub fn run(&mut self) -> (Vec<Token<'a>>, Vec<Error>) {
        // Skip initial empty lines.
        while self.lines[self.line_index].is_empty() {
            self.line_index += 1;
        }
        loop {
            self.skip_whitespaces();
            if self.eof {
                break;
            }
            if let Ok(tk) = self.read_token() {
                self.tokens.as_mut().unwrap().push(tk);
            }
        }
        (self.tokens.take().unwrap(), self.errors.take().unwrap())
    }

    fn skip_whitespaces(&mut self) {
        while let Some(ch) = self.get_cur_ch() {
            if ch.trim_start().is_empty() {
                self.forward();
            } else {
                break;
            }
        }
    }

    /// Notice this function will return `None` only when EOF.
    fn get_cur_ch(&self) -> Option<&'a str> {
        if self.eof {
            None
        } else {
            let line = self.lines[self.line_index];
            Some(&line[self.char_index..=self.char_index])
        }
    }

    fn get_slice(&self) -> &'a str {
        let line = self.lines[self.start_line_index];
        if self.char_index == 0 {
            &line[self.start_char_index..]
        } else {
            &line[self.start_char_index..self.char_index]
        }
    }

    fn get_location(&self) -> Location {
        Location::new(self.start_line_index + 1, self.start_char_index + 1)
    }

    /// This function will automatically go to the next line to make
    /// sure `get_cur_ch()` returns `None` only when EOF.
    fn forward(&mut self) {
        let line = self.lines[self.line_index];
        // Not at the end a line.
        if self.char_index + 1 < line.len() {
            self.char_index += 1;
        // At the end of a line.
        } else if self.line_index + 1 < self.lines.len() {
            self.line_index += 1;
            self.char_index = 0;
            // Skip empty lines.
            while self.lines[self.line_index].is_empty() {
                if self.line_index + 1 < self.lines.len() {
                    self.line_index += 1;
                } else {
                    self.eof = true;
                    break;
                }
            }
        // At EOF.
        } else {
            self.char_index += 1;
            self.eof = true;
        }
    }

    fn push_error(&mut self, message: &str) {
        let location = self.get_location();
        self.errors.as_mut().unwrap().push(Error::Lexing {
            message: message.to_string(),
            location,
        });
    }

    fn read_token(&mut self) -> Result<Token<'a>, ()> {
        self.start_line_index = self.line_index;
        self.start_char_index = self.char_index;
        match self.get_cur_ch() {
            Some("+") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("+") => {
                        self.forward();
                        Ok(BiPlus(self.get_location()))
                    }
                    Some("=") => {
                        self.forward();
                        Ok(PlusEq(self.get_location()))
                    }
                    _ => Ok(Plus(self.get_location())),
                }
            }
            Some("-") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("-") => {
                        self.forward();
                        Ok(BiMinus(self.get_location()))
                    }
                    Some("=") => {
                        self.forward();
                        Ok(MinusEq(self.get_location()))
                    }
                    _ => Ok(Minus(self.get_location())),
                }
            }
            Some("*") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(AsteriskEq(self.get_location()))
                    }
                    _ => Ok(Asterisk(self.get_location())),
                }
            }
            Some("/") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(SlashEq(self.get_location()))
                    }
                    _ => Ok(Slash(self.get_location())),
                }
            }
            Some("%") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(PercentEq(self.get_location()))
                    }
                    _ => Ok(Percent(self.get_location())),
                }
            }

            Some("<") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(SmallEq(self.get_location()))
                    }
                    _ => Ok(Small(self.get_location())),
                }
            }
            Some(">") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(LargeEq(self.get_location()))
                    }
                    _ => Ok(Large(self.get_location())),
                }
            }
            Some("=") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(EqTo(self.get_location()))
                    }
                    _ => Ok(Equal(self.get_location())),
                }
            }
            Some("&") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("&") => {
                        self.forward();
                        Ok(And(self.get_location()))
                    }
                    _ => Ok(Ampersand(self.get_location())),
                }
            }
            Some("|") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("|") => {
                        self.forward();
                        Ok(Or(self.get_location()))
                    }
                    _ => {
                        self.push_error("Invalid token `|`.");
                        Err(())
                    }
                }
            }
            Some("!") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(NotEqTo(self.get_location()))
                    }
                    _ => Ok(Not(self.get_location())),
                }
            }
            Some("(") => {
                self.forward();
                Ok(LParen(self.get_location()))
            }
            Some(")") => {
                self.forward();
                Ok(RParen(self.get_location()))
            }
            Some("[") => {
                self.forward();
                Ok(LBracket(self.get_location()))
            }
            Some("]") => {
                self.forward();
                Ok(RBracket(self.get_location()))
            }
            Some("{") => {
                self.forward();
                Ok(LBrace(self.get_location()))
            }
            Some("}") => {
                self.forward();
                Ok(RBrace(self.get_location()))
            }

            Some(",") => {
                self.forward();
                Ok(Comma(self.get_location()))
            }
            Some(":") => {
                self.forward();
                Ok(Colon(self.get_location()))
            }
            Some(";") => {
                self.forward();
                Ok(Semicolon(self.get_location()))
            }
            Some(ch) if self.raw_num_regex.is_match(ch) => self.read_num(),
            Some("'") => self.read_char(),
            Some("\"") => self.read_str(),
            Some(ch) if self.word_regex.is_match(ch) => self.read_word(),
            Some(ch) => {
                self.push_error(&format!("Invalid character `{}`.", ch));
                Err(())
            }
            None => panic!("Not possible."),
        }
    }

    fn read_num(&mut self) -> Result<Token<'a>, ()> {
        loop {
            match self.get_cur_ch() {
                Some(ch) if self.raw_num_regex.is_match(ch) => self.forward(),
                _ => break,
            }
        }
        let literal = self.get_slice();
        let literal_vec: Vec<&str> = literal.split('.').collect();
        match literal_vec.len() {
            1 => Ok(IntConst {
                literal,
                location: self.get_location(),
            }),
            2 if !literal_vec[0].is_empty() && !literal_vec[1].is_empty() => Ok(FloatConst {
                literal,
                location: self.get_location(),
            }),
            _ => {
                self.push_error(&format!("Invalid number literal `{}`.", literal));
                Err(())
            }
        }
    }

    fn read_char(&mut self) -> Result<Token<'a>, ()> {
        self.forward();
        loop {
            match self.get_cur_ch() {
                Some("\\") => {
                    self.forward();
                    match self.get_cur_ch() {
                        Some("n") | Some("'") => self.forward(),
                        _ => {}
                    }
                }
                Some("'") => {
                    self.forward();
                    break;
                }
                Some(_) => self.forward(),
                None => {
                    self.push_error("Unexpected EOF.");
                    return Err(());
                }
            }
        }
        let literal = self.get_slice();
        match literal {
            ch if ch.len() == 3 && ch.is_ascii() => Ok(CharConst {
                literal,
                location: self.get_location(),
            }),
            "'\\n'" | "'\\''" => Ok(CharConst {
                literal,
                location: self.get_location(),
            }),
            _ => {
                self.push_error(&format!("Invalid character literal `{}`.", literal));
                Err(())
            }
        }
    }

    fn read_str(&mut self) -> Result<Token<'a>, ()> {
        self.forward();
        loop {
            match self.get_cur_ch() {
                Some("\\") => {
                    self.forward();
                    match self.get_cur_ch() {
                        Some("n") | Some("\"") => self.forward(),
                        _ => {}
                    }
                }
                Some("\"") => {
                    self.forward();
                    break;
                }
                Some(_) => self.forward(),
                None => {
                    self.push_error("Unexpected EOF.");
                    return Err(());
                }
            }
        }
        let literal = self.get_slice();
        Ok(StrConst {
            literal,
            location: self.get_location(),
        })
    }

    fn read_word(&mut self) -> Result<Token<'a>, ()> {
        loop {
            match self.get_cur_ch() {
                Some(ch) if self.word_regex.is_match(ch) => self.forward(),
                _ => break,
            }
        }
        let literal = self.get_slice();
        match literal {
            "T" => Ok(T(self.get_location())),
            "void" => Ok(Void(self.get_location())),
            "char" => Ok(Char(self.get_location())),
            "short" => Ok(Short(self.get_location())),
            "int" => Ok(Int(self.get_location())),
            "long" => Ok(Long(self.get_location())),
            "float" => Ok(Float(self.get_location())),
            "double" => Ok(Double(self.get_location())),
            "signed" => Ok(Signed(self.get_location())),
            "unsigned" => Ok(Unsigned(self.get_location())),

            "switch" => Ok(Switch(self.get_location())),
            "case" => Ok(Case(self.get_location())),
            "default" => Ok(Default(self.get_location())),
            "if" => Ok(If(self.get_location())),
            "else" => Ok(Else(self.get_location())),
            "do" => Ok(Do(self.get_location())),
            "while" => Ok(While(self.get_location())),
            "for" => Ok(For(self.get_location())),
            "continue" => Ok(Continue(self.get_location())),
            "break" => Ok(Break(self.get_location())),
            "return" => Ok(Return(self.get_location())),

            _ => Ok(Ident {
                literal,
                location: self.get_location(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ident_location() {
        let source = "a b \n \n c";
        let expected_errors = vec![];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens.len(), 3);
        match tokens[0] {
            Ident {
                literal: "a",
                location:
                    Location {
                        line_no: 1,
                        char_no: 1,
                    },
            } => {}
            _ => panic!(format!("{:?}", tokens[0])),
        }
        match tokens[1] {
            Ident {
                literal: "b",
                location:
                    Location {
                        line_no: 1,
                        char_no: 3,
                    },
            } => {}
            _ => panic!(format!("{:?}", tokens[1])),
        }
        match tokens[2] {
            Ident {
                literal: "c",
                location:
                    Location {
                        line_no: 3,
                        char_no: 2,
                    },
            } => {}
            _ => panic!(format!("{:?}", tokens[2])),
        }
    }

    #[test]
    fn ident_const() {
        let source = "a 1 1.1 '\\n' '\\'' 'a' \"\\n\" \"\\\"\" \"a\"";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Ident {
                literal: "a",
                location: Location::empty(),
            },
            IntConst {
                literal: "1",
                location: Location::empty(),
            },
            FloatConst {
                literal: "1.1",
                location: Location::empty(),
            },
            CharConst {
                literal: "'\\n'",
                location: Location::empty(),
            },
            CharConst {
                literal: "'\\''",
                location: Location::empty(),
            },
            CharConst {
                literal: "'a'",
                location: Location::empty(),
            },
            StrConst {
                literal: "\"\\n\"",
                location: Location::empty(),
            },
            StrConst {
                literal: "\"\\\"\"",
                location: Location::empty(),
            },
            StrConst {
                literal: "\"a\"",
                location: Location::empty(),
            },
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn types() {
        let source = "T void char short int long float double signed unsigned";
        let expected_errors = vec![];
        let expected_tokens = vec![
            T(Location::empty()),
            Void(Location::empty()),
            Char(Location::empty()),
            Short(Location::empty()),
            Int(Location::empty()),
            Long(Location::empty()),
            Float(Location::empty()),
            Double(Location::empty()),
            Signed(Location::empty()),
            Unsigned(Location::empty()),
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn operators() {
        let source = "+ - * / % ++ -- =\n\n";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Plus(Location::empty()),
            Minus(Location::empty()),
            Asterisk(Location::empty()),
            Slash(Location::empty()),
            Percent(Location::empty()),
            BiPlus(Location::empty()),
            BiMinus(Location::empty()),
            Equal(Location::empty()),
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn relative_operators() {
        let source = "< > <= >= == != && || !";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Small(Location::empty()),
            Large(Location::empty()),
            SmallEq(Location::empty()),
            LargeEq(Location::empty()),
            EqTo(Location::empty()),
            NotEqTo(Location::empty()),
            And(Location::empty()),
            Or(Location::empty()),
            Not(Location::empty()),
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn combined_assignment_operators() {
        let source = "+= -= *= /= %=";
        let expected_errors = vec![];
        let expected_tokens = vec![
            PlusEq(Location::empty()),
            MinusEq(Location::empty()),
            AsteriskEq(Location::empty()),
            SlashEq(Location::empty()),
            PercentEq(Location::empty()),
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn parentheses() {
        let source = "( ) [ ] { }";
        let expected_errors = vec![];
        let expected_tokens = vec![
            LParen(Location::empty()),
            RParen(Location::empty()),
            LBracket(Location::empty()),
            RBracket(Location::empty()),
            LBrace(Location::empty()),
            RBrace(Location::empty()),
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn keywords() {
        let source = "switch case default if else do while for continue break return";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Switch(Location::empty()),
            Case(Location::empty()),
            Default(Location::empty()),
            If(Location::empty()),
            Else(Location::empty()),
            Do(Location::empty()),
            While(Location::empty()),
            For(Location::empty()),
            Continue(Location::empty()),
            Break(Location::empty()),
            Return(Location::empty()),
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn punctuation() {
        let source = ", : \n ;";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Comma(Location::empty()),
            Colon(Location::empty()),
            Semicolon(Location::empty()),
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn error() {
        let source = "1 | .1 1. 1.1.1 \'ab\' \'";
        let expected_errors = vec![
            Error::Lexing {
                message: "Invalid token `|`.".to_string(),
                location: Location::empty(),
            },
            Error::Lexing {
                message: "Invalid number literal `.1`.".to_string(),
                location: Location::empty(),
            },
            Error::Lexing {
                message: "Invalid number literal `1.`.".to_string(),
                location: Location::empty(),
            },
            Error::Lexing {
                message: "Invalid number literal `1.1.1`.".to_string(),
                location: Location::empty(),
            },
            Error::Lexing {
                message: "Invalid character literal `\'ab\'`.".to_string(),
                location: Location::empty(),
            },
            Error::Lexing {
                message: "Unexpected EOF.".to_string(),
                location: Location::empty(),
            },
        ];
        let expected_tokens = vec![IntConst {
            literal: "1",
            location: Location::empty(),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn error_str_eof() {
        let source = "\"";
        let expected_errors = vec![Error::Lexing {
            message: "Unexpected EOF.".to_string(),
            location: Location::empty(),
        }];
        let expected_tokens = vec![];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }
}
