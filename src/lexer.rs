//! A lexer producing a vector of tokens.

use crate::structure::{Error, Location, Token};
use Token::*;

/// A lexer producing a vector of tokens for the parser.
///
/// The lexer first records the start location (`start_line_index` and`start_char_index`)
/// and then scans the source (`line_index` and `char_index`). When a token is produced,
/// the lexer will slice the source from the start position to the current position.
pub struct Lexer<'a> {
    lines: Vec<(String, usize, String)>,
    start_line_index: usize,
    line_index: usize,
    start_char_index: usize,
    char_index: usize,
    eof: bool,
    tokens: Option<Vec<Token>>,
    errors: &'a mut Vec<Error>,
}

impl<'a> Lexer<'a> {
    pub fn new(lines: Vec<(String, usize, String)>, errors: &'a mut Vec<Error>) -> Lexer<'a> {
        Lexer {
            lines,
            start_line_index: 0,
            line_index: 0,
            start_char_index: 0,
            char_index: 0,
            eof: false,
            tokens: Some(Vec::new()),
            errors,
        }
    }

    pub fn run(&mut self) -> Result<Vec<Token>, ()> {
        // Skip initial empty lines.
        while self.lines[self.line_index].2.is_empty() {
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
        if self.errors.is_empty() {
            Ok(self.tokens.take().unwrap())
        } else {
            Err(())
        }
    }

    fn skip_whitespaces(&mut self) {
        while let Some(ch) = self.get_cur_ch() {
            if ch.is_whitespace() {
                self.forward();
            } else {
                break;
            }
        }
    }

    /// Notice this function will return `None` only when EOF.
    fn get_cur_ch(&self) -> Option<char> {
        if self.eof {
            None
        } else {
            let line = &self.lines[self.line_index].2;
            let ch = line[self.char_index..=self.char_index]
                .chars()
                .next()
                .unwrap();
            Some(ch)
        }
    }

    fn get_literal(&self) -> String {
        let mut literal = String::new();
        let mut line_index = self.start_line_index;
        if line_index == self.line_index {
            let (_, _, line) = &self.lines[line_index];
            literal.push_str(&line[self.start_char_index..self.char_index]);
        } else {
            let (_, _, line) = &self.lines[line_index];
            literal.push_str(&line[self.start_char_index..]);
            literal.push('\n');
            line_index += 1;
            while line_index < self.line_index {
                let (_, _, line) = &self.lines[line_index];
                literal.push_str(&line);
                literal.push('\n');
                line_index += 1;
            }
            if self.char_index == 0 {
                literal.pop();
            } else {
                let (_, _, line) = &self.lines[line_index];
                literal.push_str(&line[..self.char_index]);
            }
        }
        literal
    }

    fn get_location(&self) -> Location {
        let file_name = &self.lines[self.start_line_index].0;
        let line_index = self.lines[self.start_line_index].1;
        let char_index = self.start_char_index;
        Location::new(file_name, line_index, char_index)
    }

    /// This function will automatically go to the next line to make
    /// sure `get_cur_ch()` returns `None` only when EOF.
    fn forward(&mut self) {
        let line = &self.lines[self.line_index].2;
        // Not at the end a line.
        if self.char_index + 1 < line.len() {
            self.char_index += 1;
        // At the end of a line.
        } else if self.line_index + 1 < self.lines.len() {
            self.line_index += 1;
            self.char_index = 0;
            // Skip empty lines.
            while self.lines[self.line_index].2.is_empty() {
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
        self.errors.push(Error::Lexing {
            message: message.to_string(),
            location,
        });
    }

    fn read_token(&mut self) -> Result<Token, ()> {
        self.start_line_index = self.line_index;
        self.start_char_index = self.char_index;
        match self.get_cur_ch() {
            Some('+') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('+') => {
                        self.forward();
                        Ok(BiPlus(self.get_location()))
                    }
                    Some('=') => {
                        self.forward();
                        Ok(PlusEq(self.get_location()))
                    }
                    _ => Ok(Plus(self.get_location())),
                }
            }
            Some('-') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('-') => {
                        self.forward();
                        Ok(BiMinus(self.get_location()))
                    }
                    Some('=') => {
                        self.forward();
                        Ok(MinusEq(self.get_location()))
                    }
                    Some('>') => {
                        self.forward();
                        Ok(Arrow(self.get_location()))
                    }
                    _ => Ok(Minus(self.get_location())),
                }
            }
            Some('*') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('=') => {
                        self.forward();
                        Ok(AsteriskEq(self.get_location()))
                    }
                    _ => Ok(Asterisk(self.get_location())),
                }
            }
            Some('/') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('=') => {
                        self.forward();
                        Ok(SlashEq(self.get_location()))
                    }
                    Some('/') => {
                        while !self.eof && self.line_index == self.start_line_index {
                            self.forward();
                        }
                        Ok(Comment {
                            literal: self.get_literal(),
                            location: self.get_location(),
                        })
                    }
                    Some('*') => {
                        self.forward();
                        let mut asterisk_flag = false;
                        if let Some('*') = self.get_cur_ch() {
                            asterisk_flag = true;
                        }
                        self.forward();
                        loop {
                            if self.eof {
                                break;
                            } else if asterisk_flag && self.get_cur_ch() == Some('/') {
                                self.forward();
                                break;
                            } else if asterisk_flag && self.get_cur_ch() != Some('*') {
                                asterisk_flag = false;
                                self.forward();
                            } else if !asterisk_flag && self.get_cur_ch() == Some('*') {
                                asterisk_flag = true;
                                self.forward();
                            } else {
                                self.forward();
                            }
                        }
                        Ok(Comment {
                            literal: self.get_literal(),
                            location: self.get_location(),
                        })
                    }
                    _ => Ok(Slash(self.get_location())),
                }
            }
            Some('%') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('=') => {
                        self.forward();
                        Ok(PercentEq(self.get_location()))
                    }
                    _ => Ok(Percent(self.get_location())),
                }
            }

            Some('<') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('=') => {
                        self.forward();
                        Ok(SmallEq(self.get_location()))
                    }
                    _ => Ok(Small(self.get_location())),
                }
            }
            Some('>') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('=') => {
                        self.forward();
                        Ok(LargeEq(self.get_location()))
                    }
                    _ => Ok(Large(self.get_location())),
                }
            }
            Some('=') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('=') => {
                        self.forward();
                        Ok(EqTo(self.get_location()))
                    }
                    _ => Ok(Equal(self.get_location())),
                }
            }
            Some('&') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('&') => {
                        self.forward();
                        Ok(And(self.get_location()))
                    }
                    _ => Ok(Ampersand(self.get_location())),
                }
            }
            Some('|') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('|') => {
                        self.forward();
                        Ok(Or(self.get_location()))
                    }
                    _ => {
                        self.push_error("Invalid token `|`.");
                        Err(())
                    }
                }
            }
            Some('!') => {
                self.forward();
                match self.get_cur_ch() {
                    Some('=') => {
                        self.forward();
                        Ok(NotEqTo(self.get_location()))
                    }
                    _ => Ok(Not(self.get_location())),
                }
            }
            Some('(') => {
                self.forward();
                Ok(LParen(self.get_location()))
            }
            Some(')') => {
                self.forward();
                Ok(RParen(self.get_location()))
            }
            Some('[') => {
                self.forward();
                Ok(LBracket(self.get_location()))
            }
            Some(']') => {
                self.forward();
                Ok(RBracket(self.get_location()))
            }
            Some('{') => {
                self.forward();
                Ok(LBrace(self.get_location()))
            }
            Some('}') => {
                self.forward();
                Ok(RBrace(self.get_location()))
            }

            Some(',') => {
                self.forward();
                Ok(Comma(self.get_location()))
            }
            Some('.') => {
                self.forward();
                Ok(Dot(self.get_location()))
            }
            Some(':') => {
                self.forward();
                Ok(Colon(self.get_location()))
            }
            Some(';') => {
                self.forward();
                Ok(Semicolon(self.get_location()))
            }
            Some(ch) if ch.is_numeric() => self.read_num(),
            Some('\'') => self.read_char(),
            Some('"') => self.read_str(),
            Some(ch) if ch.is_ascii_alphanumeric() => self.read_word(),
            Some(ch) => {
                self.push_error(&format!("Invalid character `{}`.", ch));
                Err(())
            }
            None => unreachable!(),
        }
    }

    fn read_num(&mut self) -> Result<Token, ()> {
        loop {
            match self.get_cur_ch() {
                Some(ch) if ch.is_numeric() || ch == '.' => self.forward(),
                _ => break,
            }
        }
        let literal = self.get_literal();
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

    fn read_char(&mut self) -> Result<Token, ()> {
        self.forward();
        loop {
            match self.get_cur_ch() {
                Some('\\') => {
                    self.forward();
                    match self.get_cur_ch() {
                        Some('n') | Some('\'') => self.forward(),
                        _ => {}
                    }
                }
                Some('\'') => {
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
        let literal = self.get_literal();
        match literal.as_str() {
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

    fn read_str(&mut self) -> Result<Token, ()> {
        self.forward();
        loop {
            match self.get_cur_ch() {
                Some('\\') => {
                    self.forward();
                    match self.get_cur_ch() {
                        Some('n') | Some('"') => self.forward(),
                        _ => {}
                    }
                }
                Some('"') => {
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
        Ok(StrConst {
            literal: self.get_literal(),
            location: self.get_location(),
        })
    }

    fn read_word(&mut self) -> Result<Token, ()> {
        loop {
            match self.get_cur_ch() {
                Some(ch) if ch.is_ascii_alphanumeric() => self.forward(),
                _ => break,
            }
        }
        let literal = self.get_literal();
        match literal.as_str() {
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
            "struct" => Ok(Struct(self.get_location())),

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
    use crate::preprocessor::Preprocessor;

    #[test]
    fn location() {
        let source = "a b \n \n c \n \n";
        let expected_errors = vec![];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens.len(), 3);
        match &tokens[0] {
            Ident { literal, location } => {
                assert_eq!(literal.as_str(), "a");
                assert_eq!(location.file_name.as_str(), "file");
                assert_eq!(location.line_no, 1);
                assert_eq!(location.char_no, 1);
            }
            _ => panic!(format!("{:?}", tokens[0])),
        }
        match &tokens[1] {
            Ident { literal, location } => {
                assert_eq!(literal.as_str(), "b");
                assert_eq!(location.file_name.as_str(), "file");
                assert_eq!(location.line_no, 1);
                assert_eq!(location.char_no, 3);
            }
            _ => panic!(format!("{:?}", tokens[1])),
        }
        match &tokens[2] {
            Ident { literal, location } => {
                assert_eq!(literal.as_str(), "c");
                assert_eq!(location.file_name.as_str(), "file");
                assert_eq!(location.line_no, 3);
                assert_eq!(location.char_no, 2);
            }
            _ => panic!(format!("{:?}", tokens[2])),
        }
    }

    #[test]
    fn ident_const() {
        let source = "a 1 1.1 '\\n' '\\'' 'a' \"\\n\" \"\\\"\" \"a\"";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Ident {
                literal: "a".to_string(),
                location: Location::default(),
            },
            IntConst {
                literal: "1".to_string(),
                location: Location::default(),
            },
            FloatConst {
                literal: "1.1".to_string(),
                location: Location::default(),
            },
            CharConst {
                literal: "'\\n'".to_string(),
                location: Location::default(),
            },
            CharConst {
                literal: "'\\''".to_string(),
                location: Location::default(),
            },
            CharConst {
                literal: "'a'".to_string(),
                location: Location::default(),
            },
            StrConst {
                literal: "\"\\n\"".to_string(),
                location: Location::default(),
            },
            StrConst {
                literal: "\"\\\"\"".to_string(),
                location: Location::default(),
            },
            StrConst {
                literal: "\"a\"".to_string(),
                location: Location::default(),
            },
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn comment() {
        let source = "
            a // 1
            /*
            */ a /*
            * / 1
            */
        ";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Ident {
                literal: "a".to_string(),
                location: Location::default(),
            },
            Comment {
                literal: "// 1".to_string(),
                location: Location::default(),
            },
            Comment {
                literal: "/*
            */"
                .to_string(),
                location: Location::default(),
            },
            Ident {
                literal: "a".to_string(),
                location: Location::default(),
            },
            Comment {
                literal: "/*
            * / 1
            */"
                .to_string(),
                location: Location::default(),
            },
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn r#type() {
        let source = "T void char short int long float double signed unsigned";
        let expected_errors = vec![];
        let expected_tokens = vec![
            T(Location::default()),
            Void(Location::default()),
            Char(Location::default()),
            Short(Location::default()),
            Int(Location::default()),
            Long(Location::default()),
            Float(Location::default()),
            Double(Location::default()),
            Signed(Location::default()),
            Unsigned(Location::default()),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn arithmetic() {
        let source = "+ - * / % ++ -- =";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Plus(Location::default()),
            Minus(Location::default()),
            Asterisk(Location::default()),
            Slash(Location::default()),
            Percent(Location::default()),
            BiPlus(Location::default()),
            BiMinus(Location::default()),
            Equal(Location::default()),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn relational() {
        let source = "< > <= >= == != && || !";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Small(Location::default()),
            Large(Location::default()),
            SmallEq(Location::default()),
            LargeEq(Location::default()),
            EqTo(Location::default()),
            NotEqTo(Location::default()),
            And(Location::default()),
            Or(Location::default()),
            Not(Location::default()),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn compound_assignment() {
        let source = "+= -= *= /= %=";
        let expected_errors = vec![];
        let expected_tokens = vec![
            PlusEq(Location::default()),
            MinusEq(Location::default()),
            AsteriskEq(Location::default()),
            SlashEq(Location::default()),
            PercentEq(Location::default()),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn parenthesis() {
        let source = "( ) [ ] { }";
        let expected_errors = vec![];
        let expected_tokens = vec![
            LParen(Location::default()),
            RParen(Location::default()),
            LBracket(Location::default()),
            RBracket(Location::default()),
            LBrace(Location::default()),
            RBrace(Location::default()),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn keyword() {
        let source = "switch case default if else do while for continue break return struct";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Switch(Location::default()),
            Case(Location::default()),
            Default(Location::default()),
            If(Location::default()),
            Else(Location::default()),
            Do(Location::default()),
            While(Location::default()),
            For(Location::default()),
            Continue(Location::default()),
            Break(Location::default()),
            Return(Location::default()),
            Struct(Location::default()),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn punctuation() {
        let source = "& . -> , : ;";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Ampersand(Location::default()),
            Dot(Location::default()),
            Arrow(Location::default()),
            Comma(Location::default()),
            Colon(Location::default()),
            Semicolon(Location::default()),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn error() {
        let source = "| 1. 1.1.1 \'ab\' \'";
        let expected_errors = vec![
            Error::Lexing {
                message: "Invalid token `|`.".to_string(),
                location: Location::default(),
            },
            Error::Lexing {
                message: "Invalid number literal `1.`.".to_string(),
                location: Location::default(),
            },
            Error::Lexing {
                message: "Invalid number literal `1.1.1`.".to_string(),
                location: Location::default(),
            },
            Error::Lexing {
                message: "Invalid character literal `\'ab\'`.".to_string(),
                location: Location::default(),
            },
            Error::Lexing {
                message: "Unexpected EOF.".to_string(),
                location: Location::default(),
            },
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        Lexer::new(lines, &mut errors).run().unwrap_err();
        assert_eq!(errors, expected_errors);
    }

    #[test]
    fn error_str_eof() {
        let source = "\"";
        let expected_errors = vec![Error::Lexing {
            message: "Unexpected EOF.".to_string(),
            location: Location::default(),
        }];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        Lexer::new(lines, &mut errors).run().unwrap_err();
        assert_eq!(errors, expected_errors);
    }
}
