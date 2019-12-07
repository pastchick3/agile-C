//! The lexer breaks a vector of lines into a vector of tokens.

use crate::structure::{Error, Location, Token};
use Token::*;

/// The lexer breaks a vector of lines into a vector of tokens.
///
/// The lexer first records the start location (`start_line_index`
/// and `start_char_index`), and then scans the source (`line_index`
/// and `char_index`). The lexer will slice the source from the start
/// position to the current position to produce a token.
pub struct Lexer<'a> {
    lines: Vec<(String, usize, String)>, // (file_name, line_index, line)
    start_line_index: usize,             // the start line index of a token
    last_line_index: usize,              // last line index, used by `backward_once`
    line_index: usize,                   // current line index
    start_char_index: usize,             // the start char index of a token
    last_char_index: usize,              // last char index, used by `backward_once`
    char_index: usize,                   // current char index
    eof: bool,                           // whether the lexer has hitted EOF
    tokens: Option<Vec<Token>>,
    errors: &'a mut Vec<Error>,
}

impl<'a> Lexer<'a> {
    pub fn new(lines: Vec<(String, usize, String)>, errors: &'a mut Vec<Error>) -> Lexer<'a> {
        Lexer {
            lines,
            start_line_index: 0,
            last_line_index: 0,
            line_index: 0,
            start_char_index: 0,
            last_char_index: 0,
            char_index: 0,
            eof: false,
            tokens: Some(Vec::new()),
            errors,
        }
    }

    /// breaks a vector of lines into a vector of tokens.
    pub fn run(&mut self) -> Result<Vec<Token>, ()> {
        while self.skip_whitespaces() {
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

    /// Skip whitespaces and return true. Return false if encounter EOF.
    fn skip_whitespaces(&mut self) -> bool {
        // Skip initial empty lines.
        while self.lines[self.line_index].2.is_empty() {
            self.forward();
        }
        // Skip whitespaces.
        while let Some(ch) = self.get_cur_ch() {
            if ch.is_whitespace() {
                self.forward();
            } else {
                break;
            }
        }
        // Check for EOF.
        !self.eof
    }

    /// Return the current charactor or return `None` when EOF.
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

    /// Slice the source from the start position to the current
    /// position to get a literal.
    fn get_literal(&self) -> String {
        let mut literal = String::new();
        let mut line_index = self.start_line_index;
        if line_index == self.line_index {
            // The lexer is still on the same line.
            let (_, _, line) = &self.lines[line_index];
            literal.push_str(&line[self.start_char_index..self.char_index]);
        } else {
            // Get the first line.
            let (_, _, line) = &self.lines[line_index];
            literal.push_str(&line[self.start_char_index..]);
            literal.push('\n');
            line_index += 1;
            // Get any intermediate lines.
            while line_index < self.line_index {
                let (_, _, line) = &self.lines[line_index];
                literal.push_str(&line);
                literal.push('\n');
                line_index += 1;
            }
            // Get the current line.
            if self.char_index == 0 {
                literal.pop();
            } else {
                let (_, _, line) = &self.lines[line_index];
                literal.push_str(&line[..self.char_index]);
            }
        }
        literal
    }

    /// Get the current position, using `start_line/char_index`.
    fn get_location(&self) -> Location {
        let file_name = &self.lines[self.start_line_index].0;
        let line_index = self.lines[self.start_line_index].1;
        let char_index = self.start_char_index;
        Location::new(file_name, line_index, char_index)
    }

    /// Move forward to the next char. This function will automatically
    /// go to the next line to make sure `get_cur_ch()` returns `None`
    /// only when EOF. Return false if encouter EOF.
    fn forward(&mut self) -> bool {
        // Update the last position.
        self.last_line_index = self.line_index;
        self.last_char_index = self.char_index;
        // Get the current line.
        let line = &self.lines[self.line_index].2;
        if self.char_index + 1 < line.len() {
            // Not at the end a line.
            self.char_index += 1;
        } else if self.line_index + 1 < self.lines.len() {
            // At the end of a line.
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
        } else {
            // Encounter EOF.
            self.char_index += 1;
            self.eof = true;
        }
        !self.eof
    }

    /// Move backward, but this function can only be called once after
    /// each `forward()`.
    fn backward_once(&mut self) {
        self.line_index = self.last_line_index;
        self.char_index = self.last_char_index;
    }

    /// A helper function to construct lexing errors.
    fn push_error(&mut self, message: &str) {
        let location = self.get_location();
        self.errors.push(Error::Lexing {
            message: message.to_string(),
            location,
        });
    }

    /// Read a token, and adjust the position to the first unread char.
    fn read_token(&mut self) -> Result<Token, ()> {
        // Record the starting position.
        self.start_line_index = self.line_index;
        self.start_char_index = self.char_index;
        // Read a token.
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
                        // Read a single line comment.
                        while !self.eof && self.line_index == self.start_line_index {
                            self.forward();
                        }
                        Ok(Comment {
                            literal: self.get_literal(),
                            location: self.get_location(),
                        })
                    }
                    Some('*') => {
                        // Read a multi-line comment.
                        let mut asterisk_flag = false;
                        loop {
                            self.forward();
                            match self.get_cur_ch() {
                                Some('/') if asterisk_flag => {
                                    self.forward();
                                    break;
                                }
                                Some(ch) => {
                                    asterisk_flag = ch == '*';
                                }
                                None => {
                                    self.push_error("Unexpected EOF.");
                                    return Err(());
                                }
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
                if let Some('.') = self.get_cur_ch() {
                    self.forward();
                    if let Some('.') = self.get_cur_ch() {
                        self.forward();
                        Ok(Ellipsis(self.get_location()))
                    } else {
                        self.backward_once();
                        Ok(Dot(self.get_location()))
                    }
                } else {
                    Ok(Dot(self.get_location()))
                }
            }
            Some(':') => {
                self.forward();
                Ok(Colon(self.get_location()))
            }
            Some(';') => {
                self.forward();
                Ok(Semicolon(self.get_location()))
            }
            Some(ch) if ch.is_ascii_digit() || ch == '.' => self.read_num(),
            Some('\'') => self.read_char(),
            Some('"') => self.read_str(),
            Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => self.read_word(),
            Some(ch) => {
                self.push_error(&format!("Invalid character `{}`.", ch));
                Err(())
            }
            None => unreachable!(),
        }
    }

    /// Read a numeric literal.
    fn read_num(&mut self) -> Result<Token, ()> {
        // Get the raw literal.
        while self.forward() {
            match self.get_cur_ch() {
                Some(ch) if ch.is_ascii_alphanumeric() || ch == '.' => (),
                _ => break,
            }
        }
        let literal = self.get_literal();
        // Split the raw literal by '.'.
        let literal_vec: Vec<&str> = literal.split('.').collect();
        match literal_vec.len() {
            // No '.', which is a integer.
            1 => Ok(IntConst {
                literal,
                location: self.get_location(),
            }),
            // One '.' and something else, which is a floating-point number.
            2 if literal.len() > 1 => Ok(FloatConst {
                literal,
                location: self.get_location(),
            }),
            // Encounter an invalid number literal.
            _ => {
                self.push_error(&format!("Invalid number literal `{}`.", literal));
                Err(())
            }
        }
    }

    /// Read a character literal, including the single quotation marks.
    fn read_char(&mut self) -> Result<Token, ()> {
        // Read a raw literal which may contain more than one character.
        let mut slash_flag = false;
        loop {
            self.forward();
            match self.get_cur_ch() {
                Some('\'') if !slash_flag => {
                    self.forward();
                    break;
                }
                Some(ch) => {
                    slash_flag = ch == '\\';
                }
                None => {
                    self.push_error("Unexpected EOF.");
                    return Err(());
                }
            }
        }
        let literal = self.get_literal();
        // Validate the raw literal.
        match literal.as_str() {
            // Normal characters.
            ch if ch.len() == 3 && ch.is_ascii() => Ok(CharConst {
                literal,
                location: self.get_location(),
            }),
            // Escaped characters.
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

    /// Read a string literal, including the double quotation marks.
    fn read_str(&mut self) -> Result<Token, ()> {
        let mut slash_flag = false;
        loop {
            self.forward();
            match self.get_cur_ch() {
                Some('"') if !slash_flag => {
                    self.forward();
                    break;
                }
                Some(ch) => {
                    slash_flag = ch == '\\';
                }
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

    /// Read a word literal, and check whether it is a keyword or a identifier.
    fn read_word(&mut self) -> Result<Token, ()> {
        // Read a word literal.
        while self.forward() {
            match self.get_cur_ch() {
                Some(ch) if ch.is_ascii_alphanumeric() || ch == '_' => (),
                _ => break,
            }
        }
        let literal = self.get_literal();
        // Check whether it is a keyword or a identifier.
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
            "default" => Ok(Default_(self.get_location())),
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
        let source = "\n a b \n \n \t c \n \n";
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
                assert_eq!(location.line_no, 2);
                assert_eq!(location.char_no, 2);
            }
            _ => panic!(format!("Unexpected token[0]: {:#?}", tokens[0])),
        }
        match &tokens[1] {
            Ident { literal, location } => {
                assert_eq!(literal.as_str(), "b");
                assert_eq!(location.file_name.as_str(), "file");
                assert_eq!(location.line_no, 2);
                assert_eq!(location.char_no, 4);
            }
            _ => panic!(format!("Unexpected token[1]: {:#?}", tokens[1])),
        }
        match &tokens[2] {
            Ident { literal, location } => {
                assert_eq!(literal.as_str(), "c");
                assert_eq!(location.file_name.as_str(), "file");
                assert_eq!(location.line_no, 4);
                assert_eq!(location.char_no, 4);
            }
            _ => panic!(format!("Unexpected token[2]: {:#?}", tokens[2])),
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
    fn type_() {
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
    fn relationship() {
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
            Default_(Location::default()),
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
        let source = "& . -> , : ; ...";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Ampersand(Location::default()),
            Dot(Location::default()),
            Arrow(Location::default()),
            Comma(Location::default()),
            Colon(Location::default()),
            Semicolon(Location::default()),
            Ellipsis(Location::default()),
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
        let source = "| 1.1.1 \'ab\' \'";
        let expected_errors = vec![
            Error::Lexing {
                message: "Invalid token `|`.".to_string(),
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
