//! A lexer producing tokens to the following parser.

use regex::Regex;

use crate::structure::{Error, Location, Token};
use Token::*;

/// A lexer producing tokens to the following parser.
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
        &line[self.start_char_index..self.char_index]
    }

    fn forward(&mut self) {
        // This function must ensure self.get_cur_ch() will never fail except EOF.
        let line = self.lines[self.line_index];
        if self.char_index + 1 < line.len() {
            self.char_index += 1;
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

    fn get_location(&self) -> Location {
        Location::new(self.start_line_index + 1, self.start_char_index + 1)
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
                    _ => {
                        self.push_error("Invalid token `&`.");
                        Err(())
                    }
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
    fn ident_const() {
        let source = "a 1 1.1 '\\n' '\\'' 'a' \"\\n\" \"\\\"\" \"a\"";
        let expected_errors = vec![];
        let expected_tokens = vec![
            Ident {
                literal: "a",
                location: Location::new(1, 1),
            },
            IntConst {
                literal: "1",
                location: Location::new(1, 3),
            },
            FloatConst {
                literal: "1.1",
                location: Location::new(1, 5),
            },
            CharConst {
                literal: "'\\n'",
                location: Location::new(1, 9),
            },
            CharConst {
                literal: "'\\''",
                location: Location::new(1, 14),
            },
            CharConst {
                literal: "'a'",
                location: Location::new(1, 19),
            },
            StrConst {
                literal: "\"\\n\"",
                location: Location::new(1, 23),
            },
            StrConst {
                literal: "\"\\\"\"",
                location: Location::new(1, 28),
            },
            StrConst {
                literal: "\"a\"",
                location: Location::new(1, 33),
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
            T(Location::new(1, 1)),
            Void(Location::new(1, 3)),
            Char(Location::new(1, 8)),
            Short(Location::new(1, 13)),
            Int(Location::new(1, 19)),
            Long(Location::new(1, 23)),
            Float(Location::new(1, 28)),
            Double(Location::new(1, 34)),
            Signed(Location::new(1, 41)),
            Unsigned(Location::new(1, 48)),
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
            Plus(Location::new(1, 1)),
            Minus(Location::new(1, 3)),
            Asterisk(Location::new(1, 5)),
            Slash(Location::new(1, 7)),
            Percent(Location::new(1, 9)),
            BiPlus(Location::new(1, 11)),
            BiMinus(Location::new(1, 14)),
            Equal(Location::new(1, 17)),
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
            Small(Location::new(1, 1)),
            Large(Location::new(1, 3)),
            SmallEq(Location::new(1, 5)),
            LargeEq(Location::new(1, 8)),
            EqTo(Location::new(1, 11)),
            NotEqTo(Location::new(1, 14)),
            And(Location::new(1, 17)),
            Or(Location::new(1, 20)),
            Not(Location::new(1, 23)),
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
            PlusEq(Location::new(1, 1)),
            MinusEq(Location::new(1, 4)),
            AsteriskEq(Location::new(1, 7)),
            SlashEq(Location::new(1, 10)),
            PercentEq(Location::new(1, 13)),
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
            LParen(Location::new(1, 1)),
            RParen(Location::new(1, 3)),
            LBracket(Location::new(1, 5)),
            RBracket(Location::new(1, 7)),
            LBrace(Location::new(1, 9)),
            RBrace(Location::new(1, 11)),
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
            Switch(Location::new(1, 1)),
            Case(Location::new(1, 8)),
            Default(Location::new(1, 13)),
            If(Location::new(1, 21)),
            Else(Location::new(1, 24)),
            Do(Location::new(1, 29)),
            While(Location::new(1, 32)),
            For(Location::new(1, 38)),
            Continue(Location::new(1, 42)),
            Break(Location::new(1, 51)),
            Return(Location::new(1, 57)),
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
            Comma(Location::new(1, 1)),
            Colon(Location::new(1, 3)),
            Semicolon(Location::new(2, 2)),
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn error() {
        let source = "1 & | .1 1. 1.1.1 \'ab\' \'";
        let expected_errors = vec![
            Error::Lexing {
                message: "Invalid token `&`.".to_string(),
                location: Location::new(1, 3),
            },
            Error::Lexing {
                message: "Invalid token `|`.".to_string(),
                location: Location::new(1, 5),
            },
            Error::Lexing {
                message: "Invalid number literal `.1`.".to_string(),
                location: Location::new(1, 7),
            },
            Error::Lexing {
                message: "Invalid number literal `1.`.".to_string(),
                location: Location::new(1, 10),
            },
            Error::Lexing {
                message: "Invalid number literal `1.1.1`.".to_string(),
                location: Location::new(1, 13),
            },
            Error::Lexing {
                message: "Invalid character literal `\'ab\'`.".to_string(),
                location: Location::new(1, 19),
            },
            Error::Lexing {
                message: "Unexpected EOF.".to_string(),
                location: Location::new(1, 24),
            },
        ];
        let expected_tokens = vec![IntConst {
            literal: "1",
            location: Location::new(1, 1),
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
            location: Location::new(1, 1),
        }];
        let expected_tokens = vec![];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }
}
