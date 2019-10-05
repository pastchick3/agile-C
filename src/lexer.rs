use crate::structure::{Location, Error, Token};
use Token::*;
use regex::Regex;

pub struct Lexer<'a> {
    lines: Vec<&'a str>,
    line_index: usize,
    start_index: usize,
    char_index: usize,
    errors: Option<Vec<Error>>,
    tokens: Option<Vec<Token<'a>>>,
    raw_num_regex: Regex,
    word_regex: Regex,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, errors: Vec<Error>) -> Lexer {
        Lexer {
            lines: source.lines().collect(),
            line_index: 0,
            start_index: 0,
            char_index: 0,
            errors: Some(errors),
            tokens: Some(Vec::new()),
            raw_num_regex: Regex::new(r"(\d|\.)+").unwrap(),
            word_regex: Regex::new(r"[[:word:]]+").unwrap(),
        }
    }

    pub fn run(&mut self) -> (Vec<Token<'a>>, Vec<Error>) {            
        loop {
            // Skip whitespaces.
            loop {
                self.skip_whitespaces();
                if self.get_cur_ch().is_none()
                        && self.line_index < self.lines.len() - 1 {
                    self.line_index += 1;
                    self.char_index = 0;
                } else {
                    break;
                }
            }
            // Detect EOF.
            if self.get_cur_ch().is_none()
                    && self.line_index == self.lines.len() - 1 {
                break;
            }
            // Read one token.
            match self.read_token() {
                Ok(tk) => self.tokens.as_mut().unwrap().push(tk),
                Err(_) => {},
            }
        }
        (self.tokens.take().unwrap(), self.errors.take().unwrap())
    }

    fn skip_whitespaces(&mut self) {
        loop {
            match self.get_cur_ch() {
                Some(ch) => {
                    if ch.trim_start().len() == 0 {
                        self.forward();
                    } else {
                        break;
                    }
                },
                None => break
            }
        }
    }

    fn get_cur_ch(&self) -> Option<&'a str> {
        let line = self.lines[self.line_index];
        if self.char_index < line.len() {
            Some(&line[self.char_index..self.char_index+1])
        } else {
            None
        }
    }

    fn get_slice(&self) -> &'a str {
        let line = self.lines[self.line_index];
        &line[self.start_index..self.char_index]
    }

    fn forward(&mut self) {
        self.char_index += 1;
    }

    fn read_token(&mut self) -> Result<Token<'a>, ()> {
        self.start_index = self.char_index;
        match self.get_cur_ch() {
            Some("+") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("+") => {
                        self.forward();
                        Ok(self.make_token("BiPlus", "++"))
                    },
                    Some("=") => {
                        self.forward();
                        Ok(self.make_token("PlusEq", "+="))
                    },
                    _ => Ok(self.make_token("Plus", "+")),
                }
            },
            Some("-") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("-") => {
                        self.forward();
                        Ok(self.make_token("BiMinus", "--"))
                    },
                    Some("=") => {
                        self.forward();
                        Ok(self.make_token("MinusEq", "-="))
                    },
                    _ => Ok(self.make_token("Minus", "-")),
                }
            },
            Some("*") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(self.make_token("AsteriskEq", "*="))
                    },
                    _ => Ok(self.make_token("Asterisk", "*")),
                }
            },
            Some("/") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(self.make_token("SlashEq", "/="))
                    },
                    _ => Ok(self.make_token("Slash", "/")),
                }
            },
            Some("%") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(self.make_token("PercentEq", "%="))
                    },
                    _ => Ok(self.make_token("Percent", "%")),
                }
            },


            Some("<") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(self.make_token("SmallEq", "<="))
                    },
                    _ => Ok(self.make_token("Small", "<")),
                }
            },
            Some(">") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(self.make_token("LargeEq", ">="))
                    },
                    _ => Ok(self.make_token("Large", ">")),
                }
            },
            Some("=") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(self.make_token("EqualTo", "=="))
                    },
                    _ => Ok(self.make_token("Equal", "=")),
                }
            },
            Some("&") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("&") => {
                        self.forward();
                        Ok(self.make_token("And", "&&"))
                    },
                    _ => Err(self.push_error("Invalid operator `&`.")),
                }
            },
            Some("|") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("|") => {
                        self.forward();
                        Ok(self.make_token("Or", "||"))
                    },
                    _ => Err(self.push_error("Invalid operator `|`.")),
                }
            },
            Some("!") => {
                self.forward();
                match self.get_cur_ch() {
                    Some("=") => {
                        self.forward();
                        Ok(self.make_token("NotEqualTo", "!="))
                    },
                    _ => Ok(self.make_token("Not", "!")),
                }
            },


            Some("(") => { self.forward(); Ok(self.make_token("LParen", "(")) },
            Some(")") => { self.forward(); Ok(self.make_token("RParen", ")")) },
            Some("[") => { self.forward(); Ok(self.make_token("LBracket", "[")) },
            Some("]") => { self.forward(); Ok(self.make_token("RBracket", "]")) },
            Some("{") => { self.forward(); Ok(self.make_token("LBrace", "{")) },
            Some("}") => { self.forward(); Ok(self.make_token("RBrace", "}")) },

            Some(",") => { self.forward(); Ok(self.make_token("Comma", ",")) },
            Some(":") => { self.forward(); Ok(self.make_token("Colon", ":")) },
            Some(";") => { self.forward(); Ok(self.make_token("Semicolon", ";")) },

            Some(ch) if self.raw_num_regex.is_match(ch) => self.read_num(),
            Some("'") => self.read_char(),
            Some("\"") => self.read_str(),
            Some(ch) if self.word_regex.is_match(ch) => self.read_word(),
            
            Some(c) => Err(self.push_error(&format!("Invalid character `{}`.", c))),
            
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
        let literal_vec: Vec<&str> = literal.split(".").collect();
        match literal_vec.len() {
            1 if literal_vec[0].len() != 0 => Ok(self.make_token("IntConst", literal)),
            2 if literal_vec[0].len() != 0 && literal_vec[1].len() != 0 => Ok(
                self.make_token("FloatingConst", literal)
            ),
            _ => Err(self.push_error(&format!("Invalid number literal `{}`.", literal))),
        }
    }

    fn read_char(&mut self) -> Result<Token<'a>, ()> {
        self.forward();
        loop {
            match self.get_cur_ch() {
                Some("\\") => {
                    self.forward();
                    match self.get_cur_ch() {
                        Some("n") | Some("\"") | Some("'") => self.forward(),
                        _ => {},
                    }
                },
                Some("'") => {
                    self.forward(); 
                    break;
                },
                Some(_) => self.forward(),
                None => return Err(
                    self.push_error("Encouter EOF while lexing a char literal.")
                ),
            }
        }
        let literal = self.get_slice();
        let literal = &literal[1..literal.len()-1];
        match literal {
            ch if ch.len() == 1 && ch.is_ascii() => Ok(self.make_token("CharConst", literal)),
            "\\n" | "\\\"" | "\\'"  => Ok(self.make_token("CharConst", literal)),
            _ => Err(
                self.push_error(&format!("Invalid character literal `\'{}\'`.", literal))
            ),
        }
    }

    fn read_str(&mut self) -> Result<Token<'a>, ()> {
        self.forward();
        loop {
            match self.get_cur_ch() {
                Some("\\") => {
                    self.forward();
                    match self.get_cur_ch() {
                        Some("n") | Some("\"") | Some("'") => self.forward(),
                        _ => {},
                    }
                },
                Some("\"") => {
                    self.forward(); 
                    break;
                },
                Some(_) => self.forward(),
                None => return Err(
                    self.push_error("Encouter EOF while lexing a str literal.")
                ),
            }
        }
        let literal = self.get_slice();
        let literal = &literal[1..literal.len()-1];
        Ok(self.make_token("StrConst", literal))
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
            "T" => Ok(self.make_token("T", "T")),
            "void" => Ok(self.make_token("Void", "void")),
            "char" => Ok(self.make_token("Char", "char")),
            "short" => Ok(self.make_token("Short", "short")),
            "int" => Ok(self.make_token("Int", "int")),
            "long" => Ok(self.make_token("Long", "long")),
            "float" => Ok(self.make_token("Float", "float")),
            "double" => Ok(self.make_token("Double", "double")),
            "signed" => Ok(self.make_token("Signed", "signed")),
            "unsigned" => Ok(self.make_token("Unsigned", "unsigned")),

            "switch" => Ok(self.make_token("Switch", "switch")),
            "case" => Ok(self.make_token("Case", "case")),
            "default" => Ok(self.make_token("Default", "default")),
            "if" => Ok(self.make_token("If", "if")),
            "else" => Ok(self.make_token("Else", "else")),
            "do" => Ok(self.make_token("Do", "do")),
            "while" => Ok(self.make_token("While", "while")),
            "for" => Ok(self.make_token("For", "for")),
            "continue" => Ok(self.make_token("Continue", "continue")),
            "break" => Ok(self.make_token("Break", "break")),
            "return" => Ok(self.make_token("Return", "return")),

            s => Ok(self.make_token("Ident", s)),
        }
    }

    fn push_error(&mut self, message: &str) {
        self.errors.as_mut().unwrap().push(
            Error::Lexing {
                message: message.to_string(),
                location: Location::new(self.line_index+1, self.start_index+1),
            }
        );
    }

    fn make_token(&self, name: &str, literal: &'a str) -> Token<'a> {
        let location = Location::new(self.line_index+1, self.start_index+1);
        let token = match name {
            "Ident" => Ident { literal, location },

            "IntConst" => IntConst { literal, location },
            "FloatingConst" => FloatingConst { literal, location },
            "CharConst" => CharConst { literal, location },
            "StrConst" => StrConst { literal, location },

            "T" => T(location),
            "Void" => Void(location),
            "Char" => Char(location),
            "Short" => Short(location),
            "Int" => Int(location),
            "Long" => Long(location),
            "Float" => Float(location),
            "Double" => Double(location),
            "Signed" => Signed(location),
            "Unsigned" => Unsigned(location),

            "Plus" => Plus(location),
            "Minus" => Minus(location),
            "Asterisk" => Asterisk(location),
            "Slash" => Slash(location),
            "Percent" => Percent(location),
            "BiPlus" => BiPlus(location),
            "BiMinus" => BiMinus(location),
            "Equal" => Equal(location),

            "Small" => Small(location),
            "Large" => Large(location),
            "SmallEq" => SmallEq(location),
            "LargeEq" => LargeEq(location),
            "EqualTo" => EqualTo(location),
            "NotEqualTo" => NotEqualTo(location),
            "And" => And(location),
            "Or" => Or(location),
            "Not" => Not(location),
            
            "PlusEq" => PlusEq(location),
            "MinusEq" => MinusEq(location),
            "AsteriskEq" => AsteriskEq(location),
            "SlashEq" => SlashEq(location),
            "PercentEq" => PercentEq(location),

            "LParen" => LParen(location),
            "RParen" => RParen(location),
            "LBracket" => LBracket(location),
            "RBracket" => RBracket(location),
            "LBrace" => LBrace(location),
            "RBrace" => RBrace(location),

            "Switch" => Switch(location),
            "Case" => Case(location),
            "Default" => Default(location),
            "If" => If(location),
            "Else" => Else(location),
            "Do" => Do(location),
            "While" => While(location),
            "For" => For(location),
            "Continue" => Continue(location),
            "Break" => Break(location),
            "Return" => Return(location),

            "Comma" => Comma(location),
            "Colon" => Colon(location),
            "Semicolon" => Semicolon(location),

            _ => panic!("Not impossible."),
        };
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn ident_const() {
        let source = "a 1 1.1 '\\n' '\\\"' '\\'' 'a' \"\\n\" \"\\\"\" \"\\'\" \"a\"";
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
            FloatingConst {
                literal: "1.1",
                location: Location::new(1, 5),
            },
            CharConst {
                literal: "\\n",
                location: Location::new(1, 9),
            },
            CharConst {
                literal: "\\\"",
                location: Location::new(1, 14),
            },
            CharConst {
                literal: "\\'",
                location: Location::new(1, 19),
            },
            CharConst {
                literal: "a",
                location: Location::new(1, 24),
            },
            StrConst {
                literal: "\\n",
                location: Location::new(1, 28),
            },
            StrConst {
                literal: "\\\"",
                location: Location::new(1, 33),
            },
            StrConst {
                literal: "\\'",
                location: Location::new(1, 38),
            },
            StrConst {
                literal: "a",
                location: Location::new(1, 43),
            },
        ];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn type_specifier() {
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
        let source = "+ - * / % ++ -- =";
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
            EqualTo(Location::new(1, 11)),
            NotEqualTo(Location::new(1, 14)),
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
    fn parens() {
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
                message: "Invalid operator `&`.".to_string(),
                location: Location::new(1, 3),
            },
            Error::Lexing {
                message: "Invalid operator `|`.".to_string(),
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
                message: "Encouter EOF while lexing a char literal.".to_string(),
                location: Location::new(1, 24),
            },
        ];
        let expected_tokens = vec![
            IntConst {
                literal: "1",
                location: Location::new(1, 1),
            },
        ];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn error_str_eof() {
        let source = "\"";
        let expected_errors = vec![
            Error::Lexing {
                message: "Encouter EOF while lexing a str literal.".to_string(),
                location: Location::new(1, 1),
            },
        ];
        let expected_tokens = vec![];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected_tokens);
    }
}
