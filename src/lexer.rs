use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Ident { literal: &'a str, line_no: usize, char_no: usize },
    Num { literal: &'a str, line_no: usize, char_no: usize },
    Str { literal: &'a str, line_no: usize, char_no: usize },
    
    Var { literal: &'a str, line_no: usize, char_no: usize },
    Int { literal: &'a str, line_no: usize, char_no: usize },
    
    Eq { literal: &'a str, line_no: usize, char_no: usize },
    Semicolon { literal: &'a str, line_no: usize, char_no: usize },

    EOF { literal: &'a str, line_no: usize, char_no: usize },
}

pub struct Lexer<'a> {
    lines: Vec<&'a str>,
    num_re: Regex,
    ident_re: Regex,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            lines: source.lines().collect(),
            num_re: Regex::new(r"^\d+").unwrap(),
            ident_re: Regex::new(r"[[:word:]]+").unwrap(),
        }
    }

    pub fn run(&self) -> Vec<Token> {
        let mut token_stream = Vec::new();
        for (line_index, line) in self.lines.iter().enumerate() {
            let line_no = line_index + 1;
            let mut char_no = 1;
            while char_no <= line.len() {
                let offset = self.skip_whitespace(line, char_no);
                char_no += offset;
                if char_no > line.len() { break; }
                let (token, offset) = self.read_token(line, line_no, char_no);
                token_stream.push(token);
                char_no += offset;
            }
        }
        token_stream.push(Token::EOF { literal: "", line_no: 0, char_no: 0 });
        token_stream
    }

    fn skip_whitespace(&self, line: &'a str, char_no: usize) -> usize {
        let len = line[char_no-1..].trim_start().len();
        line.len() - (char_no - 1) - len
    }

    fn read_token(&self, line: &'a str, line_no: usize, char_no: usize) -> (Token<'a>, usize) {
        let ch = &line[char_no-1..char_no];
        match ch {
            "=" => (Token::Eq { literal: ch, line_no, char_no }, 1),
            ";" => (Token::Semicolon { literal: ch, line_no, char_no }, 1),
            c if self.num_re.is_match(c) => self.read_num(line, line_no, char_no),
            "\"" => self.read_str(line, line_no, char_no),
            _ => self.read_word(line, line_no, char_no),
        }
    }

    fn read_num(&self, line: &'a str, line_no: usize, char_no: usize) -> (Token<'a>, usize) {
        let caps = self.num_re.captures(&line[char_no-1..]).unwrap();
        let offset = caps[0].len();
        (Token::Num { literal: &line[char_no-1..char_no-1+offset], line_no, char_no }, offset)
    }

    fn read_str(&self, line: &'a str, line_no: usize, char_no: usize) -> (Token<'a>, usize) {
        let mut offset = 1;
        loop {
            match &line[char_no+offset-1..char_no+offset] {
                "\"" => break,
                _ => offset += 1,
            }
        }
        offset += 1;    // for the leading "
        (Token::Str { literal: &line[char_no..char_no-1+offset-1], line_no, char_no }, offset)
    }

    fn read_word(&self, line: &'a str, line_no: usize, char_no: usize) -> (Token<'a>, usize) {
        let caps = self.ident_re.captures(&line[char_no-1..]).unwrap();
        let offset = caps[0].len();
        let literal = &line[char_no-1..char_no-1+offset];
        match literal {
            "var" => (Token::Var { literal, line_no, char_no }, offset),
            "int" => (Token::Int { literal, line_no, char_no }, offset),
            _ => (Token::Ident { literal, line_no, char_no }, offset),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn lexer() {
        let input = "int a = 1;
var b = \"2\";
        ";
        let output = vec!(
            Token::Int {
                literal: "int",
                line_no: 1,
                char_no: 1,
            },
            Token::Ident {
                literal: "a",
                line_no: 1,
                char_no: 5,
            },
            Token::Eq {
                literal: "=",
                line_no: 1,
                char_no: 7,
            },
            Token::Num {
                literal: "1",
                line_no: 1,
                char_no: 9,
            },
            Token::Semicolon {
                literal: ";",
                line_no: 1,
                char_no: 10,
            },
            Token::Var {
                literal: "var",
                line_no: 2,
                char_no: 1,
            },
            Token::Ident {
                literal: "b",
                line_no: 2,
                char_no: 5,
            },
            Token::Eq {
                literal: "=",
                line_no: 2,
                char_no: 7,
            },
            Token::Str {
                literal: "2",
                line_no: 2,
                char_no: 9,
            },
            Token::Semicolon {
                literal: ";",
                line_no: 2,
                char_no: 12,
            },
            Token::EOF {
                literal: "",
                line_no: 0,
                char_no: 0,
            },
        );
        let lexer = Lexer::new(&input);
        let token_stream = lexer.run();
        assert_eq!(token_stream, output);
    }
}
