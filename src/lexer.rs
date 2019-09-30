use crate::structure::{Location, Error, Token};
use regex::Regex;

const NumRegex: Regex = Regex::new(r"^\d+").unwrap();
const WordRegex: Regex = Regex::new(r"[[:word:]]+").unwrap();

type TkErrOffset = Result<(Token<'a>, usize), (Error, usize)>

pub fn run<'a>(source: &str, errors: &mut Vec<Error>) -> Vec<Token<'a>> {
    let mut token_stream = Vec::new();
    for (line_index, line) in source.lines().enumerate() {
        let line_no = line_index + 1;
        let mut char_no = 1;
        while char_no <= line.len() {
            // Skip whitespaces.
            let tail_len = line[char_no-1..].trim_start().len();
            char_no = line.len() - tail_len;
            if char_no > line.len() { break; }
            // Read one token.
            match self.read_token(line, line_no, char_no) {
                Ok((token, offset)) => token_stream.push(token),
                Err((err, offset)) => errors.push(err),
            };
            char_no += offset;
        }
    }
    token_stream
}

fn read_token<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset {
    let ch = &line[char_no-1..char_no];
    match ch {
        "+" => {
            match &line[char_no..char_no+1] {
                "+" => make_token("BiPlus", "++", line_no, char_no),
                "=" => make_token("PlusEq", "+=", line_no, char_no),
                _ => make_token("Plus", "+", line_no, char_no),
            }
        },
        "-" => {
            match &line[char_no..char_no+1] {
                "-" => make_token("BiMinus", "--", line_no, char_no),
                "=" => make_token("MinusEq", "-=", line_no, char_no),
                _ => make_token("Minus", "-", line_no, char_no),
            }
        },
        "*" => {
            match &line[char_no..char_no+1] {
                "=" => make_token("AsteriskEq", "*=", line_no, char_no),
                _ => make_token("Asterisk", "*", line_no, char_no),
            }
        },
        "/" => {
            match &line[char_no..char_no+1] {
                "=" => make_token("SlashEq", "/=", line_no, char_no),
                _ => make_token("Slash", "/", line_no, char_no),
            }
        },
        "%" => {
            match &line[char_no..char_no+1] {
                "=" => make_token("PercentEq", "%=", line_no, char_no),
                _ => make_token("Percent", "%", line_no, char_no),
            }
        },


        "<" => {
            match &line[char_no..char_no+1] {
                "=" => make_token("SmallEq", "<=", line_no, char_no),
                _ => make_token("Small", "<", line_no, char_no),
            }
        },
        ">" => {
            match &line[char_no..char_no+1] {
                "=" => make_token("LargeEq", ">=", line_no, char_no),
                _ => make_token("Large", ">", line_no, char_no),
            }
        },
        "=" => {
            match &line[char_no..char_no+1] {
                "=" => make_token("EqualTo", "==", line_no, char_no),
                _ => make_token("Equal", "=", line_no, char_no),
            }
        },
        "!" => {
            match &line[char_no..char_no+1] {
                "=" => make_token("NotEqualTo", "!=", line_no, char_no),
                _ => make_token("Not", "!", line_no, char_no),
            }
        },


        "(" => make_token("RParen", "(", line_no, char_no),
        ")" => make_token("LParen", ")", line_no, char_no),
        "[" => make_token("RBracket", "[", line_no, char_no),
        "]" => make_token("LBracket", "]", line_no, char_no),
        "{" => make_token("RBrace", "{", line_no, char_no),
        "}" => make_token("LBrace", "}", line_no, char_no),

        "," => make_token("Comma", ",", line_no, char_no),
        ":" => make_token("Colon", ":", line_no, char_no),
        ";" => make_token("Semicolon", ";", line_no, char_no),

        c if NumRegex.is_match(c) => read_num(line, line_no, char_no),
        "'" => read_char(line, line_no, char_no),
        "\"" => read_str(line, line_no, char_no),
        _ => read_word(line, line_no, char_no),
    }
}

fn read_num<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset {
    let caps = self.num_re.captures(&line[char_no-1..]).unwrap();
    let offset = caps[0].len();
    (Token::Num { literal: &line[char_no-1..char_no-1+offset], line_no, char_no }, offset)
}

fn read_char<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset {
    let caps = self.num_re.captures(&line[char_no-1..]).unwrap();
    let offset = caps[0].len();
    (Token::Num { literal: &line[char_no-1..char_no-1+offset], line_no, char_no }, offset)
}

fn read_str<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset {
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

fn read_word<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset {
    let caps = self.ident_re.captures(&line[char_no-1..]).unwrap();
    let offset = caps[0].len();
    let literal = &line[char_no-1..char_no-1+offset];
    match literal {
        "var" => (Token::Var { literal, line_no, char_no }, offset),
        "int" => (Token::Int { literal, line_no, char_no }, offset),
        _ => (Token::Ident { literal, line_no, char_no }, offset),
    }
}

fn make_error(message: &str, literal: &str, line_no: usize, char_no: usize) -> TkErrOffset {
    let error = Error::Lexing {
        error: error.to_string(),
        Location { line_no, char_no },
    };
    Err((error, literal.len()))
}

fn make_token<'a>(name: &str, literal: &str, line_no: usize, char_no: usize) -> TkErrOffset {
    let location = Location { line_no, char_no };
    let token = match name {
        "Ident" => Ident { literal, location },

        "IntConst" => IntConst { literal, location },
        "FloatingConst" => FloatingConst { literal, location },
        "CharConst" => CharConst { literal, location },
        "StrConst" => StrConst { literal, location },

        "T" => T { location },
        "Void" => Void { location },
        "Char" => Char { location },
        "Short" => Short { location },
        "Int" => Int { location },
        "Long" => Long { location },
        "Float" => Float { location },
        "Double" => Double { location },
        "Signed" => Signed { location },
        "Unsigned" => Unsigned { location },

        "Plus" => Plus { location },
        "Minus" => Minus { location },
        "Asterisk" => Asterisk { location },
        "Slash" => Slash { location },
        "Percent" => Percent { location },
        "BiPlus" => BiPlus { location },
        "BiMinus" => BiMinus { location },
        "Equal" => Equal { location },

        "Small" => Small { location },
        "Large" => Large { location },
        "SmallEq" => SmallEq { location },
        "LargeEq" => LargeEq { location },
        "EqualTo" => EqualTo { location },
        "NotEqualTo" => NotEqualTo { location },
        "And" => And { location },
        "Or" => Or { location },
        "Not" => Not { location },
        
        "PlusEq" => PlusEq { location },
        "MinusEq" => MinusEq { location },
        "AsteriskEq" => AsteriskEq { location },
        "SlashEq" => SlashEq { location },
        "PercentEq" => PercentEq { location },

        "LParen" => LParen { location },
        "RParen" => RParen { location },
        "LBracket" => LBracket { location },
        "RBracket" => RBracket { location },
        "LBrace" => LBrace { location },
        "RBrace" => RBrace { location },

        "Switch" => Switch { location },
        "Case" => Case { location },
        "Default" => Default { location },
        "If" => If { location },
        "Else" => Else { location },
        "Do" => Do { location },
        "While" => While { location },
        "For" => For { location },
        "Continue" => Continue { location },
        "Break" => Break { location },
        "Return" => Return { location },

        "Comma" => Comma { location },
        "Colon" => Colon { location },
        "Semicolon" => Semicolon { location },
    };
    Ok((token, literal.len()))
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
