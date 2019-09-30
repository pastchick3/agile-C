use crate::structure::{Location, Error, Token};
use Token::*;
use regex::Regex;

type TkErrOffset<'a> = Result<(Token<'a>, usize), (Error, usize)>;

pub fn run<'a>(source: &'a str, errors: &mut Vec<Error>) -> Vec<Token<'a>> {
    let mut token_stream = Vec::new();
    for (line_index, line) in source.lines().enumerate() {
        let line_no = line_index + 1;
        let mut char_no = 1;
        while char_no <= line.len() {
            // Skip whitespaces.
            let tail_len = line[char_no-1..].trim_start().len();
            char_no += line.len() - (char_no - 1) - tail_len;
            if char_no > line.len() { break; }
            // Read one token.
            match read_token(line, line_no, char_no) {
                Ok((token, offset)) => {
                    token_stream.push(token);
                    char_no += offset;
                },
                Err((err, offset)) => {
                    errors.push(err);
                    char_no += offset;
                },
            }
        }
    }
    token_stream
}

fn get_cur_ch<'a>(line: &'a str, char_no: usize) -> Option<&'a str> {
    if char_no <= line.len() {
        Some(&line[char_no-1..char_no])
    } else {
        None
    }
}

fn get_next_ch<'a>(line: &'a str, char_no: usize) -> Option<&'a str> {
    if char_no + 1 <= line.len() {
        Some(&line[char_no..char_no+1])
    } else {
        None
    }
}

fn read_token<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset<'a> {
    let raw_num_regex = Regex::new(r"(\d|\.)+").unwrap();
    let word_regex = Regex::new(r"[[:word:]]+").unwrap();
    let cur_ch = get_cur_ch(line, char_no);
    let next_ch = get_next_ch(line, char_no);
    match cur_ch {
        Some("+") => {
            match next_ch {
                Some("+") => make_tk_offset("BiPlus", "++", line_no, char_no),
                Some("=") => make_tk_offset("PlusEq", "+=", line_no, char_no),
                _ => make_tk_offset("Plus", "+", line_no, char_no),
            }
        },
        Some("-") => {
            match next_ch {
                Some("-") => make_tk_offset("BiMinus", "--", line_no, char_no),
                Some("=") => make_tk_offset("MinusEq", "-=", line_no, char_no),
                _ => make_tk_offset("Minus", "-", line_no, char_no),
            }
        },
        Some("*") => {
            match next_ch {
                Some("=") => make_tk_offset("AsteriskEq", "*=", line_no, char_no),
                _ => make_tk_offset("Asterisk", "*", line_no, char_no),
            }
        },
        Some("/") => {
            match next_ch {
                Some("=") => make_tk_offset("SlashEq", "/=", line_no, char_no),
                _ => make_tk_offset("Slash", "/", line_no, char_no),
            }
        },
        Some("%") => {
            match next_ch {
                Some("=") => make_tk_offset("PercentEq", "%=", line_no, char_no),
                _ => make_tk_offset("Percent", "%", line_no, char_no),
            }
        },


        Some("<") => {
            match next_ch {
                Some("=") => make_tk_offset("SmallEq", "<=", line_no, char_no),
                _ => make_tk_offset("Small", "<", line_no, char_no),
            }
        },
        Some(">") => {
            match next_ch {
                Some("=") => make_tk_offset("LargeEq", ">=", line_no, char_no),
                _ => make_tk_offset("Large", ">", line_no, char_no),
            }
        },
        Some("=") => {
            match next_ch {
                Some("=") => make_tk_offset("EqualTo", "==", line_no, char_no),
                _ => make_tk_offset("Equal", "=", line_no, char_no),
            }
        },
        Some("&") => {
            match next_ch {
                Some("&") => make_tk_offset("And", "&&", line_no, char_no),
                _ => make_err_offset("Invalid operator `&`", "&", line_no, char_no),
            }
        },
        Some("|") => {
            match next_ch {
                Some("|") => make_tk_offset("Or", "||", line_no, char_no),
                _ => make_err_offset("Invalid operator `|`", "|", line_no, char_no),
            }
        },
        Some("!") => {
            match next_ch {
                Some("=") => make_tk_offset("NotEqualTo", "!=", line_no, char_no),
                _ => make_tk_offset("Not", "!", line_no, char_no),
            }
        },


        Some("(") => make_tk_offset("LParen", "(", line_no, char_no),
        Some(")") => make_tk_offset("RParen", ")", line_no, char_no),
        Some("[") => make_tk_offset("LBracket", "[", line_no, char_no),
        Some("]") => make_tk_offset("RBracket", "]", line_no, char_no),
        Some("{") => make_tk_offset("LBrace", "{", line_no, char_no),
        Some("}") => make_tk_offset("RBrace", "}", line_no, char_no),

        Some(",") => make_tk_offset("Comma", ",", line_no, char_no),
        Some(":") => make_tk_offset("Colon", ":", line_no, char_no),
        Some(";") => make_tk_offset("Semicolon", ";", line_no, char_no),

        Some(c) if raw_num_regex.is_match(c) => read_num(line, line_no, char_no),
        Some("'") => read_char(line, line_no, char_no),
        Some("\"") => read_str(line, line_no, char_no),
        Some(c) if word_regex.is_match(c) => read_word(line, line_no, char_no),
        
        Some(c) => make_err_offset(&format!("Invalid character `{}`", c), c, line_no, char_no),
        None => panic!("Not possible."),
    }
}

fn read_num<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset<'a> {
    let raw_num_regex = Regex::new(r"(\d|\.)+").unwrap();
    let caps = raw_num_regex.captures(&line[char_no-1..]).unwrap();
    let s = &caps[0];
    let s_vec: Vec<&str> = s.split(".").collect();
    let literal = &line[char_no-1..char_no+s.len()-1];
    match s_vec.len() {
        1 if s_vec[0].len() != 0 => make_tk_offset("IntConst", literal, line_no, char_no),
        2 if s_vec[0].len() != 0 && s_vec[1].len() != 0 => make_tk_offset("FloatingConst", literal, line_no, char_no),
        _ => make_err_offset(&format!("Invalid number literal `{}`", literal), literal, line_no, char_no),
    }
}

fn read_char<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset<'a> {
    let mut offset = 1;
    loop {
        if char_no + offset > line.len() {
            return make_err_offset("Encouter EOF while lexing a char literal.", &line[char_no-1..], line_no, char_no);
        }
        match &line[char_no+offset-1..char_no+offset] {
            "\\" => match &line[char_no+offset..char_no+offset+1] {
                "n" | "\"" | "'" => offset += 2,
                _ => offset += 1,
            },
            "'" => {
                offset += 1; 
                break
            },
            _ => offset += 1,
        }
    }
    let literal = &line[char_no-1..char_no+offset-1];
    let content = &line[char_no..char_no+offset-2];
    match content {
        c if c.len() == 1 && c.is_ascii() => make_tk_offset("CharConst", literal, line_no, char_no),
        "\\n" | "\\\"" | "\\'"  => make_tk_offset("CharConst", literal, line_no, char_no),
        _ => make_err_offset(&format!("Invalid character literal `{}`", literal), literal, line_no, char_no),
    }
}

fn read_str<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset<'a> {
    let mut offset = 1;
    loop {
        if char_no + offset > line.len() {
            return make_err_offset("Encouter EOF while lexing a str literal.", &line[char_no-1..], line_no, char_no);
        }
        match &line[char_no+offset-1..char_no+offset] {
            "\\" => match &line[char_no+offset..char_no+offset+1] {
                "n" | "\"" | "'" => offset += 2,
                _ => offset += 1,
            },
            "\"" => {
                offset += 1; 
                break
            },
            _ => offset += 1,
        }
    }
    let literal = &line[char_no-1..char_no+offset-1];
    make_tk_offset("StrConst", literal, line_no, char_no)
}

fn read_word<'a>(line: &'a str, line_no: usize, char_no: usize) -> TkErrOffset<'a> {
    let word_regex = Regex::new(r"[[:word:]]+").unwrap();
    let caps = word_regex.captures(&line[char_no-1..]).unwrap();
    let offset = caps[0].len();
    let literal = &line[char_no-1..char_no+offset-1];
    match literal {
        "T" => make_tk_offset("T", "T", line_no, char_no),
        "void" => make_tk_offset("Void", "void", line_no, char_no),
        "char" => make_tk_offset("Char", "char", line_no, char_no),
        "short" => make_tk_offset("Short", "short", line_no, char_no),
        "int" => make_tk_offset("Int", "int", line_no, char_no),
        "long" => make_tk_offset("Long", "long", line_no, char_no),
        "float" => make_tk_offset("Float", "float", line_no, char_no),
        "double" => make_tk_offset("Double", "double", line_no, char_no),
        "signed" => make_tk_offset("Signed", "signed", line_no, char_no),
        "unsigned" => make_tk_offset("Unsigned", "unsigned", line_no, char_no),

        "switch" => make_tk_offset("Switch", "switch", line_no, char_no),
        "case" => make_tk_offset("Case", "case", line_no, char_no),
        "default" => make_tk_offset("Default", "default", line_no, char_no),
        "if" => make_tk_offset("If", "if", line_no, char_no),
        "else" => make_tk_offset("Else", "else", line_no, char_no),
        "do" => make_tk_offset("Do", "do", line_no, char_no),
        "while" => make_tk_offset("While", "while", line_no, char_no),
        "for" => make_tk_offset("For", "for", line_no, char_no),
        "continue" => make_tk_offset("Continue", "continue", line_no, char_no),
        "break" => make_tk_offset("Break", "break", line_no, char_no),
        "return" => make_tk_offset("Return", "return", line_no, char_no),

        s => make_tk_offset("Ident", s, line_no, char_no),
    }
}

fn make_err_offset<'a>(message: &str, literal: &str, line_no: usize, char_no: usize) -> TkErrOffset<'a> {
    let error = Error::Lexing {
        message: message.to_string(),
        location: Location { line_no, char_no },
    };
    Err((error, literal.len()))
}

fn make_tk_offset<'a>(name: &str, literal: &'a str, line_no: usize, char_no: usize) -> TkErrOffset<'a> {
    let location = Location { line_no, char_no };
    let token = match name {
        "Ident" => Ident { literal, location },

        "IntConst" => IntConst { literal, location },
        "FloatingConst" => FloatingConst { literal, location },
        "CharConst" => CharConst { literal: &literal[1..literal.len()-1], location },
        "StrConst" => StrConst { literal: &literal[1..literal.len()-1], location },

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

        _ => panic!("Not impossible."),
    };
    Ok((token, literal.len()))
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn ident_const() {
        let source = "a 1 1.1 '\\n' '\\\"' '\\'' 'a' \"\\n\" \"\\\"\" \"\\'\" \"a\"";
        let expected_errors = vec![];
        let expected_token_stream = vec![
            Ident {
                literal: "a",
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
            IntConst {
                literal: "1",
                location: Location {
                    line_no: 1,
                    char_no: 3,
                },
            },
            FloatingConst {
                literal: "1.1",
                location: Location {
                    line_no: 1,
                    char_no: 5,
                },
            },
            CharConst {
                literal: "\\n",
                location: Location {
                    line_no: 1,
                    char_no: 9,
                },
            },
            CharConst {
                literal: "\\\"",
                location: Location {
                    line_no: 1,
                    char_no: 14,
                },
            },
            CharConst {
                literal: "\\'",
                location: Location {
                    line_no: 1,
                    char_no: 19,
                },
            },
            CharConst {
                literal: "a",
                location: Location {
                    line_no: 1,
                    char_no: 24,
                },
            },
            StrConst {
                literal: "\\n",
                location: Location {
                    line_no: 1,
                    char_no: 28,
                },
            },
            StrConst {
                literal: "\\\"",
                location: Location {
                    line_no: 1,
                    char_no: 33,
                },
            },
            StrConst {
                literal: "\\'",
                location: Location {
                    line_no: 1,
                    char_no: 38,
                },
            },
            StrConst {
                literal: "a",
                location: Location {
                    line_no: 1,
                    char_no: 43,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn type_specifier() {
        let source = "T void char short int long float double signed unsigned";
        let expected_errors = vec![];
        let expected_token_stream = vec![
            T {
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
            Void {
                location: Location {
                    line_no: 1,
                    char_no: 3,
                },
            },
            Char {
                location: Location {
                    line_no: 1,
                    char_no: 8,
                },
            },
            Short {
                location: Location {
                    line_no: 1,
                    char_no: 13,
                },
            },
            Int {
                location: Location {
                    line_no: 1,
                    char_no: 19,
                },
            },
            Long {
                location: Location {
                    line_no: 1,
                    char_no: 23,
                },
            },
            Float {
                location: Location {
                    line_no: 1,
                    char_no: 28,
                },
            },
            Double {
                location: Location {
                    line_no: 1,
                    char_no: 34,
                },
            },
            Signed {
                location: Location {
                    line_no: 1,
                    char_no: 41,
                },
            },
            Unsigned {
                location: Location {
                    line_no: 1,
                    char_no: 48,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn operators() {
        let source = "+ - * / % ++ -- =";
        let expected_errors = vec![];
        let expected_token_stream = vec![
            Plus {
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
            Minus {
                location: Location {
                    line_no: 1,
                    char_no: 3,
                },
            },
            Asterisk {
                location: Location {
                    line_no: 1,
                    char_no: 5,
                },
            },
            Slash {
                location: Location {
                    line_no: 1,
                    char_no: 7,
                },
            },
            Percent {
                location: Location {
                    line_no: 1,
                    char_no: 9,
                },
            },
            BiPlus {
                location: Location {
                    line_no: 1,
                    char_no: 11,
                },
            },
            BiMinus {
                location: Location {
                    line_no: 1,
                    char_no: 14,
                },
            },
            Equal {
                location: Location {
                    line_no: 1,
                    char_no: 17,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn relative_operators() {
        let source = "< > <= >= == != && || !";
        let expected_errors = vec![];
        let expected_token_stream = vec![
            Small {
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
            Large {
                location: Location {
                    line_no: 1,
                    char_no: 3,
                },
            },
            SmallEq {
                location: Location {
                    line_no: 1,
                    char_no: 5,
                },
            },
            LargeEq {
                location: Location {
                    line_no: 1,
                    char_no: 8,
                },
            },
            EqualTo {
                location: Location {
                    line_no: 1,
                    char_no: 11,
                },
            },
            NotEqualTo {
                location: Location {
                    line_no: 1,
                    char_no: 14,
                },
            },
            And {
                location: Location {
                    line_no: 1,
                    char_no: 17,
                },
            },
            Or {
                location: Location {
                    line_no: 1,
                    char_no: 20,
                },
            },
            Not {
                location: Location {
                    line_no: 1,
                    char_no: 23,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn combined_assignment_operators() {
        let source = "+= -= *= /= %=";
        let expected_errors = vec![];
        let expected_token_stream = vec![
            PlusEq {
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
            MinusEq {
                location: Location {
                    line_no: 1,
                    char_no: 4,
                },
            },
            AsteriskEq {
                location: Location {
                    line_no: 1,
                    char_no: 7,
                },
            },
            SlashEq {
                location: Location {
                    line_no: 1,
                    char_no: 10,
                },
            },
            PercentEq {
                location: Location {
                    line_no: 1,
                    char_no: 13,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn parens() {
        let source = "( ) [ ] { }";
        let expected_errors = vec![];
        let expected_token_stream = vec![
            LParen {
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
            RParen {
                location: Location {
                    line_no: 1,
                    char_no: 3,
                },
            },
            LBracket {
                location: Location {
                    line_no: 1,
                    char_no: 5,
                },
            },
            RBracket {
                location: Location {
                    line_no: 1,
                    char_no: 7,
                },
            },
            LBrace {
                location: Location {
                    line_no: 1,
                    char_no: 9,
                },
            },
            RBrace {
                location: Location {
                    line_no: 1,
                    char_no: 11,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn parentheses() {
        let source = "( ) [ ] { }";
        let expected_errors = vec![];
        let expected_token_stream = vec![
            LParen {
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
            RParen {
                location: Location {
                    line_no: 1,
                    char_no: 3,
                },
            },
            LBracket {
                location: Location {
                    line_no: 1,
                    char_no: 5,
                },
            },
            RBracket {
                location: Location {
                    line_no: 1,
                    char_no: 7,
                },
            },
            LBrace {
                location: Location {
                    line_no: 1,
                    char_no: 9,
                },
            },
            RBrace {
                location: Location {
                    line_no: 1,
                    char_no: 11,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn keywords() {
        let source = "switch case default if else do while for continue break return";
        let expected_errors = vec![];
        let expected_token_stream = vec![
            Switch {
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
            Case {
                location: Location {
                    line_no: 1,
                    char_no: 8,
                },
            },
            Default {
                location: Location {
                    line_no: 1,
                    char_no: 13,
                },
            },
            If {
                location: Location {
                    line_no: 1,
                    char_no: 21,
                },
            },
            Else {
                location: Location {
                    line_no: 1,
                    char_no: 24,
                },
            },
            Do {
                location: Location {
                    line_no: 1,
                    char_no: 29,
                },
            },
            While {
                location: Location {
                    line_no: 1,
                    char_no: 32,
                },
            },
            For {
                location: Location {
                    line_no: 1,
                    char_no: 38,
                },
            },
            Continue {
                location: Location {
                    line_no: 1,
                    char_no: 42,
                },
            },
            Break {
                location: Location {
                    line_no: 1,
                    char_no: 51,
                },
            },
            Return {
                location: Location {
                    line_no: 1,
                    char_no: 57,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn punctuation() {
        let source = ", : \n ;";
        let expected_errors = vec![];
        let expected_token_stream = vec![
            Comma {
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
            Colon {
                location: Location {
                    line_no: 1,
                    char_no: 3,
                },
            },
            Semicolon {
                location: Location {
                    line_no: 2,
                    char_no: 2,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn error() {
        let source = "1 & | .1 1. 1.1.1 \'ab\' \'";
        let expected_errors = vec![
            Error::Lexing {
                message: "Invalid operator `&`".to_string(),
                location: Location {
                    line_no: 1,
                    char_no: 3,
                },
            },
            Error::Lexing {
                message: "Invalid operator `|`".to_string(),
                location: Location {
                    line_no: 1,
                    char_no: 5,
                },
            },
            Error::Lexing {
                message: "Invalid number literal `.1`".to_string(),
                location: Location {
                    line_no: 1,
                    char_no: 7,
                },
            },
            Error::Lexing {
                message: "Invalid number literal `1.`".to_string(),
                location: Location {
                    line_no: 1,
                    char_no: 10,
                },
            },
            Error::Lexing {
                message: "Invalid number literal `1.1.1`".to_string(),
                location: Location {
                    line_no: 1,
                    char_no: 13,
                },
            },
            Error::Lexing {
                message: "Invalid character literal `\'ab\'`".to_string(),
                location: Location {
                    line_no: 1,
                    char_no: 19,
                },
            },
            Error::Lexing {
                message: "Encouter EOF while lexing a char literal.".to_string(),
                location: Location {
                    line_no: 1,
                    char_no: 24,
                },
            },
        ];
        let expected_token_stream = vec![
            IntConst {
                literal: "1",
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
        ];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }

    #[test]
    fn error_str_eof() {
        let source = "\"";
        let expected_errors = vec![
            Error::Lexing {
                message: "Encouter EOF while lexing a str literal.".to_string(),
                location: Location {
                    line_no: 1,
                    char_no: 1,
                },
            },
        ];
        let expected_token_stream = vec![];

        let mut errors = Vec::new();
        let token_stream = run(&source, &mut errors);
        assert_eq!(errors, expected_errors);
        assert_eq!(token_stream, expected_token_stream);
    }
}
