#[derive(Debug, PartialEq)]
pub struct Location {
    pub line_no: usize,
    pub char_no: usize,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Lexing { message: String, location: Location },
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Ident { literal: &'a str, location: Location },

    IntConst { literal: &'a str, location: Location },
    FloatingConst { literal: &'a str, location: Location },
    CharConst { literal: &'a str, location: Location },
    StrConst { literal: &'a str, location: Location },

    T { location: Location },
    Void { location: Location },
    Char { location: Location },
    Short { location: Location },
    Int { location: Location },
    Long { location: Location },
    Float { location: Location },
    Double { location: Location },
    Signed { location: Location },
    Unsigned { location: Location },

    Plus { location: Location },
    Minus { location: Location },
    Asterisk { location: Location },
    Slash { location: Location },
    Percent { location: Location },
    BiPlus { location: Location },
    BiMinus { location: Location },
    Equal { location: Location },

    Small { location: Location },
    Large { location: Location },
    SmallEq { location: Location },
    LargeEq { location: Location },
    EqualTo { location: Location },
    NotEqualTo { location: Location },
    And { location: Location },
    Or { location: Location },
    Not { location: Location },
    
    PlusEq { location: Location },
    MinusEq { location: Location },
    AsteriskEq { location: Location },
    SlashEq { location: Location },
    PercentEq { location: Location },

    LParen { location: Location },
    RParen { location: Location },
    LBracket { location: Location },
    RBracket { location: Location },
    LBrace { location: Location },
    RBrace { location: Location },

    Switch { location: Location },
    Case { location: Location },
    Default { location: Location },
    If { location: Location },
    Else { location: Location },
    Do { location: Location },
    While { location: Location },
    For { location: Location },
    Continue { location: Location },
    Break { location: Location },
    Return { location: Location },

    Comma { location: Location },
    Colon { location: Location },
    Semicolon { location: Location },
}
