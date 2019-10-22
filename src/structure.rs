use std::fmt;

use indexmap::IndexMap;

pub trait Locate {
    fn locate(&self) -> Location;
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Location {
    pub line_no: usize,
    pub char_no: usize,
}

impl Location {
    pub fn new(line_no: usize, char_no: usize) -> Location {
        Location { line_no, char_no }
    }

    pub fn empty() -> Location {
        Location {
            line_no: 0,
            char_no: 0,
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line_no, self.char_no)
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Lexing { message: String, location: Location },
    Parsing { message: String, location: Location },
    Resolving { message: String, location: Location },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            Lexing { message, location } => write!(f, "({}) Lexing Error: {}", location, message),
            Parsing { message, location } => write!(f, "({}) Parsing Error: {}", location, message),
            Resolving { message, location } => {
                write!(f, "({}) Resolving Error: {}", location, message)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Ident {
        literal: &'a str,
        location: Location,
    },
    IntConst {
        literal: &'a str,
        location: Location,
    },
    FloatConst {
        literal: &'a str,
        location: Location,
    },
    CharConst {
        literal: &'a str,
        location: Location,
    },
    StrConst {
        literal: &'a str,
        location: Location,
    },

    T(Location),
    Void(Location),
    Char(Location),
    Short(Location),
    Int(Location),
    Long(Location),
    Float(Location),
    Double(Location),
    Signed(Location),
    Unsigned(Location),

    Plus(Location),
    Minus(Location),
    Asterisk(Location),
    Slash(Location),
    Percent(Location),
    BiPlus(Location),
    BiMinus(Location),
    Equal(Location),

    Small(Location),
    Large(Location),
    SmallEq(Location),
    LargeEq(Location),
    EqTo(Location),
    NotEqTo(Location),
    And(Location),
    Or(Location),
    Not(Location),

    PlusEq(Location),
    MinusEq(Location),
    AsteriskEq(Location),
    SlashEq(Location),
    PercentEq(Location),

    LParen(Location),
    RParen(Location),
    LBracket(Location),
    RBracket(Location),
    LBrace(Location),
    RBrace(Location),

    Switch(Location),
    Case(Location),
    Default(Location),
    If(Location),
    Else(Location),
    Do(Location),
    While(Location),
    For(Location),
    Continue(Location),
    Break(Location),
    Return(Location),

    Comma(Location),
    Colon(Location),
    Semicolon(Location),
}

impl<'a> Locate for Token<'a> {
    fn locate(&self) -> Location {
        use Token::*;

        match self {
            Ident { location, .. }
            | IntConst { location, .. }
            | FloatConst { location, .. }
            | CharConst { location, .. }
            | StrConst { location, .. } => *location,

            T(loc) | Void(loc) | Char(loc) | Short(loc) | Int(loc) | Long(loc) | Float(loc)
            | Double(loc) | Signed(loc) | Unsigned(loc) | Plus(loc) | Minus(loc)
            | Asterisk(loc) | Slash(loc) | Percent(loc) | BiPlus(loc) | BiMinus(loc)
            | Equal(loc) | Small(loc) | Large(loc) | SmallEq(loc) | LargeEq(loc) | EqTo(loc)
            | NotEqTo(loc) | And(loc) | Or(loc) | Not(loc) | PlusEq(loc) | MinusEq(loc)
            | AsteriskEq(loc) | SlashEq(loc) | PercentEq(loc) | LParen(loc) | RParen(loc)
            | LBracket(loc) | RBracket(loc) | LBrace(loc) | RBrace(loc) | Switch(loc)
            | Case(loc) | Default(loc) | If(loc) | Else(loc) | Do(loc) | While(loc) | For(loc)
            | Continue(loc) | Break(loc) | Return(loc) | Comma(loc) | Colon(loc)
            | Semicolon(loc) => *loc,
        }
    }
}

pub trait Array {
    fn set_array(&self, array_flag: bool, array_len: Option<usize>) -> Type;
    fn get_array(&self) -> (bool, Option<usize>);
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    T {
        dummy_flag: bool,
        array_flag: bool,
        array_len: Option<usize>,
        location: Location,
    },
    Void {
        array_flag: bool,
        array_len: Option<usize>,
        location: Location,
    },
    Char {
        array_flag: bool,
        array_len: Option<usize>,
        location: Location,
    },
    Short {
        signed_flag: bool,
        array_flag: bool,
        array_len: Option<usize>,
        location: Location,
    },
    Int {
        signed_flag: bool,
        array_flag: bool,
        array_len: Option<usize>,
        location: Location,
    },
    Long {
        signed_flag: bool,
        array_flag: bool,
        array_len: Option<usize>,
        location: Location,
    },
    Float {
        array_flag: bool,
        array_len: Option<usize>,
        location: Location,
    },
    Double {
        array_flag: bool,
        array_len: Option<usize>,
        location: Location,
    },
}

impl Array for Type {
    fn set_array(&self, array_flag: bool, array_len: Option<usize>) -> Type {
        use Type::*;

        match self {
            T {
                dummy_flag,
                location,
                ..
            } => T {
                dummy_flag: *dummy_flag,
                array_flag,
                array_len,
                location: *location,
            },
            Void { location, .. } => Void {
                array_flag,
                array_len,
                location: *location,
            },
            Char { location, .. } => Char {
                array_flag,
                array_len,
                location: *location,
            },
            Short {
                signed_flag,
                location,
                ..
            } => Short {
                signed_flag: *signed_flag,
                array_flag,
                array_len,
                location: *location,
            },
            Int {
                signed_flag,
                location,
                ..
            } => Int {
                signed_flag: *signed_flag,
                array_flag,
                array_len,
                location: *location,
            },
            Long {
                signed_flag,
                location,
                ..
            } => Long {
                signed_flag: *signed_flag,
                array_flag,
                array_len,
                location: *location,
            },
            Float { location, .. } => Float {
                array_flag,
                array_len,
                location: *location,
            },
            Double { location, .. } => Double {
                array_flag,
                array_len,
                location: *location,
            },
        }
    }

    fn get_array(&self) -> (bool, Option<usize>) {
        use Type::*;

        match self {
            T {
                array_flag,
                array_len,
                ..
            } => (*array_flag, *array_len),
            Void {
                array_flag,
                array_len,
                ..
            } => (*array_flag, *array_len),
            Char {
                array_flag,
                array_len,
                ..
            } => (*array_flag, *array_len),
            Short {
                array_flag,
                array_len,
                ..
            } => (*array_flag, *array_len),
            Int {
                array_flag,
                array_len,
                ..
            } => (*array_flag, *array_len),
            Long {
                array_flag,
                array_len,
                ..
            } => (*array_flag, *array_len),
            Float {
                array_flag,
                array_len,
                ..
            } => (*array_flag, *array_len),
            Double {
                array_flag,
                array_len,
                ..
            } => (*array_flag, *array_len),
        }
    }
}

impl<'a> Locate for Type {
    fn locate(&self) -> Location {
        use Type::*;

        match self {
            T { location, .. }
            | Void { location, .. }
            | Char { location, .. }
            | Short { location, .. }
            | Int { location, .. }
            | Long { location, .. }
            | Float { location, .. }
            | Double { location, .. } => *location,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Ident {
        value: &'a str,
        location: Location,
    },
    IntConst {
        value: i128,
        location: Location,
    },
    FloatConst {
        value: f64,
        location: Location,
    },
    CharConst {
        value: &'a str,
        location: Location,
    },
    StrConst {
        value: &'a str,
        location: Location,
    },
    Prefix {
        operator: &'a str,
        expression: Box<Expression<'a>>,
        location: Location,
    },
    Infix {
        left: Box<Expression<'a>>,
        operator: &'a str,
        right: Box<Expression<'a>>,
    },
    Suffix {
        operator: &'a str,
        expression: Box<Expression<'a>>,
    },
    Index {
        expression: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
    },
    Call {
        expression: Box<Expression<'a>>,
        arguments: Vec<Box<Expression<'a>>>,
    },
    InitList {
        expressions: Vec<Box<Expression<'a>>>,
        location: Location,
    },
}

impl<'a> Locate for Expression<'a> {
    fn locate(&self) -> Location {
        use Expression::*;

        match self {
            Ident { location, .. }
            | IntConst { location, .. }
            | FloatConst { location, .. }
            | CharConst { location, .. }
            | StrConst { location, .. }
            | Prefix { location, .. }
            | InitList { location, .. } => *location,
            Infix { left, .. } => left.locate(),
            Suffix { expression, .. } => expression.locate(),
            Index { expression, .. } => expression.locate(),
            Call { expression, .. } => expression.locate(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Continue(Location),
    Break(Location),
    Expr(Expression<'a>),
    Return {
        expr: Option<Expression<'a>>,
        location: Location,
    },
    Block {
        statements: Vec<Box<Statement<'a>>>,
        location: Location,
    },
    Def {
        declarators: Vec<(Type, &'a str, Option<Expression<'a>>)>,
        location: Location,
    },
    While {
        condition: Expression<'a>,
        body: Box<Statement<'a>>,
        location: Location,
    },
    Do {
        condition: Expression<'a>,
        body: Box<Statement<'a>>,
        location: Location,
    },
    For {
        initialization: Option<Expression<'a>>,
        condition: Option<Expression<'a>>,
        increment: Option<Expression<'a>>,
        body: Box<Statement<'a>>,
        location: Location,
    },
    If {
        condition: Expression<'a>,
        body: Box<Statement<'a>>,
        alternative: Option<Box<Statement<'a>>>,
        location: Location,
    },
    Switch {
        expression: Expression<'a>,
        branches: Vec<(Expression<'a>, Vec<Box<Statement<'a>>>)>,
        default: Option<Vec<Box<Statement<'a>>>>,
        location: Location,
    },
}

impl<'a> Locate for Statement<'a> {
    fn locate(&self) -> Location {
        use Statement::*;

        match self {
            Continue(loc) | Break(loc) => *loc,
            Expr(expr) => expr.locate(),
            Return { location, .. } | Block { location, .. } | Def { location, .. } => *location,
            While { location, .. }
            | Do { location, .. }
            | For { location, .. }
            | If { location, .. }
            | Switch { location, .. } => *location,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub r#type: Type,
    pub name: &'a str,
    pub parameters: IndexMap<&'a str, Type>,
    pub body: Statement<'a>,
    pub location: Location,
}

impl<'a> Locate for Function<'a> {
    fn locate(&self) -> Location {
        self.location
    }
}
