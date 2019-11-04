//! Basic structures which will be used among multiple modules.

use std::cmp::PartialEq;
use std::fmt;

use colored::*;
use indexmap::IndexMap;

/// Types that can locate itself in the source file.
///
/// All sturctures in this module except `Error` implement this trait.
pub trait Locate {
    /// Return the location in the source file.
    fn locate(&self) -> Location;
}

/// Flag for array types.
///
/// Only `Type` implements this trait.
pub trait Array {
    /// Change the array flag of the `Type` object.
    ///
    /// if `array_flag` is `true`, this object represents an array type
    /// with the length of `array_len`. `array_len` is `None` either
    /// because this object does not represent an array type or the length
    /// is unspecified in the definition.
    fn set_array(&self, array_flag: bool, array_len: Option<usize>) -> Type;

    /// Retrieve `(array_flag, array_len)` from the object.
    fn get_array(&self) -> (bool, Option<usize>);
}

/// Flag for pointer types.
///
/// Only `Type` implements this trait.
pub trait Pointer {
    /// Change the pointer flag of the `Type` object.
    ///
    /// If `pointer_flag` is `true`, this object represents an pointer type.
    fn set_pointer_flag(&self, pointer_flag: bool) -> Type;

    /// Retrieve `pointer_flag` from the Object.
    fn get_pointer_flag(&self) -> bool;
}

/// Represent all possible errors may occur during the transpilation.
///
/// Notice the serializer should never produce an error.
#[derive(Debug, PartialEq)]
pub enum Error {
    Preprocessing {
        message: String,
        location: Location,
    },
    /// Errors that occurs during lexing.
    Lexing {
        message: String,
        location: Location,
    },

    /// Errors that occurs during parsing.
    Parsing {
        message: String,
        location: Location,
    },

    /// Errors that occurs during type inference.
    Resolving {
        message: String,
        location: Location,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            Preprocessing { message, location } => write!(
                f,
                "{} ({}): {}",
                "Preprocessing Error".red(),
                location,
                message
            ),
            Lexing { message, location } => {
                write!(f, "{} ({}): {}", "Lexing Error".red(), location, message)
            }
            Parsing { message, location } => {
                write!(f, "{} ({}): {}", "Parsing Error".red(), location, message)
            }
            Resolving { message, location } => {
                write!(f, "{} ({}): {}", "Resolving Error".red(), location, message)
            }
        }
    }
}

/// Represent a specific location in the source file.
#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub line_no: usize,
    pub char_no: usize,
}

impl Location {
    pub fn new(line_no: usize, char_no: usize) -> Location {
        Location { line_no, char_no }
    }

    /// Constructor used in dummy `Type` objects.
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

impl PartialEq for Location {
    fn eq(&self, _other: &Self) -> bool {
        // `Location` will never be directly compared, and we want other
        // structures which only different in `Location` to be compared equal.
        true
    }
}

/// Tokens used by the lexer.
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
    Struct(Location),

    Ampersand(Location),
    Dot(Location),
    Arrow(Location),
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
            | Continue(loc) | Break(loc) | Return(loc) | Struct(loc) | Ampersand(loc)
            | Dot(loc) | Arrow(loc) | Comma(loc) | Colon(loc) | Semicolon(loc) => *loc,
        }
    }
}

/// AST nodes for type declarations.
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    T {
        array_flag: bool,
        array_len: Option<usize>,
        pointer_flag: bool,
        location: Location,
    },
    Void {
        array_flag: bool,
        array_len: Option<usize>,
        pointer_flag: bool,
        location: Location,
    },
    Char {
        array_flag: bool,
        array_len: Option<usize>,
        pointer_flag: bool,
        location: Location,
    },
    Short {
        signed_flag: bool,
        array_flag: bool,
        array_len: Option<usize>,
        pointer_flag: bool,
        location: Location,
    },
    Int {
        signed_flag: bool,
        array_flag: bool,
        array_len: Option<usize>,
        pointer_flag: bool,
        location: Location,
    },
    Long {
        signed_flag: bool,
        array_flag: bool,
        array_len: Option<usize>,
        pointer_flag: bool,
        location: Location,
    },
    Float {
        array_flag: bool,
        array_len: Option<usize>,
        pointer_flag: bool,
        location: Location,
    },
    Double {
        array_flag: bool,
        array_len: Option<usize>,
        pointer_flag: bool,
        location: Location,
    },
    Struct {
        name: String,
        members: IndexMap<String, Type>,
        array_flag: bool,
        array_len: Option<usize>,
        pointer_flag: bool,
        location: Location,
    },
}

impl Array for Type {
    fn set_array(&self, array_flag: bool, array_len: Option<usize>) -> Type {
        use Type::*;

        match self {
            T {
                pointer_flag,
                location,
                ..
            } => T {
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: *location,
            },
            Void {
                pointer_flag,
                location,
                ..
            } => Void {
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: *location,
            },
            Char {
                pointer_flag,
                location,
                ..
            } => Char {
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: *location,
            },
            Short {
                signed_flag,
                pointer_flag,
                location,
                ..
            } => Short {
                signed_flag: *signed_flag,
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: *location,
            },
            Int {
                signed_flag,
                pointer_flag,
                location,
                ..
            } => Int {
                signed_flag: *signed_flag,
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: *location,
            },
            Long {
                signed_flag,
                pointer_flag,
                location,
                ..
            } => Long {
                signed_flag: *signed_flag,
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: *location,
            },
            Float {
                pointer_flag,
                location,
                ..
            } => Float {
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: *location,
            },
            Double {
                pointer_flag,
                location,
                ..
            } => Double {
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: *location,
            },
            Struct {
                name,
                members,
                pointer_flag,
                location,
                ..
            } => Struct {
                name: name.to_string(),
                members: members.clone(),
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
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
            Struct {
                array_flag,
                array_len,
                ..
            } => (*array_flag, *array_len),
        }
    }
}

impl Pointer for Type {
    fn set_pointer_flag(&self, pointer_flag: bool) -> Type {
        use Type::*;

        match self.clone() {
            T {
                array_flag,
                array_len,
                location,
                ..
            } => T {
                array_flag,
                array_len,
                pointer_flag,
                location,
            },
            Void {
                array_flag,
                array_len,
                location,
                ..
            } => Void {
                array_flag,
                array_len,
                pointer_flag,
                location,
            },
            Char {
                array_flag,
                array_len,
                location,
                ..
            } => Char {
                array_flag,
                array_len,
                pointer_flag,
                location,
            },
            Short {
                signed_flag,
                array_flag,
                array_len,
                location,
                ..
            } => Short {
                signed_flag,
                array_flag,
                array_len,
                pointer_flag,
                location,
            },
            Int {
                signed_flag,
                array_flag,
                array_len,
                location,
                ..
            } => Int {
                signed_flag,
                array_flag,
                array_len,
                pointer_flag,
                location,
            },
            Long {
                signed_flag,
                array_flag,
                array_len,
                location,
                ..
            } => Long {
                signed_flag,
                array_flag,
                array_len,
                pointer_flag,
                location,
            },
            Float {
                array_flag,
                array_len,
                location,
                ..
            } => Float {
                array_flag,
                array_len,
                pointer_flag,
                location,
            },
            Double {
                array_flag,
                array_len,
                location,
                ..
            } => Double {
                array_flag,
                array_len,
                pointer_flag,
                location,
            },
            Struct {
                name,
                members,
                array_flag,
                array_len,
                location,
                ..
            } => Struct {
                name,
                members,
                array_flag,
                array_len,
                pointer_flag,
                location,
            },
        }
    }

    fn get_pointer_flag(&self) -> bool {
        use Type::*;

        match self {
            T { pointer_flag, .. } => *pointer_flag,
            Void { pointer_flag, .. } => *pointer_flag,
            Char { pointer_flag, .. } => *pointer_flag,
            Short { pointer_flag, .. } => *pointer_flag,
            Int { pointer_flag, .. } => *pointer_flag,
            Long { pointer_flag, .. } => *pointer_flag,
            Float { pointer_flag, .. } => *pointer_flag,
            Double { pointer_flag, .. } => *pointer_flag,
            Struct { pointer_flag, .. } => *pointer_flag,
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
            | Double { location, .. }
            | Struct { location, .. } => *location,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;

        match self {
            T { .. } => write!(f, "T"),
            Void { .. } => write!(f, "void"),
            Char { .. } => write!(f, "char"),
            Short { .. } => write!(f, "short"),
            Int { .. } => write!(f, "int"),
            Long { .. } => write!(f, "long"),
            Float { .. } => write!(f, "float"),
            Double { .. } => write!(f, "double"),
            Struct { name, .. } => write!(f, "struct {}", name),
        }
    }
}

impl Type {
    /// Constructor for dummy `Type` objects in the resolver.
    pub fn make_dummy(name: &str, signed_flag: bool, array_flag: bool, pointer_flag: bool) -> Type {
        use Type::*;

        match name {
            "T" => Type::T {
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::empty(),
            },
            "Char" => Type::Char {
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::empty(),
            },
            "Short" => Short {
                signed_flag,
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::empty(),
            },
            "Int" => Int {
                signed_flag,
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::empty(),
            },
            "Long" => Long {
                signed_flag,
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::empty(),
            },
            "Float" => Float {
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::empty(),
            },
            "Double" => Double {
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::empty(),
            },
            _ => unreachable!(),
        }
    }
}

/// AST nodes for expressions.
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
        arguments: Vec<Expression<'a>>,
    },
    InitList {
        expressions: Vec<Expression<'a>>,
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

/// AST nodes for statements.
#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Continue(Location),
    Break(Location),
    Expr(Expression<'a>),
    Return {
        expression: Option<Expression<'a>>,
        location: Location,
    },
    Block {
        statements: Vec<Statement<'a>>,
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
        branches: Vec<(Expression<'a>, Vec<Statement<'a>>)>,
        default: Option<Vec<Statement<'a>>>,
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

/// AST nodes for functions.
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
