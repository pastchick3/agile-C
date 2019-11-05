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
                "{} {}: {}",
                location,
                "Preprocessing Error".red(),
                message
            ),
            Lexing { message, location } => {
                write!(f, "{} {}: {}", location, "Lexing Error".red(), message)
            }
            Parsing { message, location } => {
                write!(f, "{} {}: {}", location, "Parsing Error".red(), message)
            }
            Resolving { message, location } => {
                write!(f, "{} {}: {}", location, "Resolving Error".red(), message)
            }
        }
    }
}

/// Represent a specific location in the source file.
#[derive(Debug, Clone, Default)]
pub struct Location {
    pub file_name: String,
    pub line_no: usize,
    pub char_no: usize,
}

impl Location {
    pub fn new(file_name: &str, line_index: usize, char_index: usize) -> Location {
        Location {
            file_name: file_name.to_string(),
            line_no: line_index + 1,
            char_no: char_index + 1,
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({}:{})", self.file_name, self.line_no, self.char_no)
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
pub enum Token {
    Ident { literal: String, location: Location },
    IntConst { literal: String, location: Location },
    FloatConst { literal: String, location: Location },
    CharConst { literal: String, location: Location },
    StrConst { literal: String, location: Location },

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

impl Locate for Token {
    fn locate(&self) -> Location {
        use Token::*;

        match self {
            Ident { location, .. }
            | IntConst { location, .. }
            | FloatConst { location, .. }
            | CharConst { location, .. }
            | StrConst { location, .. } => location.clone(),

            T(loc) | Void(loc) | Char(loc) | Short(loc) | Int(loc) | Long(loc) | Float(loc)
            | Double(loc) | Signed(loc) | Unsigned(loc) | Plus(loc) | Minus(loc)
            | Asterisk(loc) | Slash(loc) | Percent(loc) | BiPlus(loc) | BiMinus(loc)
            | Equal(loc) | Small(loc) | Large(loc) | SmallEq(loc) | LargeEq(loc) | EqTo(loc)
            | NotEqTo(loc) | And(loc) | Or(loc) | Not(loc) | PlusEq(loc) | MinusEq(loc)
            | AsteriskEq(loc) | SlashEq(loc) | PercentEq(loc) | LParen(loc) | RParen(loc)
            | LBracket(loc) | RBracket(loc) | LBrace(loc) | RBrace(loc) | Switch(loc)
            | Case(loc) | Default(loc) | If(loc) | Else(loc) | Do(loc) | While(loc) | For(loc)
            | Continue(loc) | Break(loc) | Return(loc) | Struct(loc) | Ampersand(loc)
            | Dot(loc) | Arrow(loc) | Comma(loc) | Colon(loc) | Semicolon(loc) => loc.clone(),
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
                location: location.clone(),
            },
            Void {
                pointer_flag,
                location,
                ..
            } => Void {
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: location.clone(),
            },
            Char {
                pointer_flag,
                location,
                ..
            } => Char {
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: location.clone(),
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
                location: location.clone(),
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
                location: location.clone(),
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
                location: location.clone(),
            },
            Float {
                pointer_flag,
                location,
                ..
            } => Float {
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: location.clone(),
            },
            Double {
                pointer_flag,
                location,
                ..
            } => Double {
                array_flag,
                array_len,
                pointer_flag: *pointer_flag,
                location: location.clone(),
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
                location: location.clone(),
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

impl Locate for Type {
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
            | Struct { location, .. } => location.clone(),
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
                location: Location::default(),
            },
            "Char" => Type::Char {
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::default(),
            },
            "Short" => Short {
                signed_flag,
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::default(),
            },
            "Int" => Int {
                signed_flag,
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::default(),
            },
            "Long" => Long {
                signed_flag,
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::default(),
            },
            "Float" => Float {
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::default(),
            },
            "Double" => Double {
                array_flag,
                array_len: None,
                pointer_flag,
                location: Location::default(),
            },
            _ => unreachable!(),
        }
    }
}

/// AST nodes for expressions.
#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident {
        value: String,
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
        value: String,
        location: Location,
    },
    StrConst {
        value: String,
        location: Location,
    },
    Prefix {
        operator: &'static str,
        expression: Box<Expression>,
        location: Location,
    },
    Infix {
        left: Box<Expression>,
        operator: &'static str,
        right: Box<Expression>,
    },
    Suffix {
        operator: &'static str,
        expression: Box<Expression>,
    },
    Index {
        expression: Box<Expression>,
        index: Box<Expression>,
    },
    Call {
        expression: Box<Expression>,
        arguments: Vec<Expression>,
    },
    InitList {
        expressions: Vec<Expression>,
        location: Location,
    },
}

impl Locate for Expression {
    fn locate(&self) -> Location {
        use Expression::*;

        match self {
            Ident { location, .. }
            | IntConst { location, .. }
            | FloatConst { location, .. }
            | CharConst { location, .. }
            | StrConst { location, .. }
            | Prefix { location, .. }
            | InitList { location, .. } => location.clone(),
            Infix { left, .. } => left.locate(),
            Suffix { expression, .. } => expression.locate(),
            Index { expression, .. } => expression.locate(),
            Call { expression, .. } => expression.locate(),
        }
    }
}

/// AST nodes for statements.
#[derive(Debug, PartialEq)]
pub enum Statement {
    Null(Location),
    Continue(Location),
    Break(Location),
    Expr(Expression),
    Return {
        expression: Option<Expression>,
        location: Location,
    },
    Block {
        statements: Vec<Statement>,
        location: Location,
    },
    Def {
        declarators: Vec<(Type, String, Option<Expression>)>,
        location: Location,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
        location: Location,
    },
    Do {
        condition: Expression,
        body: Box<Statement>,
        location: Location,
    },
    For {
        initialization: Option<Expression>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Box<Statement>,
        location: Location,
    },
    If {
        condition: Expression,
        body: Box<Statement>,
        alternative: Option<Box<Statement>>,
        location: Location,
    },
    Switch {
        expression: Expression,
        branches: Vec<(Expression, Vec<Statement>)>,
        default: Option<Vec<Statement>>,
        location: Location,
    },
}

impl Locate for Statement {
    fn locate(&self) -> Location {
        use Statement::*;

        match self {
            Null(loc) | Continue(loc) | Break(loc) => loc.clone(),
            Expr(expr) => expr.locate(),
            Return { location, .. } | Block { location, .. } | Def { location, .. } => {
                location.clone()
            }
            While { location, .. }
            | Do { location, .. }
            | For { location, .. }
            | If { location, .. }
            | Switch { location, .. } => location.clone(),
        }
    }
}

/// AST nodes for functions.
#[derive(Debug, PartialEq)]
pub struct Function {
    pub r#type: Type,
    pub name: String,
    pub parameters: IndexMap<String, Type>,
    pub body: Statement,
    pub location: Location,
}

impl Locate for Function {
    fn locate(&self) -> Location {
        self.location.clone()
    }
}
