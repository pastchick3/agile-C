/* * * * * * * * * *
 * Utilities
 * 
 * * * * * * * * * *
 */

#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    pub line_no: usize,
    pub char_no: usize,
}

impl Location {
    pub fn new(line_no: usize, char_no: usize) -> Location {
        Location { line_no, char_no }
    }

    pub fn empty() -> Location {
        Location { line_no: 0, char_no: 0 }
    }
}

pub trait Locate {
    fn locate(&self) -> Location;
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Lexing { message: String, location: Location },
    Parsing { message: String, location: Location },
}

/* * * * * * * * * *
 * Lexing
 * 
 * * * * * * * * * *
 */

impl Locate for Error {
    fn locate(&self) -> Location {
        use Error::*;

        match self {
            Lexing { message: _, location } => location.clone(),
            Parsing { message: _, location } => location.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Ident { literal: &'a str, location: Location },

    IntConst { literal: &'a str, location: Location },
    FloatingConst { literal: &'a str, location: Location },
    CharConst { literal: &'a str, location: Location },
    StrConst { literal: &'a str, location: Location },

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
    EqualTo(Location),
    NotEqualTo(Location),
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
            Ident { literal: _, location }
            | IntConst { literal: _, location }
            | FloatingConst { literal: _, location }
            | CharConst { literal: _, location }
            | StrConst { literal: _, location } => location.clone(),
            
            T(loc)
            | Void(loc)
            | Char(loc)
            | Short(loc)
            | Int(loc)
            | Long(loc)
            | Float(loc)
            | Double(loc)
            | Signed(loc)
            | Unsigned(loc)

            | Plus(loc)
            | Minus(loc)
            | Asterisk(loc)
            | Slash(loc)
            | Percent(loc)
            | BiPlus(loc)
            | BiMinus(loc)
            | Equal(loc)

            | Small(loc)
            | Large(loc)
            | SmallEq(loc)
            | LargeEq(loc)
            | EqualTo(loc)
            | NotEqualTo(loc)
            | And(loc)
            | Or(loc)
            | Not(loc)
            
            | PlusEq(loc)
            | MinusEq(loc)
            | AsteriskEq(loc)
            | SlashEq(loc)
            | PercentEq(loc)

            | LParen(loc)
            | RParen(loc)
            | LBracket(loc)
            | RBracket(loc)
            | LBrace(loc)
            | RBrace(loc)

            | Switch(loc)
            | Case(loc)
            | Default(loc)
            | If(loc)
            | Else(loc)
            | Do(loc)
            | While(loc)
            | For(loc)
            | Continue(loc)
            | Break(loc)
            | Return(loc)

            | Comma(loc)
            | Colon(loc)
            | Semicolon(loc) => loc.clone(),
        }
    }
}

/* * * * * * * * * *
 * Parsing
 * 
 * * * * * * * * * *
 */

#[derive(Debug, PartialEq)]
pub enum Type {
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
}

impl Locate for Type {
    fn locate(&self) -> Location {
        use Type::*;
        match self {
            T(loc)
            | Void(loc)
            | Char(loc)
            | Short(loc)
            | Int(loc)
            | Long(loc)
            | Float(loc)
            | Double(loc)
            | Signed(loc)
            | Unsigned(loc) => loc.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Ident { value: &'a str, location: Location },
    IntConst { value: i128, location: Location },
    FloatingConst { value: f64, location: Location },
    CharConst { value: &'a str, location: Location },
    StrConst { value: &'a str, location: Location },
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
        name: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
    },
    Call {
        name: Box<Expression<'a>>,
        arguments: Vec<Box<Expression<'a>>>,
    },
}

impl<'a> Locate for Expression<'a> {
    fn locate(&self) -> Location {
        use Expression::*;

        match self {
            Ident { value: _, location }
            | IntConst { value: _, location }
            | FloatingConst { value: _, location }
            | CharConst { value: _, location }
            | StrConst { value: _, location }
            | Prefix { operator: _, expression: _, location } => location.clone(),

            Infix { left, operator: _, right: _ } => left.locate(),
            Suffix { operator: _, expression } => expression.locate(),
            Call { name, arguments: _ } => name.locate(),
            Index { name, index: _ } => name.locate(),
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
        types: Vec<Type>,
        declarators: Vec<(Expression<'a>, Option<Expression<'a>>)>,
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
            Continue(loc)
            | Break(loc) => loc.clone(),

            Expr(expr) => expr.locate(),

            Return { expr: _, location }
            | Block { statements: _, location }
            | Def { types: _, declarators: _, location }
            | While { condition: _, body: _, location }
            | Do { condition:_, body: _, location }
            | For { initialization:_, condition: _, increment: _, body: _, location }
            | If { condition:_, body: _, alternative: _, location }
            | Switch { expression: _, branches:_, default: _, location } => location.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub types: Vec<Type>,
    pub name: Expression<'a>,
    pub parameters: Vec<(Vec<Type>, Expression<'a>)>,
    pub body: Statement<'a>,
    pub location: Location,
}

impl<'a> Locate for Function<'a> {
    fn locate(&self) -> Location {
        self.location.clone()
    }
}
