//! Basic structures which will be used among multiple modules.

use std::cell::RefCell;
use std::cmp::PartialEq;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use colored::*;
use indexmap::IndexMap;

/// Represent all possible errors may occur during the transpilation.
#[derive(Debug, PartialEq)]
pub enum Error {
    Preprocessing { message: String, location: Location },
    Lexing { message: String, location: Location },
    Parsing { message: String, location: Location },
    Resolving { message: String, location: Location },
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
#[derive(Debug, Clone, Eq, Default)]
pub struct Location {
    pub file_name: String,
    pub line_no: usize, // start from 1
    pub char_no: usize, // start from 1
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

/// `Location` will never be directly compared, and we want other
/// structures which only different in `Location` to be compared equal.
impl PartialEq for Location {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

/// Since we implement `PartialEq`, we cannot derive `Hash`.
/// Instead We must implement both `PartialEq` and `Hash` to uphold
/// `k1 == k2 -> hash(k1) == hash(k2)`
impl Hash for Location {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

/// Types that can locate itself in the source file.
///
/// `Token`, `Type`, `Expression`, `Statement`, and `Function` implement this trait.
pub trait Locate {
    fn locate(&self) -> Location;
}

/// Tokens used by the lexer and the parser.
#[derive(Debug, PartialEq)]
pub enum Token {
    Ident { literal: String, location: Location },
    IntConst { literal: String, location: Location },
    FloatConst { literal: String, location: Location },
    CharConst { literal: String, location: Location },
    StrConst { literal: String, location: Location },
    Comment { literal: String, location: Location },

    T(Location), // generic type for internal usage only
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
    Equal(Location), // "="

    Small(Location),
    Large(Location),
    SmallEq(Location),
    LargeEq(Location),
    EqTo(Location), // "=="
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
    Default_(Location),
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
    Ellipsis(Location),
}

impl Locate for Token {
    fn locate(&self) -> Location {
        use Token::*;

        match self {
            Ident { location, .. }
            | IntConst { location, .. }
            | FloatConst { location, .. }
            | CharConst { location, .. }
            | StrConst { location, .. }
            | Comment { location, .. } => location.clone(),

            T(loc) | Void(loc) | Char(loc) | Short(loc) | Int(loc) | Long(loc) | Float(loc)
            | Double(loc) | Signed(loc) | Unsigned(loc) | Plus(loc) | Minus(loc)
            | Asterisk(loc) | Slash(loc) | Percent(loc) | BiPlus(loc) | BiMinus(loc)
            | Equal(loc) | Small(loc) | Large(loc) | SmallEq(loc) | LargeEq(loc) | EqTo(loc)
            | NotEqTo(loc) | And(loc) | Or(loc) | Not(loc) | PlusEq(loc) | MinusEq(loc)
            | AsteriskEq(loc) | SlashEq(loc) | PercentEq(loc) | LParen(loc) | RParen(loc)
            | LBracket(loc) | RBracket(loc) | LBrace(loc) | RBrace(loc) | Switch(loc)
            | Case(loc) | Default_(loc) | If(loc) | Else(loc) | Do(loc) | While(loc) | For(loc)
            | Continue(loc) | Break(loc) | Return(loc) | Struct(loc) | Ampersand(loc)
            | Dot(loc) | Arrow(loc) | Comma(loc) | Colon(loc) | Semicolon(loc) | Ellipsis(loc) => {
                loc.clone()
            }
        }
    }
}

/// The result of comparing two types.
///
/// Please refer to the README file for what these relationship mean.
#[derive(Debug)]
pub enum TypeRelationship {
    Sub,
    Equal,
    Super,
    None,
}

/// AST nodes for types.
///
/// Please refer to the README file for the type hierarchy.
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Any,
    Nothing,
    AnyRef,
    Null,
    T {
        array_flag: bool,   // whether it is a array type
        pointer_flag: bool, // whether it is a array type
        location: Option<Location>,
        specialized: Option<Box<Type>>, // whether it has been specialized to a concrete type
    },
    Void(Option<Location>),
    Char {
        array_flag: bool,
        pointer_flag: bool,
        location: Option<Location>,
    },
    Byte {
        array_flag: bool,
        pointer_flag: bool,
        location: Option<Location>,
    },
    Short {
        signed_flag: bool,
        array_flag: bool,
        pointer_flag: bool,
        location: Option<Location>,
    },
    Int {
        signed_flag: bool,
        array_flag: bool,
        pointer_flag: bool,
        location: Option<Location>,
    },
    Long {
        signed_flag: bool,
        array_flag: bool,
        pointer_flag: bool,
        location: Option<Location>,
    },
    Float {
        array_flag: bool,
        pointer_flag: bool,
        location: Option<Location>,
    },
    Double {
        array_flag: bool,
        pointer_flag: bool,
        location: Option<Location>,
    },
    Struct {
        name: String,
        members: IndexMap<String, Type>,
        array_flag: bool,
        pointer_flag: bool,
        location: Option<Location>,
    },
}

impl Locate for Type {
    fn locate(&self) -> Location {
        use Type::*;

        match self {
            Any | Nothing | AnyRef | Null => panic!("Try to locate a dummy type."),
            T { location, .. }
            | Void(location)
            | Char { location, .. }
            | Byte { location, .. }
            | Short { location, .. }
            | Int { location, .. }
            | Long { location, .. }
            | Float { location, .. }
            | Double { location, .. }
            | Struct { location, .. } => location.clone().expect("Try to locate a dummy type."),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;

        let array_marker = |&flag| if flag { "[]" } else { "" };
        let pointer_marker = |&flag| if flag { "*" } else { "" };

        match self {
            Any { .. } => write!(f, "Any"),
            Nothing { .. } => write!(f, "Nothing"),
            AnyRef { .. } => write!(f, "AnyRef"),
            Null { .. } => write!(f, "Null"),
            T {
                array_flag,
                pointer_flag,
                specialized: Some(type_),
                ..
            } => write!(f, "T({})", type_),
            T {
                array_flag,
                pointer_flag,
                specialized: None,
                ..
            } => write!(
                f,
                "{}T{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Void(_) => write!(f, "void"),
            Char {
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}char{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Byte {
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}byte{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Short {
                signed_flag: true,
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}short{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Short {
                signed_flag: false,
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}unsigned short{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Int {
                signed_flag: true,
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}int{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Int {
                signed_flag: false,
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}unsigned int{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Long {
                signed_flag: true,
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}long{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Long {
                signed_flag: false,
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}unsigned long{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Float {
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}float{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Double {
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}double{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Double {
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}double{}",
                pointer_marker(pointer_flag),
                array_marker(array_flag)
            ),
            Struct {
                name,
                array_flag,
                pointer_flag,
                ..
            } => write!(
                f,
                "{}struct {}{}",
                pointer_marker(pointer_flag),
                name,
                array_marker(array_flag)
            ),
        }
    }
}

impl Type {
    fn set_array_flag(&self, array_flag: bool) -> Type {
        use Type::*;

        match self.clone() {
            Any => panic!("Type `Any` cannot be an array."),
            Nothing => panic!("Type `Nothing` cannot be an array."),
            AnyRef => panic!("Type `AnyRef` cannot be an array."),
            Null => panic!("Type `Null` cannot be an array."),
            T {
                pointer_flag,
                location,
                specialized: Some(type_),
                ..
            } => T {
                array_flag,
                pointer_flag,
                location,
                specialized: Some(Box::new(type_.set_array_flag(array_flag))),
            },
            T {
                pointer_flag,
                location,
                specialized: None,
                ..
            } => T {
                array_flag,
                pointer_flag,
                location,
                specialized: None,
            },
            Void(_) => panic!("Type `Void` cannot be an array."),
            Char {
                pointer_flag,
                location,
                ..
            } => Char {
                array_flag,
                pointer_flag,
                location,
            },
            Byte {
                pointer_flag,
                location,
                ..
            } => Byte {
                array_flag,
                pointer_flag,
                location,
            },
            Short {
                signed_flag,
                pointer_flag,
                location,
                ..
            } => Short {
                signed_flag,
                array_flag,
                pointer_flag,
                location,
            },
            Int {
                signed_flag,
                pointer_flag,
                location,
                ..
            } => Int {
                signed_flag,
                array_flag,
                pointer_flag,
                location,
            },
            Long {
                signed_flag,
                pointer_flag,
                location,
                ..
            } => Long {
                signed_flag,
                array_flag,
                pointer_flag,
                location,
            },
            Float {
                pointer_flag,
                location,
                ..
            } => Float {
                array_flag,
                pointer_flag,
                location,
            },
            Double {
                pointer_flag,
                location,
                ..
            } => Double {
                array_flag,
                pointer_flag,
                location,
            },
            Struct {
                name,
                members,
                pointer_flag,
                location,
                ..
            } => Struct {
                name,
                members,
                array_flag,
                pointer_flag,
                location,
            },
        }
    }

    fn get_array_flag(&self) -> bool {
        use Type::*;

        match self.clone() {
            Any => panic!("Type `Any` cannot be an array."),
            Nothing => panic!("Type `Nothing` cannot be an array."),
            AnyRef => panic!("Type `AnyRef` cannot be an array."),
            Null => panic!("Type `Null` cannot be an array."),
            T { array_flag, .. } => array_flag,
            Void(_) => panic!("Type `Void` cannot be an array."),
            Char { array_flag, .. } => array_flag,
            Byte { array_flag, .. } => array_flag,
            Short { array_flag, .. } => array_flag,
            Int { array_flag, .. } => array_flag,
            Long { array_flag, .. } => array_flag,
            Float { array_flag, .. } => array_flag,
            Double { array_flag, .. } => array_flag,
            Struct { array_flag, .. } => array_flag,
        }
    }

    fn set_pointer_flag(&self, pointer_flag: bool) -> Type {
        use Type::*;

        match self.clone() {
            Any => panic!("Type `Any` cannot be a pointer."),
            Nothing => panic!("Type `Nothing` cannot be a pointer."),
            AnyRef => panic!("Type `AnyRef` cannot be a pointer."),
            Null => panic!("Type `Null` cannot be a pointer."),
            T {
                array_flag,
                location,
                specialized: Some(type_),
                ..
            } => T {
                array_flag,
                pointer_flag,
                location,
                specialized: Some(Box::new(type_.set_pointer_flag(pointer_flag))),
            },
            T {
                array_flag,
                location,
                specialized: None,
                ..
            } => T {
                array_flag,
                pointer_flag,
                location,
                specialized: None,
            },
            Void(_) => panic!("Type `Void` cannot be a pointer."),
            Char {
                array_flag,
                location,
                ..
            } => Char {
                array_flag,
                pointer_flag,
                location,
            },
            Byte {
                array_flag,
                location,
                ..
            } => Byte {
                array_flag,
                pointer_flag,
                location,
            },
            Short {
                signed_flag,
                array_flag,
                location,
                ..
            } => Short {
                signed_flag,
                array_flag,
                pointer_flag,
                location,
            },
            Int {
                signed_flag,
                array_flag,
                location,
                ..
            } => Int {
                signed_flag,
                array_flag,
                pointer_flag,
                location,
            },
            Long {
                signed_flag,
                array_flag,
                location,
                ..
            } => Long {
                signed_flag,
                array_flag,
                pointer_flag,
                location,
            },
            Float {
                array_flag,
                location,
                ..
            } => Float {
                array_flag,
                pointer_flag,
                location,
            },
            Double {
                array_flag,
                location,
                ..
            } => Double {
                array_flag,
                pointer_flag,
                location,
            },
            Struct {
                name,
                members,
                array_flag,
                location,
                ..
            } => Struct {
                name,
                members,
                array_flag,
                pointer_flag,
                location,
            },
        }
    }

    fn get_pointer_flag(&self) -> bool {
        use Type::*;

        match self.clone() {
            Any => panic!("Type `Any` cannot be a pointer."),
            Nothing => panic!("Type `Nothing` cannot be a pointer."),
            AnyRef => panic!("Type `AnyRef` cannot be a pointer."),
            Null => panic!("Type `Null` cannot be a pointer."),
            T { pointer_flag, .. } => pointer_flag,
            Void(_) => panic!("Type `Void` cannot be a pointer."),
            Char { pointer_flag, .. } => pointer_flag,
            Byte { pointer_flag, .. } => pointer_flag,
            Short { pointer_flag, .. } => pointer_flag,
            Int { pointer_flag, .. } => pointer_flag,
            Long { pointer_flag, .. } => pointer_flag,
            Float { pointer_flag, .. } => pointer_flag,
            Double { pointer_flag, .. } => pointer_flag,
            Struct { pointer_flag, .. } => pointer_flag,
        }
    }

    pub fn is_specialized(&mut self) -> bool {
        match self {
            Type::T {
                specialized: None, ..
            } => false,
            _ => true,
        }
    }

    pub fn set_specialized(&mut self, type_: Type) {
        match self {
            Type::T { specialized, .. } => {
                specialized.replace(Box::new(type_));
            }
            _ => {
                panic!("Try to specialize a concrete type.");
            }
        }
    }

    /// Determine the relationship of `left` type and `right` type.
    ///
    /// We treat `right` as the comparision base, which means if this function
    /// returns `Super`, then `left` is a `superclass` of `right`.
    pub fn compare_types(left: &Type, right: &Type) -> TypeRelationship {
        use Type::*;
        use TypeRelationship::*;

        // Reject all `T` types.
        if let T { .. } = left {
            panic!("Try to compare generic types.");
        } else if let T { .. } = right {
            panic!("Try to compare generic types.");
        }

        // Check for equivalent types.
        if left == right {
            return Equal;
        }

        // Relationships involving `Any`, `Nothing`, and `Void`
        // are trivially determined.
        match (left, right) {
            (Any, _) => {
                return Super;
            }
            (_, Any) => {
                return Sub;
            }
            (Nothing, _) => {
                return Sub;
            }
            (_, Nothing) => {
                return Super;
            }
            (Void(_), _) => {
                return None;
            }
            (_, Void(_)) => {
                return None;
            }
            _ => (),
        }

        // If one type is a pointer (`Anyref` or `Null`), then the other
        // type must also be a pointer (but cannot be an array).
        if let AnyRef = left {
            match right {
                Null => {
                    return Super;
                }
                type_ if type_.get_array_flag() => {
                    return None;
                }
                type_ if type_.get_pointer_flag() => {
                    return Super;
                }
                _ => unreachable!(),
            }
        } else if let Null = left {
            match right {
                AnyRef => {
                    return Sub;
                }
                type_ if type_.get_array_flag() => {
                    return None;
                }
                type_ if type_.get_pointer_flag() => {
                    return Sub;
                }
                _ => unreachable!(),
            }
        } else if let AnyRef = right {
            match left {
                Null => {
                    return Sub;
                }
                type_ if type_.get_array_flag() => {
                    return None;
                }
                type_ if type_.get_pointer_flag() => {
                    return Sub;
                }
                _ => unreachable!(),
            }
        } else if let Null = right {
            match left {
                AnyRef => {
                    return Super;
                }
                type_ if type_.get_array_flag() => {
                    return None;
                }
                type_ if type_.get_pointer_flag() => {
                    return Super;
                }
                _ => unreachable!(),
            }
        }

        // Type relationships for numerical types.
        if !left.get_array_flag()
            && !right.get_array_flag()
            && !left.get_pointer_flag()
            && !right.get_pointer_flag()
        {
            match left {
                Byte { .. } => match right {
                    Byte { .. } => Equal,
                    Short {
                        signed_flag: true, ..
                    } => Sub,
                    Short {
                        signed_flag: false, ..
                    } => Sub,
                    Int {
                        signed_flag: true, ..
                    } => Sub,
                    Int {
                        signed_flag: false, ..
                    } => Sub,
                    Long {
                        signed_flag: true, ..
                    } => Sub,
                    Long {
                        signed_flag: false, ..
                    } => Sub,
                    Float { .. } => Sub,
                    Double { .. } => Sub,
                    _ => None,
                },
                Short {
                    signed_flag: true, ..
                } => match right {
                    Byte { .. } => Super,
                    Short {
                        signed_flag: true, ..
                    } => Equal,
                    Short {
                        signed_flag: false, ..
                    } => None,
                    Int {
                        signed_flag: true, ..
                    } => Sub,
                    Int {
                        signed_flag: false, ..
                    } => Sub,
                    Long {
                        signed_flag: true, ..
                    } => Sub,
                    Long {
                        signed_flag: false, ..
                    } => Sub,
                    Float { .. } => Sub,
                    Double { .. } => Sub,
                    _ => None,
                },
                Short {
                    signed_flag: false, ..
                } => match right {
                    Byte { .. } => Super,
                    Short {
                        signed_flag: true, ..
                    } => None,
                    Short {
                        signed_flag: false, ..
                    } => Equal,
                    Int {
                        signed_flag: true, ..
                    } => Sub,
                    Int {
                        signed_flag: false, ..
                    } => Sub,
                    Long {
                        signed_flag: true, ..
                    } => Sub,
                    Long {
                        signed_flag: false, ..
                    } => Sub,
                    Float { .. } => Sub,
                    Double { .. } => Sub,
                    _ => None,
                },
                Int {
                    signed_flag: true, ..
                } => match right {
                    Byte { .. } => Super,
                    Short {
                        signed_flag: true, ..
                    } => Super,
                    Short {
                        signed_flag: false, ..
                    } => Super,
                    Int {
                        signed_flag: true, ..
                    } => Equal,
                    Int {
                        signed_flag: false, ..
                    } => None,
                    Long {
                        signed_flag: true, ..
                    } => Sub,
                    Long {
                        signed_flag: false, ..
                    } => Sub,
                    Float { .. } => Sub,
                    Double { .. } => Sub,
                    _ => None,
                },
                Int {
                    signed_flag: false, ..
                } => match right {
                    Byte { .. } => Super,
                    Short {
                        signed_flag: true, ..
                    } => None,
                    Short {
                        signed_flag: false, ..
                    } => Super,
                    Int {
                        signed_flag: true, ..
                    } => None,
                    Int {
                        signed_flag: false, ..
                    } => Equal,
                    Long {
                        signed_flag: true, ..
                    } => Sub,
                    Long {
                        signed_flag: false, ..
                    } => Sub,
                    Float { .. } => Sub,
                    Double { .. } => Sub,
                    _ => None,
                },
                Long {
                    signed_flag: true, ..
                } => match right {
                    Byte { .. } => Super,
                    Short {
                        signed_flag: true, ..
                    } => Super,
                    Short {
                        signed_flag: false, ..
                    } => Super,
                    Int {
                        signed_flag: true, ..
                    } => Super,
                    Int {
                        signed_flag: false, ..
                    } => Super,
                    Long {
                        signed_flag: true, ..
                    } => Equal,
                    Long {
                        signed_flag: false, ..
                    } => None,
                    Float { .. } => Sub,
                    Double { .. } => Sub,
                    _ => None,
                },
                Long {
                    signed_flag: false, ..
                } => match right {
                    Byte { .. } => Super,
                    Short {
                        signed_flag: true, ..
                    } => None,
                    Short {
                        signed_flag: false, ..
                    } => Super,
                    Int {
                        signed_flag: true, ..
                    } => None,
                    Int {
                        signed_flag: false, ..
                    } => Super,
                    Long {
                        signed_flag: true, ..
                    } => None,
                    Long {
                        signed_flag: false, ..
                    } => Equal,
                    Float { .. } => Sub,
                    Double { .. } => Sub,
                    _ => None,
                },
                Float { .. } => match right {
                    Byte { .. } => Super,
                    Short {
                        signed_flag: true, ..
                    } => Super,
                    Short {
                        signed_flag: false, ..
                    } => Super,
                    Int {
                        signed_flag: true, ..
                    } => Super,
                    Int {
                        signed_flag: false, ..
                    } => Super,
                    Long {
                        signed_flag: true, ..
                    } => Super,
                    Long {
                        signed_flag: false, ..
                    } => Super,
                    Float { .. } => Equal,
                    Double { .. } => Sub,
                    _ => None,
                },
                Double { .. } => match right {
                    Byte { .. } => Super,
                    Short {
                        signed_flag: true, ..
                    } => Super,
                    Short {
                        signed_flag: false, ..
                    } => Super,
                    Int {
                        signed_flag: true, ..
                    } => Super,
                    Int {
                        signed_flag: false, ..
                    } => Super,
                    Long {
                        signed_flag: true, ..
                    } => Super,
                    Long {
                        signed_flag: false, ..
                    } => Super,
                    Float { .. } => Super,
                    Double { .. } => Equal,
                    _ => None,
                },
                _ => None,
            }
        } else {
            None
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
    Group {
        // "(expression)"
        expression: Box<Expression>,
        location: Location,
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
        // { 1 } for arrays, { mem = 1 } for structures
        pairs: Vec<(Option<String>, Expression)>,
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
            | Group { location, .. }
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
    Null(Location), // ";"
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
        base_type: Rc<RefCell<Type>>, // does not contain array/pointer definitions
        declarators: Vec<(Rc<RefCell<Type>>, String, Option<Expression>)>,
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
        initialization: Option<Box<Statement>>,
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
            Return { location, .. }
            | Block { location, .. }
            | Def { location, .. }
            | While { location, .. }
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
    pub return_type: Rc<RefCell<Type>>,
    pub name: String,
    pub parameters: IndexMap<String, Rc<RefCell<Type>>>,
    pub body: Statement,
    pub location: Location,
}

impl Locate for Function {
    fn locate(&self) -> Location {
        self.location.clone()
    }
}

/// AST nodes for static objects that can appear in the global scope.
#[derive(Debug, PartialEq)]
pub enum StaticObject {
    Type(Type),
    Function(Box<Function>), // Boxing the large field to reduce the total size of the enum.
}
