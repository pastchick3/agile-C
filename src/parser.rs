use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use indexmap::IndexMap;

use crate::structure::{
    Error, Expression, Function, Locate, Location, Statement, StaticObject, Token, Type,
};

/// A structure containg names defined in different scopes.
struct Environment {
    envs: Vec<HashSet<String>>,
    structs: Vec<HashMap<String, Type>>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            envs: vec![HashSet::new()],
            structs: vec![HashMap::new()],
        }
    }

    // Enter a new scope.
    fn enter(&mut self) {
        self.envs.push(HashSet::new());
        self.structs.push(HashMap::new());
    }

    // Leave the current scope.
    fn leave(&mut self) {
        self.envs.pop();
        self.structs.pop();
    }

    // Define an identifier in the current scope.
    fn define(&mut self, name: &str) {
        self.envs.last_mut().unwrap().insert(name.to_string());
    }

    // Check whether an identifier has been defined.
    fn is_defined(&self, name: &str) -> bool {
        self.envs.iter().any(|e| e.contains(name))
    }

    // Define a structure in the current scope.
    fn define_struct(&mut self, name: &str, struct_: Type) {
        self.structs
            .last_mut()
            .unwrap()
            .insert(name.to_string(), struct_);
    }

    // Get the definition of a structure.
    fn get_struct(&self, name: &str) -> Option<Type> {
        self.structs
            .iter()
            .rev()
            .find_map(|structs| structs.get(name))
            .cloned()
    }
}

/// A parser producing a vector of static objects which may
/// contain unresolved generic types.
///
/// It is a Pratt parser (operator-precedence parser) with small
/// tweaks on how it parses potentially missing type specifiers.
/// Now the parser can handle three kinds of missing type specifiers.
///
/// For missing functions' return types and parameters, the parser
/// will simply insert a dummy type `T`.
///
/// For assignments to undefined variables, the parser will transform
/// these assignments to definition statements with dummy types `T`.
pub struct Parser<'a> {
    tokens: Vec<Token>,
    errors: &'a mut Vec<Error>,
    generic_ast: Option<Vec<StaticObject>>,
    environment: Environment,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, errors: &'a mut Vec<Error>) -> Self {
        // Filter out comments and also reverse the vector so we can
        // use `pop()` and `last()` to get tokens.
        let tokens: Vec<_> = tokens
            .into_iter()
            .filter(|tk| {
                if let Token::Comment { .. } = tk {
                    false
                } else {
                    true
                }
            })
            .rev()
            .collect();
        Parser {
            tokens,
            errors,
            generic_ast: Some(Vec::new()),
            environment: Environment::new(),
        }
    }

    /// Assemble a generic AST from tokens.
    pub fn run(&mut self) -> Result<Vec<StaticObject>, ()> {
        loop {
            match self.tokens.pop() {
                Some(Token::Struct(location)) => match self.parse_type_struct(location) {
                    Ok(struct_) => {
                        self.assert_token("Semicolon")?;
                        self.generic_ast
                            .as_mut()
                            .unwrap()
                            .push(StaticObject::Type(struct_));
                    }
                    Err(_) => self.tokens.clear(),
                },
                Some(Token::Include { literal, location }) => {
                    let include = Statement::Include {
                        content: literal,
                        location,
                    };
                    self.generic_ast
                        .as_mut()
                        .unwrap()
                        .push(StaticObject::Statement(include));
                }
                Some(Token::Ident { literal, location }) => {
                    if literal == "_AGILE_C_PROTO_" {
                        match self.parse_function(true) {
                            Ok(func) => self
                                .generic_ast
                                .as_mut()
                                .unwrap()
                                .push(StaticObject::Function(Box::new(func))),
                            Err(_) => self.tokens.clear(),
                        }
                    } else {
                        self.tokens.push(Token::Ident { literal, location });
                        match self.parse_function(false) {
                            Ok(func) => self
                                .generic_ast
                                .as_mut()
                                .unwrap()
                                .push(StaticObject::Function(Box::new(func))),
                            Err(_) => self.tokens.clear(),
                        }
                    }
                }
                Some(tk) => {
                    self.tokens.push(tk);
                    match self.parse_function(false) {
                        Ok(func) => self
                            .generic_ast
                            .as_mut()
                            .unwrap()
                            .push(StaticObject::Function(Box::new(func))),
                        Err(_) => self.tokens.clear(),
                    }
                }
                None => {
                    if self.errors.is_empty() {
                        return Ok(self.generic_ast.take().unwrap());
                    } else {
                        return Err(());
                    }
                }
            }
        }
    }

    /// Assert the next token's type. `forward()` will be called once
    /// and the asserted token will be returned only if it succeeds.
    fn assert_token(&mut self, name: &str) -> Result<Token, ()> {
        match self.tokens.pop() {
            Some(tk) => {
                if format!("{:?}", tk).contains(name) {
                    Ok(tk)
                } else {
                    let message = format!("Fail to assert `Token::{}` here.", name);
                    self.push_error(&message, tk.locate());
                    self.tokens.push(tk);
                    Err(())
                }
            }
            None => {
                self.push_error("Unexpected EOF.", Location::default());
                Err(())
            }
        }
    }

    /// Skip all tokens until (including) the next semicolons or EOF.
    fn skip_to_semicolon(&mut self) {
        loop {
            if let None | Some(Token::Semicolon(_)) = self.tokens.pop() {
                break;
            }
        }
    }

    /// A larger number means a higher priority.
    fn get_prefix_precedence(&self) -> u8 {
        match self.tokens.last() {
            Some(Token::Not(_)) => 15,
            Some(Token::Plus(_)) => 15,
            Some(Token::Minus(_)) => 15,
            Some(Token::BiPlus(_)) => 15,
            Some(Token::BiMinus(_)) => 15,
            Some(Token::Ampersand(_)) => 15,
            Some(Token::Asterisk(_)) => 15,
            _ => 0,
        }
    }

    // A larger number means a higher priority.
    fn get_infix_precedence(&self) -> u8 {
        match self.tokens.last() {
            Some(Token::Equal(_)) => 2,
            Some(Token::PlusEq(_)) => 2,
            Some(Token::MinusEq(_)) => 2,
            Some(Token::AsteriskEq(_)) => 2,
            Some(Token::SlashEq(_)) => 2,
            Some(Token::PercentEq(_)) => 2,
            Some(Token::Or(_)) => 4,
            Some(Token::And(_)) => 5,
            Some(Token::EqTo(_)) => 9,
            Some(Token::NotEqTo(_)) => 9,
            Some(Token::Small(_)) => 10,
            Some(Token::Large(_)) => 10,
            Some(Token::SmallEq(_)) => 10,
            Some(Token::LargeEq(_)) => 10,
            Some(Token::Plus(_)) => 12,
            Some(Token::Minus(_)) => 12,
            Some(Token::Asterisk(_)) => 13,
            Some(Token::Slash(_)) => 13,
            Some(Token::Percent(_)) => 13,
            Some(Token::BiPlus(_)) => 16,
            Some(Token::BiMinus(_)) => 16,
            Some(Token::LBracket(_)) => 16,
            Some(Token::LParen(_)) => 16,
            Some(Token::Dot(_)) => 16,
            Some(Token::Arrow(_)) => 16,
            _ => 0,
        }
    }

    /// A helper function to construct parsing errors.
    fn push_error(&mut self, message: &str, location: Location) {
        self.errors.push(Error::Parsing {
            message: message.to_string(),
            location,
        });
    }

    fn parse_function(&mut self, is_proto: bool) -> Result<Function, ()> {
        // Enter a new scope.
        self.environment.enter();

        // Record the function location.
        let location = match self.tokens.last() {
            Some(tk) => tk.locate(),
            None => {
                self.push_error("Unexpected EOF.", Location::default());
                return Err(());
            }
        };

        // Parse the return type.
        let mut return_type = self.parse_type()?;
        if let Some(Token::Asterisk(_)) = self.tokens.last() {
            let location = self.tokens.pop().unwrap().locate();
            return_type = Type::Pointer {
                refer: Box::new(return_type),
                location: Some(location),
            };
        }

        // Parse the function name.
        let name = match self.tokens.pop() {
            Some(Token::Ident { literal, .. }) => literal,
            Some(tk) => {
                self.push_error("Expect a function name.", tk.locate());
                return Err(());
            }
            None => {
                self.push_error("Unexpected EOF.", Location::default());
                return Err(());
            }
        };

        // Parse the parameter list.
        match self.tokens.pop() {
            Some(Token::LParen(_)) => (),
            Some(tk) => {
                self.push_error("Expect `(`.", tk.locate());
                return Err(());
            }
            None => {
                self.push_error("Unexpected EOF.", Location::default());
                return Err(());
            }
        }
        let mut parameters = IndexMap::new();
        let mut ellipsis = false;
        match self.tokens.pop() {
            Some(Token::RParen(_)) => {
                ellipsis = true;
            }
            Some(Token::Void(_)) => {
                self.assert_token("RParen")?;
            }
            Some(tk) => {
                self.tokens.push(tk);
                match self.tokens.last() {
                    Some(Token::RParen(_)) => {
                        self.tokens.pop();
                    }
                    Some(Token::Ellipsis(_)) => {
                        self.tokens.pop();
                        ellipsis = true;
                        self.assert_token("RParen")?;
                    }
                    Some(_) => loop {
                        // Parse an ellipsis.
                        if let Some(Token::Ellipsis(_)) = self.tokens.last() {
                            self.tokens.pop();
                            ellipsis = true;
                            self.assert_token("RParen")?;
                            break;
                        }
                        // Parse an normal parameter.
                        let mut type_ = self.parse_type()?;
                        if let Some(Token::Asterisk(_)) = self.tokens.last() {
                            let location = self.tokens.pop().unwrap().locate();
                            type_ = Type::Pointer {
                                refer: Box::new(type_),
                                location: Some(location),
                            };
                        }
                        let (name, location) = match self.tokens.pop() {
                            Some(Token::Ident { literal, location }) => {
                                self.environment.define(&literal);
                                (literal, location)
                            }
                            Some(tk) => {
                                self.push_error("Expect a parameter name.", tk.locate());
                                return Err(());
                            }
                            None => {
                                self.push_error("Unexpected EOF.", Location::default());
                                return Err(());
                            }
                        };
                        if parameters.contains_key(&name) {
                            self.push_error("Duplicate parameters.", location);
                            return Err(());
                        } else {
                            parameters.insert(name, Rc::new(RefCell::new(type_)));
                        }
                        if let Some(Token::Comma(_)) = self.tokens.last() {
                            self.tokens.pop();
                        } else {
                            self.assert_token("RParen")?;
                            break;
                        }
                    },
                    None => {
                        self.push_error("Unexpected EOF.", Location::default());
                        return Err(());
                    }
                }
            }
            None => {
                self.push_error("Unexpected EOF.", Location::default());
                return Err(());
            }
        }

        // Parse the function body.
        let body = self.parse_statement()?;

        // Leave the scope.
        self.environment.leave();

        // Return the function node.
        Ok(Function {
            is_proto,
            return_type: Rc::new(RefCell::new(return_type)),
            name,
            parameters,
            ellipsis,
            body,
            location,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ()> {
        match self.tokens.pop() {
            Some(Token::Void(location)) => Ok(Type::Void(Some(location))),
            Some(Token::Char(location)) => Ok(Type::Char(Some(location))),
            Some(Token::Short(location)) => Ok(Type::Short(Some(location))),
            Some(Token::Int(location)) => Ok(Type::Int(Some(location))),
            Some(Token::Long(location)) => Ok(Type::Long(Some(location))),
            Some(Token::Float(location)) => Ok(Type::Float(Some(location))),
            Some(Token::Double(location)) => Ok(Type::Double(Some(location))),
            Some(Token::Signed(location)) => match self.tokens.pop() {
                Some(Token::Short(_)) => Ok(Type::Short(Some(location))),
                Some(Token::Int(_)) => Ok(Type::Int(Some(location))),
                Some(Token::Long(_)) => Ok(Type::Long(Some(location))),
                Some(tk) => {
                    self.push_error(
                        "Expect `short`, `int`, or `long` after `signed`.",
                        tk.locate(),
                    );
                    self.tokens.push(tk);
                    Err(())
                }
                None => {
                    self.push_error("Unexpected EOF.", Location::default());
                    Err(())
                }
            },
            Some(Token::Unsigned(location)) => match self.tokens.pop() {
                Some(Token::Short(_)) => Ok(Type::UnsignedShort(Some(location))),
                Some(Token::Int(_)) => Ok(Type::UnsignedInt(Some(location))),
                Some(Token::Long(_)) => Ok(Type::UnsignedLong(Some(location))),
                Some(tk) => {
                    self.push_error(
                        "Expect `short`, `int`, or `long` after `unsigned`.",
                        tk.locate(),
                    );
                    self.tokens.push(tk);
                    Err(())
                }
                None => {
                    self.push_error("Unexpected EOF.", Location::default());
                    Err(())
                }
            },
            Some(Token::Struct(location)) => self.parse_type_struct(location),
            Some(tk) => {
                self.tokens.push(tk);
                Ok(Type::T(None))
            }
            None => {
                self.push_error("Unexpected EOF.", Location::default());
                Err(())
            }
        }
    }

    fn parse_type_struct(&mut self, location: Location) -> Result<Type, ()> {
        // Parse the structure name.
        let name = match self.tokens.pop() {
            Some(Token::Ident { literal, .. }) => literal,
            Some(tk) => {
                self.push_error("Expect a struct name.", tk.locate());
                return Err(());
            }
            None => {
                self.push_error("Unexpected EOF.", Location::default());
                return Err(());
            }
        };

        if let Some(Token::LBrace(_)) = self.tokens.last() {
            // Parse a structure definition.
            self.tokens.pop();
            let mut members = IndexMap::new();
            loop {
                match self.tokens.last() {
                    Some(Token::RBrace(_)) => {
                        self.tokens.pop();
                        break;
                    }
                    Some(_) => {
                        // Parse a structure member.
                        let (member, type_) = match self.parse_statement()? {
                            Statement::Def {
                                mut declarators, ..
                            } => {
                                // Require one member per field.
                                if declarators.len() != 1 {
                                    self.push_error("Expect fields.", Location::default());
                                    return Err(());
                                }
                                let declarator = declarators.pop().unwrap();
                                // The member field cannot have an initializer.
                                if declarator.2.is_some() {
                                    self.push_error("Expect struct members.", Location::default());
                                    return Err(());
                                }
                                // Get (name, type).
                                (
                                    declarator.1,
                                    Rc::try_unwrap(declarator.0).unwrap().into_inner(),
                                )
                            }
                            _ => {
                                self.push_error("Expect struct members.", Location::default());
                                return Err(());
                            }
                        };
                        members.insert(member, type_);
                    }
                    None => {
                        self.push_error("Unexpected EOF.", Location::default());
                        return Err(());
                    }
                }
            }
            let struct_ = Type::Struct {
                name: name.clone(),
                members,
                location: Some(location),
            };
            self.environment.define_struct(&name, struct_.clone());
            Ok(struct_)
        } else {
            // Parse a structure declaration.
            self.environment.get_struct(&name).ok_or_else(|| {
                self.push_error("Undefined structure.", location);
            })
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ()> {
        match self.tokens.pop() {
            Some(Token::Semicolon(loc)) => Ok(Statement::Null(loc)),

            Some(Token::Continue(loc)) => self.parse_statement_continue(loc),
            Some(Token::Break(loc)) => self.parse_statement_break(loc),
            Some(Token::Return(loc)) => self.parse_statement_return(loc),
            Some(Token::LBrace(loc)) => self.parse_statement_block(loc),
            Some(Token::While(loc)) => self.parse_statement_while(loc),
            Some(Token::Do(loc)) => self.parse_statement_do(loc),
            Some(Token::For(loc)) => self.parse_statement_for(loc),
            Some(Token::If(loc)) => self.parse_statement_if(loc),
            Some(Token::Switch(loc)) => self.parse_statement_switch(loc),

            Some(type_ @ Token::Void(_))
            | Some(type_ @ Token::Char(_))
            | Some(type_ @ Token::Struct(_))
            | Some(type_ @ Token::Double(_))
            | Some(type_ @ Token::Float(_))
            | Some(type_ @ Token::Long(_))
            | Some(type_ @ Token::Int(_))
            | Some(type_ @ Token::Short(_))
            | Some(type_ @ Token::Signed(_))
            | Some(type_ @ Token::Unsigned(_)) => {
                self.tokens.push(type_);
                self.parse_statement_def()
            }

            Some(Token::Include { literal, location }) => Ok(Statement::Include {
                content: literal,
                location,
            }),

            Some(tk) => {
                self.tokens.push(tk);
                self.parse_statement_expr()
            }

            None => {
                self.push_error("Unexpected EOF.", Location::default());
                Err(())
            }
        }
    }

    fn parse_statement_continue(&mut self, location: Location) -> Result<Statement, ()> {
        self.assert_token("Semicolon")
            .map_err(|_| self.skip_to_semicolon())?;
        Ok(Statement::Continue(location))
    }

    fn parse_statement_break(&mut self, location: Location) -> Result<Statement, ()> {
        self.assert_token("Semicolon")
            .map_err(|_| self.skip_to_semicolon())?;
        Ok(Statement::Break(location))
    }

    fn parse_statement_return(&mut self, location: Location) -> Result<Statement, ()> {
        if let Some(Token::Semicolon(_)) = self.tokens.last() {
            // Parse `return;`.
            self.tokens.pop();
            Ok(Statement::Return {
                expression: None,
                location,
            })
        } else {
            // Parse `return expr;`.
            let expr = self.parse_expression(0)?;
            self.assert_token("Semicolon")
                .map_err(|_| self.skip_to_semicolon())?;
            Ok(Statement::Return {
                expression: Some(expr),
                location,
            })
        }
    }

    fn parse_statement_block(&mut self, location: Location) -> Result<Statement, ()> {
        self.environment.enter();
        let mut statements = Vec::new();
        loop {
            match self.tokens.last() {
                Some(Token::RBrace(_)) => {
                    self.tokens.pop();
                    break;
                }
                Some(_) => {
                    // Do not abort if one statement goes wrong.
                    if let Ok(stmt) = self.parse_statement() {
                        statements.push(stmt);
                    }
                }
                None => {
                    self.push_error("Unexpected EOF.", Location::default());
                    return Err(());
                }
            }
        }
        self.environment.leave();
        Ok(Statement::Block {
            statements,
            location,
        })
    }

    fn parse_statement_while(&mut self, location: Location) -> Result<Statement, ()> {
        self.assert_token("LParen")?;
        let condition = self.parse_expression(0)?;
        self.assert_token("RParen")?;
        let body = self.parse_statement()?;
        Ok(Statement::While {
            condition,
            body: Box::new(body),
            location,
        })
    }

    fn parse_statement_do(&mut self, location: Location) -> Result<Statement, ()> {
        let body = self.parse_statement()?;
        self.assert_token("While")?;
        self.assert_token("LParen")?;
        let condition = self.parse_expression(0)?;
        self.assert_token("RParen")?;
        self.assert_token("Semicolon")
            .map_err(|_| self.skip_to_semicolon())?;
        Ok(Statement::Do {
            condition,
            body: Box::new(body),
            location,
        })
    }

    fn parse_statement_for(&mut self, location: Location) -> Result<Statement, ()> {
        self.environment.enter();
        self.assert_token("LParen")?;

        // Parse the initialization.
        let initialization = match self.tokens.last() {
            Some(Token::Semicolon(_)) => {
                self.tokens.pop();
                None
            }
            _ => Some(Box::new(self.parse_statement()?)),
        };

        // Parse the condition.
        let condition = match self.tokens.last() {
            Some(Token::Semicolon(_)) => None,
            _ => Some(self.parse_expression(0)?),
        };
        self.assert_token("Semicolon")
            .map_err(|_| self.skip_to_semicolon())?;

        // Parse the increment.
        let increment = match self.tokens.last() {
            Some(Token::RParen(_)) => None,
            _ => Some(self.parse_expression(0)?),
        };
        self.assert_token("RParen")?;

        let body = self.parse_statement()?;
        self.environment.leave();
        Ok(Statement::For {
            initialization,
            condition,
            increment,
            body: Box::new(body),
            location,
        })
    }

    fn parse_statement_if(&mut self, location: Location) -> Result<Statement, ()> {
        self.assert_token("LParen")?;
        let condition = self.parse_expression(0)?;
        self.assert_token("RParen")?;
        let body = self.parse_statement()?;
        let alternative = match self.tokens.last() {
            Some(Token::Else(_)) => {
                self.tokens.pop();
                Some(Box::new(self.parse_statement()?))
            }
            _ => None,
        };
        Ok(Statement::If {
            condition,
            body: Box::new(body),
            alternative,
            location,
        })
    }

    fn parse_statement_switch(&mut self, location: Location) -> Result<Statement, ()> {
        self.assert_token("LParen")?;
        let expression = self.parse_expression(0)?;
        self.assert_token("RParen")?;
        self.assert_token("LBrace")?;
        let mut default = None;
        let mut branches = Vec::new();
        loop {
            match self.tokens.pop() {
                // Parse `case`.
                Some(Token::Case(_)) => {
                    let expr = self.parse_expression(0)?;
                    self.assert_token("Colon")?;
                    let mut stmts = Vec::new();
                    loop {
                        match self.tokens.last() {
                            Some(Token::Case(_))
                            | Some(Token::Default_(_))
                            | Some(Token::RBrace(_)) => break,
                            _ => stmts.push(self.parse_statement()?),
                        }
                    }
                    branches.push((expr, stmts));
                }

                // Parse `default`.
                Some(tk @ Token::Default_(_)) => {
                    if default.is_some() {
                        self.push_error("Multiple default.", tk.locate());
                        return Err(());
                    }
                    self.assert_token("Colon")?;
                    let mut stmts = Vec::new();
                    loop {
                        match self.tokens.last() {
                            Some(Token::Case(_))
                            | Some(Token::Default_(_))
                            | Some(Token::RBrace(_)) => break,
                            _ => stmts.push(self.parse_statement()?),
                        }
                    }
                    default = Some(stmts);
                }

                Some(Token::RBrace(_)) => break,
                Some(tk) => {
                    self.push_error("Expect `case` or `default`.", tk.locate());
                    self.tokens.push(tk);
                    return Err(());
                }
                None => {
                    self.push_error("Unexpected EOF.", Location::default());
                    return Err(());
                }
            }
        }
        Ok(Statement::Switch {
            expression,
            branches,
            default,
            location,
        })
    }

    fn parse_statement_def(&mut self) -> Result<Statement, ()> {
        let base_type = self.parse_type().map_err(|_| self.skip_to_semicolon())?;
        let location = base_type.locate();
        let mut declarators = Vec::new();
        loop {
            // Check for pointer types.
            let mut type_ = base_type.clone();
            while let Some(Token::Asterisk(_)) = self.tokens.last() {
                let location = self.tokens.pop().unwrap().locate();
                type_ = Type::Pointer {
                    refer: Box::new(type_),
                    location: Some(location),
                };
            }

            // Parse the identifier.
            match self.tokens.pop() {
                Some(Token::Ident { literal, .. }) => {
                    self.environment.define(&literal);
                    // Check for array types.
                    while let Some(Token::LBracket(_)) = self.tokens.last() {
                        let location = self.tokens.pop().unwrap().locate();
                        let mut length = None;
                        match self.tokens.last() {
                            Some(Token::RBracket(_)) => {
                                self.tokens.pop();
                            }
                            _ => {
                                match self.parse_expression(0)? {
                                    Expression::IntConst { value, .. } if value > 0 => {
                                        length = Some(value as usize);
                                    }
                                    expr => {
                                        self.push_error(
                                            "Expect an integral literal.",
                                            expr.locate(),
                                        );
                                    }
                                }
                                self.assert_token("RBracket")?;
                            }
                        }
                        type_ = Type::Array {
                            content: Box::new(type_),
                            length,
                            location: Some(location),
                        }
                    }
                    // Parse the initializer.
                    let mut initializer = None;
                    if let Some(Token::Equal(_)) = self.tokens.last() {
                        self.tokens.pop();
                        let value = match self.tokens.last() {
                            Some(Token::LBrace(_)) => self.parse_expression_init_list()?,
                            _ => self.parse_expression(0)?,
                        };
                        initializer = Some(value);
                    }
                    declarators.push((Rc::new(RefCell::new(type_)), literal, initializer));
                }
                Some(tk) => {
                    self.push_error("Expect an identifier.", tk.locate());
                    self.tokens.push(tk);
                    return Err(());
                }
                None => (),
            }

            // Parse the delimiter.
            match self.tokens.pop() {
                Some(Token::Comma(_)) => (),
                Some(Token::Semicolon(_)) => break,
                Some(tk) => {
                    self.push_error("Expect `,` or `;`.", tk.locate());
                    self.tokens.push(tk);
                    return Err(());
                }
                None => {
                    self.push_error("Unexpected EOF.", Location::default());
                    return Err(());
                }
            }
        }

        Ok(Statement::Def {
            base_type: Rc::new(RefCell::new(base_type)),
            declarators,
            location,
        })
    }

    fn parse_statement_expr(&mut self) -> Result<Statement, ()> {
        let expr = self.parse_expression(0)?;
        self.assert_token("Semicolon")
            .map_err(|_| self.skip_to_semicolon())?;
        // Transform an assignment to a definition if the name is
        // not defined yet. Also notice we currently cannot match
        // a boxed value in the stable Rust, so we need two seperate
        // matches.
        if let Expression::Infix {
            left,
            operator: "=",
            right,
        } = expr
        {
            if let Expression::Ident { value, location } = *left {
                if !self.environment.is_defined(&value) {
                    self.environment.define(&value);
                    Ok(Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::T(None))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::T(None))),
                            value,
                            Some(*right),
                        )],
                        location,
                    })
                } else {
                    Ok(Statement::Expr(Expression::Infix {
                        left: Box::new(Expression::Ident { value, location }),
                        operator: "=",
                        right,
                    }))
                }
            } else {
                Ok(Statement::Expr(Expression::Infix {
                    left,
                    operator: "=",
                    right,
                }))
            }
        } else {
            Ok(Statement::Expr(expr))
        }
    }

    fn parse_expression_init_list(&mut self) -> Result<Expression, ()> {
        let mut initializers = Vec::new();
        let location = self.assert_token("LBrace")?.locate();
        loop {
            match self.tokens.last() {
                // { .mem = 1 } for structures
                Some(Token::Dot(_)) => {
                    // Parse a structure member.
                    let location = self.tokens.pop().unwrap().locate();
                    match self.tokens.pop() {
                        Some(Token::Ident { literal, .. }) => {
                            if let Some(Token::Equal(_)) = self.tokens.last() {
                                self.tokens.pop();
                                initializers.push((Some(literal), self.parse_expression(0)?));
                            } else {
                                self.push_error("Expect `.member = expr`.", location);
                                return Err(());
                            }
                        }
                        Some(tk) => {
                            self.tokens.push(tk);
                            self.push_error("Expect `.member`.", location);
                            return Err(());
                        }
                        None => {
                            self.push_error("Unexpected EOF.", Location::default());
                            return Err(());
                        }
                    }
                    // Parse the delimiter.
                    match self.tokens.pop() {
                        Some(Token::Comma(_)) => {
                            if let Some(Token::RBrace(_)) = self.tokens.last() {
                                break;
                            }
                        }
                        Some(tk @ Token::RBrace(_)) => {
                            self.tokens.push(tk);
                            break;
                        }
                        Some(tk) => {
                            self.push_error("Expect `,` or `}`.", tk.locate());
                            self.tokens.push(tk);
                            return Err(());
                        }
                        None => {
                            self.push_error("Unexpected EOF.", Location::default());
                            return Err(());
                        }
                    }
                }

                // { 1 } for arrays
                Some(_) => {
                    // Parse an array element.
                    initializers.push((None, self.parse_expression(0)?));
                    // Parse the delimiter.
                    match self.tokens.pop() {
                        Some(Token::Comma(_)) => {
                            if let Some(Token::RBrace(_)) = self.tokens.last() {
                                break;
                            }
                        }
                        Some(tk @ Token::RBrace(_)) => {
                            self.tokens.push(tk);
                            break;
                        }
                        Some(tk) => {
                            self.push_error("Expect `,` or `}`.", tk.locate());
                            self.tokens.push(tk);
                            return Err(());
                        }
                        None => {
                            self.push_error("Unexpected EOF.", Location::default());
                            return Err(());
                        }
                    }
                }

                None => {
                    self.push_error("Unexpected EOF.", Location::default());
                    return Err(());
                }
            }
        }
        self.assert_token("RBrace")
            .map_err(|_| self.skip_to_semicolon())?;
        Ok(Expression::InitList {
            initializers,
            location,
        })
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<Expression, ()> {
        let mut left = self.parse_prefix()?;
        while self.get_infix_precedence() != 0 && precedence < self.get_infix_precedence() {
            left = self.parse_infix(self.get_infix_precedence(), left)?;
        }
        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expression, ()> {
        let precedence = self.get_prefix_precedence();
        match self.tokens.pop() {
            Some(Token::Ident { literal, location }) => Ok(Expression::Ident {
                value: literal,
                location,
            }),
            Some(Token::IntConst { literal, location }) => Ok(Expression::IntConst {
                value: literal.parse().unwrap(),
                location,
            }),
            Some(Token::FloatConst { literal, location }) => Ok(Expression::FloatConst {
                value: literal.parse().unwrap(),
                location,
            }),
            Some(Token::CharConst { literal, location }) => Ok(Expression::CharConst {
                value: literal,
                location,
            }),
            Some(Token::StrConst { literal, location }) => Ok(Expression::StrConst {
                value: literal,
                location,
            }),
            Some(Token::Not(location)) => Ok(Expression::Prefix {
                operator: "!",
                expression: Box::new(self.parse_expression(precedence)?),
                location,
            }),
            Some(Token::Plus(location)) => Ok(Expression::Prefix {
                operator: "+",
                expression: Box::new(self.parse_expression(precedence)?),
                location,
            }),
            Some(Token::Minus(location)) => Ok(Expression::Prefix {
                operator: "-",
                expression: Box::new(self.parse_expression(precedence)?),
                location,
            }),
            Some(Token::BiPlus(location)) => Ok(Expression::Prefix {
                operator: "++",
                expression: Box::new(self.parse_expression(precedence)?),
                location,
            }),
            Some(Token::BiMinus(location)) => Ok(Expression::Prefix {
                operator: "--",
                expression: Box::new(self.parse_expression(precedence)?),
                location,
            }),
            Some(Token::Ampersand(location)) => Ok(Expression::Prefix {
                operator: "&",
                expression: Box::new(self.parse_expression(precedence)?),
                location,
            }),
            Some(Token::Asterisk(location)) => Ok(Expression::Prefix {
                operator: "*",
                expression: Box::new(self.parse_expression(precedence)?),
                location,
            }),
            Some(Token::LParen(location)) => {
                let expr = self.parse_expression(0)?;
                self.assert_token("RParen")?;
                Ok(Expression::Group {
                    expression: Box::new(expr),
                    location,
                })
            }
            Some(Token::LBrace(loc)) => {
                self.tokens.push(Token::LBrace(loc));
                self.parse_expression_init_list()
            }
            Some(tk) => {
                self.push_error("Expect a prefix operator.", tk.locate());
                self.tokens.push(tk);
                Err(())
            }
            None => {
                self.push_error("Unexpected EOF.", Location::default());
                Err(())
            }
        }
    }

    fn parse_infix(&mut self, precedence: u8, left: Expression) -> Result<Expression, ()> {
        match self.tokens.pop() {
            Some(Token::Plus(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "+",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::Minus(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "-",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::Asterisk(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "*",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::Slash(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "/",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::Percent(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "%",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::BiPlus(_)) => Ok(Expression::Postfix {
                operator: "++",
                expression: Box::new(left),
            }),
            Some(Token::BiMinus(_)) => Ok(Expression::Postfix {
                operator: "--",
                expression: Box::new(left),
            }),
            Some(Token::Equal(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "=",
                right: Box::new(self.parse_expression(precedence - 1)?),
            }),
            Some(Token::Small(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "<",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::Large(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: ">",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::SmallEq(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "<=",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::LargeEq(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: ">=",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::PlusEq(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "+=",
                right: Box::new(self.parse_expression(precedence - 1)?),
            }),
            Some(Token::MinusEq(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "-=",
                right: Box::new(self.parse_expression(precedence - 1)?),
            }),
            Some(Token::AsteriskEq(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "*=",
                right: Box::new(self.parse_expression(precedence - 1)?),
            }),
            Some(Token::SlashEq(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "/=",
                right: Box::new(self.parse_expression(precedence - 1)?),
            }),
            Some(Token::PercentEq(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "%=",
                right: Box::new(self.parse_expression(precedence - 1)?),
            }),
            Some(Token::EqTo(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "==",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::NotEqTo(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "!=",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::And(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "&&",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::Or(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "||",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::LParen(_)) => {
                let mut arguments = Vec::new();
                if let Some(Token::RParen(_)) = self.tokens.last() {
                    self.tokens.pop();
                } else {
                    loop {
                        let arg = self.parse_expression(0)?;
                        arguments.push(arg);
                        match self.tokens.pop() {
                            Some(Token::Comma(_)) => (),
                            Some(Token::RParen(_)) => break,
                            Some(tk) => {
                                self.push_error("Expect `,` or `)`.", tk.locate());
                                self.tokens.push(tk);
                                return Err(());
                            }
                            None => {
                                self.push_error("Unexpected EOF.", Location::default());
                                return Err(());
                            }
                        }
                    }
                }
                Ok(Expression::Call {
                    expression: Box::new(left),
                    arguments,
                })
            }
            Some(Token::LBracket(_)) => {
                let index = self.parse_expression(0)?;
                self.assert_token("RBracket")?;
                Ok(Expression::Index {
                    expression: Box::new(left),
                    index: Box::new(index),
                })
            }
            Some(Token::Dot(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: ".",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::Arrow(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "->",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(tk) => {
                self.push_error("Expect an infix operator.", tk.locate());
                self.tokens.push(tk);
                Err(())
            }
            None => {
                self.push_error("Unexpected EOF.", Location::default());
                Err(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::preprocessor::Preprocessor;

    #[test]
    fn comment() {
        let source = "/**/";
        let expected_errors = vec![];
        let expected_generic_ast = vec![];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn structure() {
        let source = "struct A {};";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Type(Type::Struct {
            name: "A".to_string(),
            members: IndexMap::new(),
            location: Some(Location::default()),
        })];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn include() {
        let source = "#include <_test>";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            StaticObject::Statement(Statement::Include {
                content: "#include <_test>".to_string(),
                location: Location::default(),
            }),
            StaticObject::Function(Box::new(Function {
                is_proto: true,
                return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
                name: "_AGILE_C_F_".to_string(),
                parameters: IndexMap::new(),
                ellipsis: false,
                body: Statement::Null(Location::default()),
                location: Location::default(),
            })),
        ];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn return_pointer() {
        let source = "int *f(void) {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Pointer {
                refer: Box::new(Type::Int(Some(Location::default()))),
                location: Some(Location::default()),
            })),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn function_unchecked() {
        let source = "void f() {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: true,
            body: Statement::Block {
                statements: vec![],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn function_void() {
        let source = "void f(void) {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn function_ellipsis() {
        let source = "void f(...) {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: true,
            body: Statement::Block {
                statements: vec![],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn function_single() {
        let source = "void f(int a) {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: [(
                "a".to_string(),
                Rc::new(RefCell::new(Type::Int(Some(Location::default())))),
            )]
            .iter()
            .cloned()
            .collect(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn function_multiple() {
        let source = "void f(int a, int b, ...) {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: [
                (
                    "a".to_string(),
                    Rc::new(RefCell::new(Type::Int(Some(Location::default())))),
                ),
                (
                    "b".to_string(),
                    Rc::new(RefCell::new(Type::Int(Some(Location::default())))),
                ),
            ]
            .iter()
            .cloned()
            .collect(),
            ellipsis: true,
            body: Statement::Block {
                statements: vec![],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn duplicate_parameters() {
        let source = "
            void f(int a, unsigned int a) {}
        ";
        let expected_errors = vec![Error::Parsing {
            message: "Duplicate parameters.".to_string(),
            location: Location::default(),
        }];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        Parser::new(tokens, &mut errors).run().unwrap_err();
        assert_eq!(errors, expected_errors);
    }

    #[test]
    fn statement_null() {
        let source = "
            void f(void) { ; }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::Null(Location::default())],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_continue() {
        let source = "void f(void) { continue; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::Continue(Location::default())],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_break() {
        let source = "void f(void) { break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::Break(Location::default())],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_return() {
        let source = "
            void f(void) {
                return;
                return 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::Return {
                        expression: None,
                        location: Location::default(),
                    },
                    Statement::Return {
                        expression: Some(Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        }),
                        location: Location::default(),
                    },
                ],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_def_var() {
        let source = "
            void f(void) {
                char a[1];
                short a[] = { 1 };
                unsigned short a[1] = { 1 };
                int a[2] = { 1, 2 };
                unsigned int *a;
                long *a = &a;
                unsigned long a = *a;
                float a, b;
                double a = 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Char(Some(Location::default())))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Array {
                                content: Box::new(Type::Char(Some(Location::default()))),
                                length: Some(1),
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            None,
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Short(Some(Location::default())))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Array {
                                content: Box::new(Type::Short(Some(Location::default()))),
                                length: None,
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            Some(Expression::InitList {
                                initializers: vec![(
                                    None,
                                    Expression::IntConst {
                                        value: 1,
                                        location: Location::default(),
                                    },
                                )],
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::UnsignedShort(Some(
                            Location::default(),
                        )))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Array {
                                content: Box::new(Type::UnsignedShort(Some(Location::default()))),
                                length: Some(1),
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            Some(Expression::InitList {
                                initializers: vec![(
                                    None,
                                    Expression::IntConst {
                                        value: 1,
                                        location: Location::default(),
                                    },
                                )],
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Int(Some(Location::default())))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Array {
                                content: Box::new(Type::Int(Some(Location::default()))),
                                length: Some(2),
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            Some(Expression::InitList {
                                initializers: vec![
                                    (
                                        None,
                                        Expression::IntConst {
                                            value: 1,
                                            location: Location::default(),
                                        },
                                    ),
                                    (
                                        None,
                                        Expression::IntConst {
                                            value: 2,
                                            location: Location::default(),
                                        },
                                    ),
                                ],
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::UnsignedInt(Some(
                            Location::default(),
                        )))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Pointer {
                                refer: Box::new(Type::UnsignedInt(Some(Location::default()))),
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            None,
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Long(Some(Location::default())))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Pointer {
                                refer: Box::new(Type::Long(Some(Location::default()))),
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            Some(Expression::Prefix {
                                operator: "&",
                                expression: Box::new(Expression::Ident {
                                    value: "a".to_string(),
                                    location: Location::default(),
                                }),
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::UnsignedLong(Some(
                            Location::default(),
                        )))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::UnsignedLong(Some(Location::default())))),
                            "a".to_string(),
                            Some(Expression::Prefix {
                                operator: "*",
                                expression: Box::new(Expression::Ident {
                                    value: "a".to_string(),
                                    location: Location::default(),
                                }),
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Float(Some(Location::default())))),
                        declarators: vec![
                            (
                                Rc::new(RefCell::new(Type::Float(Some(Location::default())))),
                                "a".to_string(),
                                None,
                            ),
                            (
                                Rc::new(RefCell::new(Type::Float(Some(Location::default())))),
                                "b".to_string(),
                                None,
                            ),
                        ],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Double(Some(Location::default())))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Double(Some(Location::default())))),
                            "a".to_string(),
                            Some(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                ],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_def_struct() {
        let source = "
            void f(void) {
                struct A {} a;
                struct A {
                    int a;
                } a = { 1 };
                struct A {
                    int a;
                    float b;
                } a = { 1, .b = 2, };
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Struct {
                            name: "A".to_string(),
                            members: IndexMap::new(),
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Struct {
                                name: "A".to_string(),
                                members: IndexMap::new(),
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            None,
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Struct {
                            name: "A".to_string(),
                            members: [("a".to_string(), Type::Int(Some(Location::default())))]
                                .iter()
                                .cloned()
                                .collect(),
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Struct {
                                name: "A".to_string(),
                                members: [("a".to_string(), Type::Int(Some(Location::default())))]
                                    .iter()
                                    .cloned()
                                    .collect(),
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            Some(Expression::InitList {
                                initializers: vec![(
                                    None,
                                    Expression::IntConst {
                                        value: 1,
                                        location: Location::default(),
                                    },
                                )],
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Struct {
                            name: "A".to_string(),
                            members: [
                                ("a".to_string(), Type::Int(Some(Location::default()))),
                                ("b".to_string(), Type::Float(Some(Location::default()))),
                            ]
                            .iter()
                            .cloned()
                            .collect(),
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Struct {
                                name: "A".to_string(),
                                members: [
                                    ("a".to_string(), Type::Int(Some(Location::default()))),
                                    ("b".to_string(), Type::Float(Some(Location::default()))),
                                ]
                                .iter()
                                .cloned()
                                .collect(),
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            Some(Expression::InitList {
                                initializers: vec![
                                    (
                                        None,
                                        Expression::IntConst {
                                            value: 1,
                                            location: Location::default(),
                                        },
                                    ),
                                    (
                                        Some("b".to_string()),
                                        Expression::IntConst {
                                            value: 2,
                                            location: Location::default(),
                                        },
                                    ),
                                ],
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                ],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_def_nested() {
        let source = "
            void f(void) {
                int **a[1][2];
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::Def {
                    base_type: Rc::new(RefCell::new(Type::Int(Some(Location::default())))),
                    declarators: vec![(
                        Rc::new(RefCell::new(Type::Array {
                            content: Box::new(Type::Array {
                                content: Box::new(Type::Pointer {
                                    refer: Box::new(Type::Pointer {
                                        refer: Box::new(Type::Int(Some(Location::default()))),
                                        location: Some(Location::default()),
                                    }),
                                    location: Some(Location::default()),
                                }),
                                length: Some(1),
                                location: Some(Location::default()),
                            }),
                            length: Some(2),
                            location: Some(Location::default()),
                        })),
                        "a".to_string(),
                        None,
                    )],
                    location: Location::default(),
                }],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_while() {
        let source = "
            void f(void) {
                while (1) ;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::While {
                    condition: Expression::IntConst {
                        value: 1,
                        location: Location::default(),
                    },
                    body: Box::new(Statement::Null(Location::default())),
                    location: Location::default(),
                }],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_do() {
        let source = "
            void f(void) {
                do ; while (1);
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::Do {
                    condition: Expression::IntConst {
                        value: 1,
                        location: Location::default(),
                    },
                    body: Box::new(Statement::Null(Location::default())),
                    location: Location::default(),
                }],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_for() {
        let source = "
            void f(int a) {
                for ( ; ; ) ;
                for (a = 1; 1; a++) ;
                for (int b = 1; 1; b++) ;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: [(
                "a".to_string(),
                Rc::new(RefCell::new(Type::Int(Some(Location::default())))),
            )]
            .iter()
            .cloned()
            .collect(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::For {
                        initialization: None,
                        condition: None,
                        increment: None,
                        body: Box::new(Statement::Null(Location::default())),
                        location: Location::default(),
                    },
                    Statement::For {
                        initialization: Some(Box::new(Statement::Expr(Expression::Infix {
                            left: Box::new(Expression::Ident {
                                value: "a".to_string(),
                                location: Location::default(),
                            }),
                            operator: "=",
                            right: Box::new(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                        }))),
                        condition: Some(Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        }),
                        increment: Some(Expression::Postfix {
                            operator: "++",
                            expression: Box::new(Expression::Ident {
                                value: "a".to_string(),
                                location: Location::default(),
                            }),
                        }),
                        body: Box::new(Statement::Null(Location::default())),
                        location: Location::default(),
                    },
                    Statement::For {
                        initialization: Some(Box::new(Statement::Def {
                            base_type: Rc::new(RefCell::new(Type::Int(Some(Location::default())))),
                            declarators: vec![(
                                Rc::new(RefCell::new(Type::Int(Some(Location::default())))),
                                "b".to_string(),
                                Some(Expression::IntConst {
                                    value: 1,
                                    location: Location::default(),
                                }),
                            )],
                            location: Location::default(),
                        })),
                        condition: Some(Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        }),
                        increment: Some(Expression::Postfix {
                            operator: "++",
                            expression: Box::new(Expression::Ident {
                                value: "b".to_string(),
                                location: Location::default(),
                            }),
                        }),
                        body: Box::new(Statement::Null(Location::default())),
                        location: Location::default(),
                    },
                ],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_if() {
        let source = "
            void f(void) {
                if (1) ;
                if (1) ; else ;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::If {
                        condition: Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        },
                        body: Box::new(Statement::Null(Location::default())),
                        alternative: None,
                        location: Location::default(),
                    },
                    Statement::If {
                        condition: Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        },
                        body: Box::new(Statement::Null(Location::default())),
                        alternative: Some(Box::new(Statement::Null(Location::default()))),
                        location: Location::default(),
                    },
                ],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_switch() {
        let source = "
            void f(void) {
                switch (1) {}
                switch (1) {
                    case 1: ;
                }
                switch (1) {
                    case 1: ;
                    default: ;
                }
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::Switch {
                        expression: Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        },
                        branches: vec![],
                        default: None,
                        location: Location::default(),
                    },
                    Statement::Switch {
                        expression: Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        },
                        branches: vec![(
                            Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            },
                            vec![Statement::Null(Location::default())],
                        )],
                        default: None,
                        location: Location::default(),
                    },
                    Statement::Switch {
                        expression: Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        },
                        branches: vec![(
                            Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            },
                            vec![Statement::Null(Location::default())],
                        )],
                        default: Some(vec![Statement::Null(Location::default())]),
                        location: Location::default(),
                    },
                ],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn expression_ident_const() {
        let source = "
            void f(void) {
                a;
                1;
                1.1;
                'a';
                \"a\";
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::Expr(Expression::Ident {
                        value: "a".to_string(),
                        location: Location::default(),
                    }),
                    Statement::Expr(Expression::IntConst {
                        value: 1,
                        location: Location::default(),
                    }),
                    Statement::Expr(Expression::FloatConst {
                        value: 1.1,
                        location: Location::default(),
                    }),
                    Statement::Expr(Expression::CharConst {
                        value: "'a'".to_string(),
                        location: Location::default(),
                    }),
                    Statement::Expr(Expression::StrConst {
                        value: "\"a\"".to_string(),
                        location: Location::default(),
                    }),
                ],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn expression_arithmetic() {
        let source = "
            void f(void) {
                1 = ++1 + (--1 - 1++) * 1-- / 1 % 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::Expr(Expression::Infix {
                    left: Box::new(Expression::IntConst {
                        value: 1,
                        location: Location::default(),
                    }),
                    operator: "=",
                    right: Box::new(Expression::Infix {
                        left: Box::new(Expression::Prefix {
                            operator: "++",
                            expression: Box::new(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                            location: Location::default(),
                        }),
                        operator: "+",
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::Infix {
                                left: Box::new(Expression::Infix {
                                    left: Box::new(Expression::Group {
                                        expression: Box::new(Expression::Infix {
                                            left: Box::new(Expression::Prefix {
                                                operator: "--",
                                                location: Location::default(),
                                                expression: Box::new(Expression::IntConst {
                                                    value: 1,
                                                    location: Location::default(),
                                                }),
                                            }),
                                            operator: "-",
                                            right: Box::new(Expression::Postfix {
                                                operator: "++",
                                                expression: Box::new(Expression::IntConst {
                                                    value: 1,
                                                    location: Location::default(),
                                                }),
                                            }),
                                        }),
                                        location: Location::default(),
                                    }),
                                    operator: "*",
                                    right: Box::new(Expression::Postfix {
                                        operator: "--",
                                        expression: Box::new(Expression::IntConst {
                                            value: 1,
                                            location: Location::default(),
                                        }),
                                    }),
                                }),
                                operator: "/",
                                right: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::default(),
                                }),
                            }),
                            operator: "%",
                            right: Box::new(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                        }),
                    }),
                })],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn expression_relational() {
        let source = "
            void f(void) {
                1 == 1 >= 1 || 1 != 1 <= 1 && 1 < 1 > !1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::Expr(Expression::Infix {
                    left: Box::new(Expression::Infix {
                        left: Box::new(Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        }),
                        operator: "==",
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                            operator: ">=",
                            right: Box::new(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                        }),
                    }),
                    operator: "||",
                    right: Box::new(Expression::Infix {
                        left: Box::new(Expression::Infix {
                            left: Box::new(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                            operator: "!=",
                            right: Box::new(Expression::Infix {
                                left: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::default(),
                                }),
                                operator: "<=",
                                right: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::default(),
                                }),
                            }),
                        }),
                        operator: "&&",
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::Infix {
                                left: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::default(),
                                }),
                                operator: "<",
                                right: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::default(),
                                }),
                            }),
                            operator: ">",
                            right: Box::new(Expression::Prefix {
                                operator: "!",
                                expression: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::default(),
                                }),
                                location: Location::default(),
                            }),
                        }),
                    }),
                })],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn expression_compound_assignment() {
        let source = "
            void f(void) {
                1 += 1 -= 1 *= 1 /= 1 %= 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::Expr(Expression::Infix {
                    left: Box::new(Expression::IntConst {
                        value: 1,
                        location: Location::default(),
                    }),
                    operator: "+=",
                    right: Box::new(Expression::Infix {
                        left: Box::new(Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        }),
                        operator: "-=",
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                            operator: "*=",
                            right: Box::new(Expression::Infix {
                                left: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::default(),
                                }),
                                operator: "/=",
                                right: Box::new(Expression::Infix {
                                    left: Box::new(Expression::IntConst {
                                        value: 1,
                                        location: Location::default(),
                                    }),
                                    operator: "%=",
                                    right: Box::new(Expression::IntConst {
                                        value: 1,
                                        location: Location::default(),
                                    }),
                                }),
                            }),
                        }),
                    }),
                })],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn index_call_pointer_struct() {
        let source = "
            void f(void) {
                a[0];
                a(0);
                a.a + *a + &a + a->a;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::Expr(Expression::Index {
                        expression: Box::new(Expression::Ident {
                            value: "a".to_string(),
                            location: Location::default(),
                        }),
                        index: Box::new(Expression::IntConst {
                            value: 0,
                            location: Location::default(),
                        }),
                    }),
                    Statement::Expr(Expression::Call {
                        expression: Box::new(Expression::Ident {
                            value: "a".to_string(),
                            location: Location::default(),
                        }),
                        arguments: vec![Expression::IntConst {
                            value: 0,
                            location: Location::default(),
                        }],
                    }),
                    Statement::Expr(Expression::Infix {
                        left: Box::new(Expression::Infix {
                            left: Box::new(Expression::Infix {
                                left: Box::new(Expression::Infix {
                                    left: Box::new(Expression::Ident {
                                        value: "a".to_string(),
                                        location: Location::default(),
                                    }),
                                    operator: ".",
                                    right: Box::new(Expression::Ident {
                                        value: "a".to_string(),
                                        location: Location::default(),
                                    }),
                                }),
                                operator: "+",
                                right: Box::new(Expression::Prefix {
                                    operator: "*",
                                    expression: Box::new(Expression::Ident {
                                        value: "a".to_string(),
                                        location: Location::default(),
                                    }),
                                    location: Location::default(),
                                }),
                            }),
                            operator: "+",
                            right: Box::new(Expression::Prefix {
                                operator: "&",
                                expression: Box::new(Expression::Ident {
                                    value: "a".to_string(),
                                    location: Location::default(),
                                }),
                                location: Location::default(),
                            }),
                        }),
                        operator: "+",
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::Ident {
                                value: "a".to_string(),
                                location: Location::default(),
                            }),
                            operator: "->",
                            right: Box::new(Expression::Ident {
                                value: "a".to_string(),
                                location: Location::default(),
                            }),
                        }),
                    }),
                ],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn dummy_type() {
        let source = "
            f(a) {
                a = 1;
                b = 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            is_proto: false,
            return_type: Rc::new(RefCell::new(Type::T(None))),
            name: "f".to_string(),
            parameters: [("a".to_string(), Rc::new(RefCell::new(Type::T(None))))]
                .iter()
                .cloned()
                .collect(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::Expr(Expression::Infix {
                        left: Box::new(Expression::Ident {
                            value: "a".to_string(),
                            location: Location::default(),
                        }),
                        operator: "=",
                        right: Box::new(Expression::IntConst {
                            value: 1,
                            location: Location::default(),
                        }),
                    }),
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::T(None))),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::T(None))),
                            "b".to_string(),
                            Some(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                ],
                location: Location::default(),
            },
            location: Location::default(),
        }))];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn error_recovery() {
        let source = "
            void f(void) {
                break
                continue;
            }
        ";
        let expected_errors = vec![Error::Parsing {
            message: "Fail to assert `Token::Semicolon` here.".to_string(),
            location: Location::default(),
        }];
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        Parser::new(tokens, &mut errors).run().unwrap_err();
        assert_eq!(errors, expected_errors);
    }
}
