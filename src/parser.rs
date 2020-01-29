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
        self.envs.iter().find(|e| e.contains(name)).is_some()
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
            match self.tokens.last() {
                Some(Token::Struct(_)) => {
                    let location = self.tokens.pop().unwrap().locate();
                    match self.parse_type_struct(location) {
                        Ok(struct_) => {
                            self.assert_token("Semicolon")?;
                            self.generic_ast
                                .as_mut()
                                .unwrap()
                                .push(StaticObject::Type(struct_));
                        }
                        Err(_) => self.tokens.clear(),
                    }
                }
                Some(_) => match self.parse_function() {
                    Ok(func) => self
                        .generic_ast
                        .as_mut()
                        .unwrap()
                        .push(StaticObject::Function(Box::new(func))),
                    Err(_) => self.tokens.clear(),
                },
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

    fn parse_function(&mut self) -> Result<Function, ()> {
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
            Some(Token::LParen(_)) => {}
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
        loop {
            match self.tokens.last() {
                Some(Token::RParen(_)) => {
                    self.tokens.pop();
                    break;
                }
                Some(Token::Ellipsis(_)) => {
                    self.tokens.pop();
                    ellipsis = true;
                    self.assert_token("RParen")?;
                    break;
                }
                None => {
                    self.push_error("Unexpected EOF.", Location::default());
                    return Err(());
                }
                _ => loop {
                    let type_ = self.parse_type()?;
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
                    }
                },
            }
        }
        // Parse the function body.
        let body = self.parse_statement()?;
        // Leave the scope.
        self.environment.leave();
        // Return the function node.
        Ok(Function {
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
                Some(Token::Short(location)) => Ok(Type::Short(Some(location))),
                Some(Token::Int(location)) => Ok(Type::Int(Some(location))),
                Some(Token::Long(location)) => Ok(Type::Long(Some(location))),
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
                Some(Token::Short(location)) => Ok(Type::UnsignedShort(Some(location))),
                Some(Token::Int(location)) => Ok(Type::UnsignedInt(Some(location))),
                Some(Token::Long(location)) => Ok(Type::UnsignedLong(Some(location))),
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
            self.environment.get_struct(&name).ok_or(())
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

            Some(type_ @ Token::T(_))
            | Some(type_ @ Token::Void(_))
            | Some(type_ @ Token::Char(_))
            | Some(type_ @ Token::Short(_))
            | Some(type_ @ Token::Int(_))
            | Some(type_ @ Token::Long(_))
            | Some(type_ @ Token::Float(_))
            | Some(type_ @ Token::Double(_))
            | Some(type_ @ Token::Signed(_))
            | Some(type_ @ Token::Unsigned(_))
            | Some(type_ @ Token::Struct(_)) => {
                self.tokens.push(type_);
                self.parse_statement_def()
            }

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
        // Parse `return;`.
        if let Some(Token::Semicolon(_)) = self.tokens.last() {
            self.tokens.pop();
            Ok(Statement::Return {
                expression: None,
                location,
            })
        } else {
            // Parse `return expr;`.
            let expr = self
                .parse_expression(0)
                .map_err(|_| self.skip_to_semicolon())?;
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
        self.assert_token("LParen")
            .map_err(|_| self.skip_to_semicolon())?;
        let condition = self
            .parse_expression(0)
            .map_err(|_| self.skip_to_semicolon())?;
        self.assert_token("RParen")
            .map_err(|_| self.skip_to_semicolon())?;
        let body = self
            .parse_statement()
            .map_err(|_| self.skip_to_semicolon())?;
        Ok(Statement::While {
            condition,
            body: Box::new(body),
            location,
        })
    }

    fn parse_statement_do(&mut self, location: Location) -> Result<Statement, ()> {
        let body = self
            .parse_statement()
            .map_err(|_| self.skip_to_semicolon())?;
        self.assert_token("While")
            .map_err(|_| self.skip_to_semicolon())?;
        self.assert_token("LParen")
            .map_err(|_| self.skip_to_semicolon())?;
        let condition = self
            .parse_expression(0)
            .map_err(|_| self.skip_to_semicolon())?;
        self.assert_token("RParen")
            .map_err(|_| self.skip_to_semicolon())?;
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
        self.assert_token("LParen")
            .map_err(|_| self.skip_to_semicolon())?;
        let initialization = match self.tokens.last() {
            Some(Token::Semicolon(_)) => {
                self.tokens.pop();
                None
            }
            _ => Some(Box::new(self.parse_statement()?)),
        };
        let condition = match self.tokens.last() {
            Some(Token::Semicolon(_)) => None,
            _ => Some(self.parse_expression(0)?),
        };
        self.assert_token("Semicolon")
            .map_err(|_| self.skip_to_semicolon())?;
        let increment = match self.tokens.last() {
            Some(Token::RParen(_)) => None,
            _ => Some(self.parse_expression(0)?),
        };
        self.assert_token("RParen")
            .map_err(|_| self.skip_to_semicolon())?;
        let body = self
            .parse_statement()
            .map_err(|_| self.skip_to_semicolon())?;
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
        self.assert_token("LParen")
            .map_err(|_| self.skip_to_semicolon())?;
        let condition = self
            .parse_expression(0)
            .map_err(|_| self.skip_to_semicolon())?;
        self.assert_token("RParen")
            .map_err(|_| self.skip_to_semicolon())?;
        let body = self
            .parse_statement()
            .map_err(|_| self.skip_to_semicolon())?;
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
        self.assert_token("LParen")
            .map_err(|_| self.skip_to_semicolon())?;
        let expression = self
            .parse_expression(0)
            .map_err(|_| self.skip_to_semicolon())?;
        self.assert_token("RParen")
            .map_err(|_| self.skip_to_semicolon())?;
        self.assert_token("LBrace")
            .map_err(|_| self.skip_to_semicolon())?;
        let mut default = None;
        let mut branches = Vec::new();
        loop {
            match self.tokens.pop() {
                // Parse `case`.
                Some(Token::Case(_)) => {
                    let expr = self
                        .parse_expression(0)
                        .map_err(|_| self.skip_to_semicolon())?;
                    self.assert_token("Colon")
                        .map_err(|_| self.skip_to_semicolon())?;
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
                    self.assert_token("Colon")
                        .map_err(|_| self.skip_to_semicolon())?;
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
        let type_ = self.parse_type().map_err(|_| self.skip_to_semicolon())?;
        let location = type_.locate();
        let mut declarators = Vec::new();
        loop {
            // Parse the delimiter.
            match self.tokens.pop() {
                Some(Token::Comma(_)) => {}
                Some(Token::Semicolon(_)) => {
                    break;
                }
                Some(tk) => self.tokens.push(tk),
                None => {
                    self.push_error("Unexpected EOF.", Location::default());
                    return Err(());
                }
            }
            // Check for pointer types.
            let mut pointer_flag = false;
            if let Some(Token::Asterisk { .. }) = self.tokens.last() {
                self.tokens.pop();
                pointer_flag = true;
            }
            match self.tokens.pop() {
                Some(Token::Ident { literal, .. }) => {
                    let mut type_ = type_.set_pointer_flag(pointer_flag);
                    self.environment.define(&literal);
                    // Check for array types.
                    if let Some(Token::LBracket(_)) = self.tokens.last() {
                        self.tokens.pop();
                        match self.tokens.last() {
                            Some(Token::RBracket(_)) => (),
                            _ => {
                                let expr = self
                                    .parse_expression(0)
                                    .map_err(|_| self.skip_to_semicolon())?;
                                match expr {
                                    Expression::IntConst { .. } => (),
                                    expr => {
                                        self.push_error(
                                            "Expect a integral literal.",
                                            expr.locate(),
                                        );
                                    }
                                }
                            }
                        };
                        self.assert_token("RBracket")
                            .map_err(|_| self.skip_to_semicolon())?;
                        type_ = type_.set_array_flag(true);
                    }
                    // Parse the initializer.
                    let initializer = if let Some(Token::Equal(_)) = self.tokens.last() {
                        self.tokens.pop();
                        let value = match self.tokens.last() {
                            Some(Token::LBrace(_)) => self.parse_expression_init_list()?,
                            _ => self.parse_expression(0)?,
                        };
                        Some(value)
                    } else {
                        None
                    };
                    declarators.push((Rc::new(RefCell::new(type_)), literal, initializer));
                }
                Some(tk) => {
                    self.push_error("Expect a identifier.", tk.locate());
                    self.tokens.push(tk);
                    return Err(());
                }
                None => unreachable!(),
            }
        }
        Ok(Statement::Def {
            base_type: Rc::new(RefCell::new(type_)),
            declarators,
            location,
        })
    }

    fn parse_statement_expr(&mut self) -> Result<Statement, ()> {
        let expr = self
            .parse_expression(0)
            .map_err(|_| self.skip_to_semicolon())?;
        self.assert_token("Semicolon")
            .map_err(|_| self.skip_to_semicolon())?;
        // Transform an assignment to a definition if the name is not defined yet.
        if let Expression::Infix {
            left,
            operator: "=",
            right,
        } = expr
        {
            if let Expression::Ident { value, location } = *left {
                if !self.environment.is_defined(&value) {
                    self.environment.define(&value);
                    let type_ = Type::T {
                        array_flag: false,
                        pointer_flag: false,
                        specialized: None,
                    };
                    Ok(Statement::Def {
                        base_type: Rc::new(RefCell::new(type_.clone())),
                        declarators: vec![(Rc::new(RefCell::new(type_)), value, Some(*right))],
                        location,
                    })
                } else {
                    Ok(Statement::Expr(Expression::Infix {
                        left: Box::new(Expression::Ident { value, location }),
                        operator: "=",
                        right,
                    }))
                }
            } else if let Expression::Index { expression, index } = *left {
                if let Expression::Ident { value, location } = *expression {
                    if !self.environment.is_defined(&value) {
                        self.environment.define(&value);
                        let type_ = Type::T {
                            array_flag: false,
                            pointer_flag: false,
                            specialized: None,
                        };
                        Ok(Statement::Def {
                            base_type: Rc::new(RefCell::new(type_.clone())),
                            declarators: vec![(
                                Rc::new(RefCell::new(type_.set_array_flag(true))),
                                value,
                                Some(*right),
                            )],
                            location,
                        })
                    } else {
                        Ok(Statement::Expr(Expression::Index {
                            expression: Box::new(Expression::Ident { value, location }),
                            index,
                        }))
                    }
                } else {
                    Ok(Statement::Expr(Expression::Index { expression, index }))
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
        let mut pairs = Vec::new();
        let location = self.assert_token("LBrace")?.locate();
        loop {
            match self.tokens.last() {
                Some(Token::RBrace(_)) => break,
                Some(Token::Comma(_)) => {
                    self.tokens.pop();
                }
                Some(Token::Dot(_)) => {
                    let location = self.tokens.pop().unwrap().locate();
                    match self.tokens.pop() {
                        Some(Token::Ident { literal, .. }) => {
                            if let Some(Token::Equal(_)) = self.tokens.last() {
                                self.tokens.pop();
                                pairs.push((Some(literal), self.parse_expression(0)?));
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
                }
                Some(_) => pairs.push((None, self.parse_expression(0)?)),
                None => {
                    self.push_error("Unexpected EOF.", Location::default());
                    return Err(());
                }
            }
        }
        self.assert_token("RBrace")
            .map_err(|_| self.skip_to_semicolon())?;
        Ok(Expression::InitList { pairs, location })
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<Expression, ()> {
        let mut left = self.parse_prefix().map_err(|_| self.skip_to_semicolon())?;
        while self.get_infix_precedence() != 0 && precedence < self.get_infix_precedence() {
            left = self
                .parse_infix(self.get_infix_precedence(), left)
                .map_err(|_| self.skip_to_semicolon())?;
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
                self.assert_token("RParen")
                    .map_err(|_| self.skip_to_semicolon())?;
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
            Some(Token::Equal(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "=",
                right: Box::new(self.parse_expression(precedence - 1)?),
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
            Some(Token::Or(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "||",
                right: Box::new(self.parse_expression(precedence)?),
            }),
            Some(Token::And(_)) => Ok(Expression::Infix {
                left: Box::new(left),
                operator: "&&",
                right: Box::new(self.parse_expression(precedence)?),
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
            Some(Token::BiPlus(_)) => Ok(Expression::Suffix {
                operator: "++",
                expression: Box::new(left),
            }),
            Some(Token::BiMinus(_)) => Ok(Expression::Suffix {
                operator: "--",
                expression: Box::new(left),
            }),
            Some(Token::LBracket(_)) => {
                let index = self
                    .parse_expression(0)
                    .map_err(|_| self.skip_to_semicolon())?;
                self.assert_token("RBracket")
                    .map_err(|_| self.skip_to_semicolon())?;
                Ok(Expression::Index {
                    expression: Box::new(left),
                    index: Box::new(index),
                })
            }
            Some(Token::LParen(_)) => {
                let mut arguments = Vec::new();
                match self.tokens.last() {
                    Some(Token::RParen(_)) => {
                        self.tokens.pop();
                    }
                    _ => loop {
                        let arg = self
                            .parse_expression(0)
                            .map_err(|_| self.skip_to_semicolon())?;
                        arguments.push(arg);
                        match self.tokens.pop() {
                            Some(Token::Comma(_)) => {}
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
                    },
                }
                Ok(Expression::Call {
                    expression: Box::new(left),
                    arguments,
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
            array_flag: false,
            pointer_flag: false,
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
    fn return_pointer() {
        let source = "int *f() {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            return_type: Rc::new(RefCell::new(Type::Int {
                signed_flag: true,
                array_flag: false,
                pointer_flag: true,
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
    fn function_empty() {
        let source = "void f() {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: [(
                "a".to_string(),
                Rc::new(RefCell::new(Type::Int {
                    signed_flag: true,
                    array_flag: false,
                    pointer_flag: false,
                    location: Some(Location::default()),
                })),
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
        let source = "
            void f() {}
            void f(int a, int b, ...) {}
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            StaticObject::Function(Box::new(Function {
                return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
                name: "f".to_string(),
                parameters: IndexMap::new(),
                ellipsis: false,
                body: Statement::Block {
                    statements: vec![],
                    location: Location::default(),
                },
                location: Location::default(),
            })),
            StaticObject::Function(Box::new(Function {
                return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
                name: "f".to_string(),
                parameters: [
                    (
                        "a".to_string(),
                        Rc::new(RefCell::new(Type::Int {
                            signed_flag: true,
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                    ),
                    (
                        "b".to_string(),
                        Rc::new(RefCell::new(Type::Int {
                            signed_flag: true,
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
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
    fn statement_null() {
        let source = "
            void f() ;
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Null(Location::default()),
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
        let source = "void f() { continue; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
        let source = "void f() { break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
            void f() {
                return;
                return 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
    fn statement_def() {
        let source = "
            void f() {
                a = 1;
                char a[1];
                short a[] = { 1 };
                unsigned short a[1] = { 1 };
                int a[2] = { 1, 2 };
                unsigned int *a;
                long *a = &a;
                unsigned long a = *a;
                float a;
                double a = 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::T {
                            array_flag: false,
                            pointer_flag: false,
                            specialized: None,
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::T {
                                array_flag: false,
                                pointer_flag: false,
                                specialized: None,
                            })),
                            "a".to_string(),
                            Some(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Char {
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Char {
                                array_flag: true,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            None,
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Short {
                            signed_flag: true,
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Short {
                                signed_flag: true,
                                array_flag: true,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            Some(Expression::InitList {
                                pairs: vec![(
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
                        base_type: Rc::new(RefCell::new(Type::Short {
                            signed_flag: false,
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Short {
                                signed_flag: false,
                                array_flag: true,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            Some(Expression::InitList {
                                pairs: vec![(
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
                        base_type: Rc::new(RefCell::new(Type::Int {
                            signed_flag: true,
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Int {
                                signed_flag: true,
                                array_flag: true,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            Some(Expression::InitList {
                                pairs: vec![
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
                        base_type: Rc::new(RefCell::new(Type::Int {
                            signed_flag: false,
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Int {
                                signed_flag: false,
                                array_flag: false,
                                pointer_flag: true,
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            None,
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Long {
                            signed_flag: true,
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Long {
                                signed_flag: true,
                                array_flag: false,
                                pointer_flag: true,
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
                        base_type: Rc::new(RefCell::new(Type::Long {
                            signed_flag: false,
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Long {
                                signed_flag: false,
                                array_flag: false,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
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
                        base_type: Rc::new(RefCell::new(Type::Float {
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Float {
                                array_flag: false,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            None,
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Double {
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Double {
                                array_flag: false,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
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
            void f() {
                struct A {} a;
                struct B {
                    int a;
                } b = { 1 };
                struct C {
                    int a;
                    float b;
                } c = { 1, .b = 2 };
                struct D {};
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Struct {
                                name: "A".to_string(),
                                members: IndexMap::new(),
                                array_flag: false,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
                            "a".to_string(),
                            None,
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Struct {
                            name: "B".to_string(),
                            members: [(
                                "a".to_string(),
                                Type::Int {
                                    signed_flag: true,
                                    array_flag: false,
                                    pointer_flag: false,
                                    location: Some(Location::default()),
                                },
                            )]
                            .iter()
                            .cloned()
                            .collect(),
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Struct {
                                name: "B".to_string(),
                                members: [(
                                    "a".to_string(),
                                    Type::Int {
                                        signed_flag: true,
                                        array_flag: false,
                                        pointer_flag: false,
                                        location: Some(Location::default()),
                                    },
                                )]
                                .iter()
                                .cloned()
                                .collect(),
                                array_flag: false,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
                            "b".to_string(),
                            Some(Expression::InitList {
                                pairs: vec![(
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
                            name: "C".to_string(),
                            members: [
                                (
                                    "a".to_string(),
                                    Type::Int {
                                        signed_flag: true,
                                        array_flag: false,
                                        pointer_flag: false,
                                        location: Some(Location::default()),
                                    },
                                ),
                                (
                                    "b".to_string(),
                                    Type::Float {
                                        array_flag: false,
                                        pointer_flag: false,
                                        location: Some(Location::default()),
                                    },
                                ),
                            ]
                            .iter()
                            .cloned()
                            .collect(),
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::Struct {
                                name: "C".to_string(),
                                members: [
                                    (
                                        "a".to_string(),
                                        Type::Int {
                                            signed_flag: true,
                                            array_flag: false,
                                            pointer_flag: false,
                                            location: Some(Location::default()),
                                        },
                                    ),
                                    (
                                        "b".to_string(),
                                        Type::Float {
                                            array_flag: false,
                                            pointer_flag: false,
                                            location: Some(Location::default()),
                                        },
                                    ),
                                ]
                                .iter()
                                .cloned()
                                .collect(),
                                array_flag: false,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
                            "c".to_string(),
                            Some(Expression::InitList {
                                pairs: vec![
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
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::Struct {
                            name: "D".to_string(),
                            members: IndexMap::new(),
                            array_flag: false,
                            pointer_flag: false,
                            location: Some(Location::default()),
                        })),
                        declarators: vec![],
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
    fn statement_while() {
        let source = "
            void f() {
                while (1) ;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
            void f() {
                do ; while (1);
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: [(
                "a".to_string(),
                Rc::new(RefCell::new(Type::Int {
                    signed_flag: true,
                    array_flag: false,
                    pointer_flag: false,
                    location: Some(Location::default()),
                })),
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
                        increment: Some(Expression::Suffix {
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
                            base_type: Rc::new(RefCell::new(Type::Int {
                                signed_flag: true,
                                array_flag: false,
                                pointer_flag: false,
                                location: Some(Location::default()),
                            })),
                            declarators: vec![(
                                Rc::new(RefCell::new(Type::Int {
                                    signed_flag: true,
                                    array_flag: false,
                                    pointer_flag: false,
                                    location: Some(Location::default()),
                                })),
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
                        increment: Some(Expression::Suffix {
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
            void f() {
                if (1) ;
                if (1) ; else ;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
            void f() {
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
            void f() {
                a;
                1;
                1.1;
                'a';
                \"a\";
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
            void f() {
                1 = ++1 + (--1 - 1++) * 1-- / 1 % 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
                                            right: Box::new(Expression::Suffix {
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
                                    right: Box::new(Expression::Suffix {
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
            void f() {
                !1 == 1 || 1 != 1 && 1 < 1 > 1 <= 1 >= 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            return_type: Rc::new(RefCell::new(Type::Void(Some(Location::default())))),
            name: "f".to_string(),
            parameters: IndexMap::new(),
            ellipsis: false,
            body: Statement::Block {
                statements: vec![Statement::Expr(Expression::Infix {
                    left: Box::new(Expression::Infix {
                        left: Box::new(Expression::Prefix {
                            operator: "!",
                            expression: Box::new(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                            location: Location::default(),
                        }),
                        operator: "==",
                        right: Box::new(Expression::IntConst {
                            value: 1,
                            location: Location::default(),
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
                            right: Box::new(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                        }),
                        operator: "&&",
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::Infix {
                                left: Box::new(Expression::Infix {
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
                                    right: Box::new(Expression::IntConst {
                                        value: 1,
                                        location: Location::default(),
                                    }),
                                }),
                                operator: "<=",
                                right: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::default(),
                                }),
                            }),
                            operator: ">=",
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
    fn expression_compound_assignment() {
        let source = "
            void f() {
                1 += 1 -= 1 *= 1 /= 1 %= 1;
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
    fn index_call_pointer() {
        let source = "void f() {
            a[0];
            a(0);
            a + *a + &a + a->a;
        }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
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
                                left: Box::new(Expression::Ident {
                                    value: "a".to_string(),
                                    location: Location::default(),
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
                c[1] = { 1 }; // allow empty index
            }
        ";
        let expected_errors = vec![];
        let expected_generic_ast = vec![StaticObject::Function(Box::new(Function {
            return_type: Rc::new(RefCell::new(Type::T {
                array_flag: false,
                pointer_flag: false,
                specialized: None,
            })),
            name: "f".to_string(),
            parameters: [(
                "a".to_string(),
                Rc::new(RefCell::new(Type::T {
                    array_flag: false,
                    pointer_flag: false,
                    specialized: None,
                })),
            )]
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
                        base_type: Rc::new(RefCell::new(Type::T {
                            array_flag: false,
                            pointer_flag: false,
                            specialized: None,
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::T {
                                array_flag: false,
                                pointer_flag: false,
                                specialized: None,
                            })),
                            "b".to_string(),
                            Some(Expression::IntConst {
                                value: 1,
                                location: Location::default(),
                            }),
                        )],
                        location: Location::default(),
                    },
                    Statement::Def {
                        base_type: Rc::new(RefCell::new(Type::T {
                            array_flag: false,
                            pointer_flag: false,
                            specialized: None,
                        })),
                        declarators: vec![(
                            Rc::new(RefCell::new(Type::T {
                                array_flag: true,
                                pointer_flag: false,
                                specialized: None,
                            })),
                            "c".to_string(),
                            Some(Expression::InitList {
                                pairs: vec![(
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
            void f() {
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
}
