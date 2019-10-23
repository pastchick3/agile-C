use std::collections::HashSet;

use indexmap::IndexMap;

use crate::structure::{
    Array, Error, Expression, Function, Locate, Location, Statement, Token, Type,
};

struct Environment {
    envs: Vec<HashSet<String>>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            envs: vec![HashSet::new()],
        }
    }

    fn enter(&mut self) {
        self.envs.push(HashSet::new());
    }

    fn leave(&mut self) {
        self.envs.pop();
    }

    fn define(&mut self, name: &str) {
        self.envs.last_mut().unwrap().insert(String::from(name));
    }

    fn is_defined(&self, name: &str) -> bool {
        let defined: Vec<_> = self.envs.iter().filter(|e| e.contains(name)).collect();
        defined.len() != 0
    }
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    errors: Option<Vec<Error>>,
    generic_ast: Option<Vec<Function<'a>>>,
    environment: Environment,
}

impl<'a> Parser<'a> {
    pub fn new(mut tokens: Vec<Token<'a>>, errors: Vec<Error>) -> Parser<'a> {
        tokens.reverse();
        Parser {
            tokens,
            errors: Some(errors),
            generic_ast: Some(Vec::new()),
            environment: Environment::new(),
        }
    }

    pub fn run(&mut self) -> (Vec<Function<'a>>, Vec<Error>) {
        loop {
            match self.tokens.last() {
                Some(_) => match self.parse_function() {
                    Ok(func) => self.generic_ast.as_mut().unwrap().push(func),
                    Err(_) => self.tokens.clear(),
                },
                None => break,
            }
        }
        (
            self.generic_ast.take().unwrap(),
            self.errors.take().unwrap(),
        )
    }

    fn assert_token(&mut self, name: &str) -> Result<Token, ()> {
        match self.tokens.pop() {
            Some(tk) => {
                if format!("{:?}", tk).contains(name) {
                    Ok(tk)
                } else {
                    let message = format!("Expect `Token::{}` here.", name);
                    self.push_error(&message, Some(&tk));
                    self.tokens.push(tk);
                    Err(())
                }
            }
            None => Err(self.push_error("Unexpected EOF.", None)),
        }
    }

    fn get_prefix_precedence(&self) -> u8 {
        match self.tokens.last() {
            Some(Token::Not(_)) => 15,
            Some(Token::Plus(_)) => 15,
            Some(Token::Minus(_)) => 15,
            Some(Token::BiPlus(_)) => 15,
            Some(Token::BiMinus(_)) => 15,
            _ => 0,
        }
    }

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
            _ => 0,
        }
    }

    fn push_error(&mut self, message: &str, token: Option<&Token>) {
        let location = match token {
            Some(tk) => tk.locate(),
            None => Location::empty(),
        };
        self.errors.as_mut().unwrap().push(Error::Parsing {
            message: message.to_string(),
            location,
        });
    }

    fn parse_function(&mut self) -> Result<Function<'a>, ()> {
        self.environment.enter();
        let location = match self.tokens.last() {
            Some(tk) => tk.locate(),
            None => return Err(self.push_error("Unexpected EOF.", None)),
        };
        let r#type = self.parse_type()?;
        let name = match self.tokens.pop() {
            Some(Token::Ident { literal, .. }) => literal,
            Some(tk) => return Err(self.push_error("Expect a function name.", Some(&tk))),
            None => return Err(self.push_error("Unexpected EOF.", None)),
        };
        match self.tokens.pop() {
            Some(Token::LParen(_)) => {}
            Some(tk) => return Err(self.push_error("Expect `(`.", Some(&tk))),
            None => return Err(self.push_error("Unexpected EOF.", None)),
        }
        let mut parameters = IndexMap::new();
        match self.tokens.last() {
            Some(Token::RParen(_)) => {
                self.tokens.pop();
            }
            _ => loop {
                let r#type = self.parse_type()?;
                let name = match self.tokens.pop() {
                    Some(Token::Ident { literal, .. }) => {
                        self.environment.define(literal);
                        literal
                    }
                    Some(tk) => return Err(self.push_error("Expect a parameter name.", Some(&tk))),
                    None => return Err(self.push_error("Unexpected EOF.", None)),
                };
                parameters.insert(name, r#type);
                match self.tokens.pop() {
                    Some(Token::Comma(_)) => {}
                    Some(Token::RParen(_)) => break,
                    Some(tk) => return Err(self.push_error("Expect `,` or `)`.", Some(&tk))),
                    None => return Err(self.push_error("Unexpected EOF.", None)),
                }
            },
        }
        let body = self.parse_statement()?;
        self.environment.leave();
        Ok(Function {
            r#type,
            name,
            parameters,
            body,
            location,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ()> {
        match self.tokens.pop() {
            Some(Token::T(location)) => Ok(Type::T {
                dummy_flag: false,
                array_flag: false,
                array_len: None,
                location,
            }),
            Some(Token::Void(location)) => Ok(Type::Void {
                array_flag: false,
                array_len: None,
                location,
            }),
            Some(Token::Char(location)) => Ok(Type::Char {
                array_flag: false,
                array_len: None,
                location,
            }),
            Some(Token::Short(location)) => Ok(Type::Short {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location,
            }),
            Some(Token::Int(location)) => Ok(Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location,
            }),
            Some(Token::Long(location)) => Ok(Type::Long {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location,
            }),
            Some(Token::Float(location)) => Ok(Type::Float {
                array_flag: false,
                array_len: None,
                location,
            }),
            Some(Token::Double(location)) => Ok(Type::Double {
                array_flag: false,
                array_len: None,
                location,
            }),
            Some(Token::Signed(location)) => match self.tokens.pop() {
                Some(Token::Short(_)) => Ok(Type::Short {
                    signed_flag: true,
                    array_flag: false,
                    array_len: None,
                    location,
                }),
                Some(Token::Int(_)) => Ok(Type::Int {
                    signed_flag: true,
                    array_flag: false,
                    array_len: None,
                    location,
                }),
                Some(Token::Long(_)) => Ok(Type::Long {
                    signed_flag: true,
                    array_flag: false,
                    array_len: None,
                    location,
                }),
                Some(tk) => {
                    self.push_error(
                        "Expect `short`, `int`, or `long` after `signed`.",
                        Some(&tk),
                    );
                    self.tokens.push(tk);
                    Err(())
                }
                None => Err(self.push_error("Unexpected EOF.", None)),
            },
            Some(Token::Unsigned(location)) => match self.tokens.pop() {
                Some(Token::Short(_)) => Ok(Type::Short {
                    signed_flag: false,
                    array_flag: false,
                    array_len: None,
                    location,
                }),
                Some(Token::Int(_)) => Ok(Type::Int {
                    signed_flag: false,
                    array_flag: false,
                    array_len: None,
                    location,
                }),
                Some(Token::Long(_)) => Ok(Type::Long {
                    signed_flag: false,
                    array_flag: false,
                    array_len: None,
                    location,
                }),
                Some(tk) => {
                    self.push_error(
                        "Expect `short`, `int`, or `long` after `unsigned`.",
                        Some(&tk),
                    );
                    self.tokens.push(tk);
                    Err(())
                }
                None => Err(self.push_error("Unexpected EOF.", None)),
            },
            Some(tk) => {
                self.tokens.push(tk);
                Ok(Type::T {
                    dummy_flag: true,
                    array_flag: false,
                    array_len: None,
                    location: Location::empty(),
                })
            }
            None => Err(self.push_error("Unexpected EOF.", None)),
        }
    }

    fn parse_statement(&mut self) -> Result<Statement<'a>, ()> {
        match self.tokens.pop() {
            Some(Token::Continue(loc)) => self.parse_statement_continue(loc),
            Some(Token::Break(loc)) => self.parse_statement_break(loc),
            Some(Token::Return(loc)) => self.parse_statement_return(loc),
            Some(Token::LBrace(loc)) => self.parse_statement_block(loc),
            Some(Token::While(loc)) => self.parse_statement_while(loc),
            Some(Token::Do(loc)) => self.parse_statement_do(loc),
            Some(Token::For(loc)) => self.parse_statement_for(loc),
            Some(Token::If(loc)) => self.parse_statement_if(loc),
            Some(Token::Switch(loc)) => self.parse_statement_switch(loc),

            Some(r#type @ Token::T(_))
            | Some(r#type @ Token::Void(_))
            | Some(r#type @ Token::Char(_))
            | Some(r#type @ Token::Short(_))
            | Some(r#type @ Token::Int(_))
            | Some(r#type @ Token::Long(_))
            | Some(r#type @ Token::Float(_))
            | Some(r#type @ Token::Double(_))
            | Some(r#type @ Token::Signed(_))
            | Some(r#type @ Token::Unsigned(_)) => {
                self.tokens.push(r#type);
                self.parse_statement_def()
            }

            Some(tk) => {
                self.tokens.push(tk);
                self.parse_statement_expr()
            }

            None => Err(self.push_error("Unexpected EOF.", None)),
        }
    }

    fn parse_statement_continue(&mut self, location: Location) -> Result<Statement<'a>, ()> {
        self.assert_token("Semicolon")?;
        Ok(Statement::Continue(location))
    }

    fn parse_statement_break(&mut self, location: Location) -> Result<Statement<'a>, ()> {
        self.assert_token("Semicolon")?;
        Ok(Statement::Break(location))
    }

    fn parse_statement_return(&mut self, location: Location) -> Result<Statement<'a>, ()> {
        if let Some(Token::Semicolon(_)) = self.tokens.last() {
            self.tokens.pop();
            return Ok(Statement::Return {
                expression: None,
                location,
            });
        }
        match self.parse_expression(0) {
            Err(_) => match self.assert_token("Semicolon") {
                _ => Err(()),
            },
            Ok(expr) => match self.assert_token("Semicolon") {
                Ok(_) => Ok(Statement::Return {
                    expression: Some(expr),
                    location,
                }),
                Err(_) => Err(()),
            },
        }
    }

    fn parse_statement_block(&mut self, location: Location) -> Result<Statement<'a>, ()> {
        self.environment.enter();
        let mut statements = Vec::new();
        loop {
            match self.tokens.last() {
                Some(Token::RBrace(_)) => {
                    self.tokens.pop();
                    break;
                }
                Some(_) => match self.parse_statement() {
                    Ok(stmt) => statements.push(Box::new(stmt)),
                    Err(_) => {}
                },
                None => return Err(self.push_error("Unexpected EOF.", None)),
            }
        }
        self.environment.leave();
        Ok(Statement::Block {
            statements,
            location,
        })
    }

    fn parse_statement_while(&mut self, location: Location) -> Result<Statement<'a>, ()> {
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

    fn parse_statement_do(&mut self, location: Location) -> Result<Statement<'a>, ()> {
        let body = self.parse_statement()?;
        self.assert_token("While")?;
        self.assert_token("LParen")?;
        let condition = self.parse_expression(0)?;
        self.assert_token("RParen")?;
        self.assert_token("Semicolon")?;
        Ok(Statement::Do {
            condition,
            body: Box::new(body),
            location,
        })
    }

    fn parse_statement_for(&mut self, location: Location) -> Result<Statement<'a>, ()> {
        self.assert_token("LParen")?;
        let initialization = match self.tokens.last() {
            Some(Token::Semicolon(_)) => None,
            _ => Some(self.parse_expression(0)?),
        };
        self.assert_token("Semicolon")?;
        let condition = match self.tokens.last() {
            Some(Token::Semicolon(_)) => None,
            _ => Some(self.parse_expression(0)?),
        };
        self.assert_token("Semicolon")?;
        let increment = match self.tokens.last() {
            Some(Token::RParen(_)) => None,
            _ => Some(self.parse_expression(0)?),
        };
        self.assert_token("RParen")?;
        let body = self.parse_statement()?;
        Ok(Statement::For {
            initialization,
            condition,
            increment,
            body: Box::new(body),
            location,
        })
    }

    fn parse_statement_if(&mut self, location: Location) -> Result<Statement<'a>, ()> {
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

    fn parse_statement_switch(&mut self, location: Location) -> Result<Statement<'a>, ()> {
        self.assert_token("LParen")?;
        let expression = self.parse_expression(0)?;
        self.assert_token("RParen")?;
        self.assert_token("LBrace")?;
        let mut default = None;
        let mut branches = Vec::new();
        loop {
            match self.tokens.pop() {
                Some(Token::Case(_)) => {
                    let expr = self.parse_expression(0)?;
                    self.assert_token("Colon")?;
                    let mut stmts = Vec::new();
                    loop {
                        match self.tokens.last() {
                            Some(Token::Case(_))
                            | Some(Token::Default(_))
                            | Some(Token::RBrace(_)) => break,
                            _ => stmts.push(Box::new(self.parse_statement()?)),
                        }
                    }
                    branches.push((expr, stmts));
                }
                Some(tk @ Token::Default(_)) => {
                    if !default.is_none() {
                        return Err(self.push_error("Multiple default.", Some(&tk)));
                    }
                    self.assert_token("Colon")?;
                    let mut stmts = Vec::new();
                    loop {
                        match self.tokens.last() {
                            Some(Token::Case(_))
                            | Some(Token::Default(_))
                            | Some(Token::RBrace(_)) => break,
                            _ => stmts.push(Box::new(self.parse_statement()?)),
                        }
                    }
                    default = Some(stmts);
                }
                Some(Token::RBrace(_)) => break,
                Some(tk) => {
                    self.push_error("Expect `case` or `default`.", Some(&tk));
                    self.tokens.push(tk);
                    return Err(());
                }
                None => return Err(self.push_error("Unexpected EOF.", None)),
            }
        }
        Ok(Statement::Switch {
            expression,
            branches,
            default,
            location,
        })
    }

    fn parse_statement_def(&mut self) -> Result<Statement<'a>, ()> {
        let r#type = self.parse_type()?;
        let location = r#type.locate();
        let mut declarators = Vec::new();
        loop {
            match self.tokens.pop() {
                Some(Token::Ident { literal, .. }) => {
                    self.environment.define(literal);
                    match self.tokens.last() {
                        Some(Token::Equal(_)) => {
                            self.tokens.pop();
                            let value = self.parse_expression(0)?;
                            declarators.push((r#type, literal, Some(value)));
                        }
                        Some(Token::LBracket(_)) => {
                            let lbracket = self.tokens.pop();
                            let array_len = match self.tokens.last() {
                                Some(Token::RBracket(_)) => None,
                                _ => {
                                    let expr = self.parse_expression(0)?;
                                    match expr {
                                        Expression::IntConst { value, .. } => Some(value as usize),
                                        _ => {
                                            self.push_error(
                                                "Expect a integral literal.",
                                                lbracket.as_ref(),
                                            );
                                            None
                                        }
                                    }
                                }
                            };
                            self.assert_token("RBracket")?;
                            let typ = r#type.set_array(true, array_len);
                            let init_list = match self.tokens.last() {
                                Some(Token::Equal(_)) => {
                                    self.tokens.pop();
                                    Some(self.parse_expression_init_list()?)
                                }
                                _ => None,
                            };
                            declarators.push((typ, literal, init_list));
                        }
                        _ => declarators.push((r#type, literal, None)),
                    }
                    match self.tokens.pop() {
                        Some(Token::Comma(_)) => {}
                        Some(Token::Semicolon(_)) => break,
                        Some(tk) => {
                            self.push_error("Expect `,` or `;`.", Some(&tk));
                            self.tokens.push(tk);
                            return Err(());
                        }
                        None => return Err(self.push_error("Unexpected EOF.", None)),
                    }
                }
                Some(tk) => {
                    self.push_error("Expect a identifier.", Some(&tk));
                    self.tokens.push(tk);
                    return Err(());
                }
                None => return Err(self.push_error("Unexpected EOF.", None)),
            }
        }
        Ok(Statement::Def {
            declarators,
            location,
        })
    }

    fn parse_statement_expr(&mut self) -> Result<Statement<'a>, ()> {
        let expr = self.parse_expression(0)?;
        self.assert_token("Semicolon")?;
        match expr {
            Expression::Infix {
                left,
                operator: "=",
                right,
            } => match *left {
                Expression::Ident { value, location } => {
                    if !self.environment.is_defined(value) {
                        self.environment.define(value);
                        let r#type = Type::T {
                            dummy_flag: true,
                            array_flag: false,
                            array_len: None,
                            location: Location::empty(),
                        };
                        Ok(Statement::Def {
                            declarators: vec![(r#type, value, Some(*right))],
                            location,
                        })
                    } else {
                        Ok(Statement::Expr(Expression::Infix {
                            left: Box::new(Expression::Ident { value, location }),
                            operator: "=",
                            right,
                        }))
                    }
                }
                _ => Ok(Statement::Expr(Expression::Infix {
                    left,
                    operator: "=",
                    right,
                })),
            },
            _ => Ok(Statement::Expr(expr)),
        }
    }

    fn parse_expression_init_list(&mut self) -> Result<Expression<'a>, ()> {
        let mut expressions = Vec::new();
        let lbrace = self.assert_token("LBrace")?;
        let location = lbrace.locate();
        loop {
            match self.tokens.last() {
                Some(Token::RBrace(_)) => break,
                Some(Token::Comma(_)) => {
                    self.tokens.pop();
                }
                Some(_) => expressions.push(Box::new(self.parse_expression(0)?)),
                None => return Err(self.push_error("Unexpected EOF.", None)),
            }
        }
        self.assert_token("RBrace")?;
        Ok(Expression::InitList {
            expressions,
            location,
        })
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<Expression<'a>, ()> {
        let mut left = self.parse_prefix()?;
        while self.get_infix_precedence() != 0 && precedence < self.get_infix_precedence() {
            left = self.parse_infix(self.get_infix_precedence(), left)?;
        }
        return Ok(left);
    }

    fn parse_prefix(&mut self) -> Result<Expression<'a>, ()> {
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
            Some(Token::LParen(_)) => {
                let expr = self.parse_expression(0);
                self.assert_token("RParen")?;
                expr
            }
            Some(tk) => {
                self.push_error("Expect a prefix operator.", Some(&tk));
                self.tokens.push(tk);
                return Err(());
            }
            None => return Err(self.push_error("Unexpected EOF.", None)),
        }
    }

    fn parse_infix(&mut self, precedence: u8, left: Expression<'a>) -> Result<Expression<'a>, ()> {
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
                let index = self.parse_expression(0)?;
                self.assert_token("RBracket")?;
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
                        let arg = self.parse_expression(0)?;
                        arguments.push(Box::new(arg));
                        match self.tokens.pop() {
                            Some(Token::Comma(_)) => {}
                            Some(Token::RParen(_)) => break,
                            Some(tk) => {
                                self.push_error("Expect `,` or `)`.", Some(&tk));
                                self.tokens.push(tk);
                                return Err(());
                            }
                            None => return Err(self.push_error("Unexpected EOF.", None)),
                        }
                    },
                }
                Ok(Expression::Call {
                    expression: Box::new(left),
                    arguments,
                })
            }
            Some(tk) => {
                self.push_error("Expect an infix operator.", Some(&tk));
                self.tokens.push(tk);
                return Err(());
            }
            None => return Err(self.push_error("Unexpected EOF.", None)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn function_empty() {
        let source = "T f() {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::T {
                dummy_flag: false,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![],
                location: Location::new(1, 7),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn function_single() {
        let source = "void f(int a) { continue; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Void {
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: [(
                "a",
                Type::Int {
                    signed_flag: true,
                    array_flag: false,
                    array_len: None,
                    location: Location::new(1, 8),
                },
            )]
            .iter()
            .cloned()
            .collect(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::Continue(Location::new(1, 17)))],
                location: Location::new(1, 15),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn function_multiple() {
        let source = "short f() {}
int f(long a, float b) { continue; break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                r#type: Type::Short {
                    signed_flag: true,
                    array_flag: false,
                    array_len: None,
                    location: Location::new(1, 1),
                },
                name: "f",
                parameters: IndexMap::new(),
                body: Statement::Block {
                    statements: vec![],
                    location: Location::new(1, 11),
                },
                location: Location::new(1, 1),
            },
            Function {
                r#type: Type::Int {
                    signed_flag: true,
                    array_flag: false,
                    array_len: None,
                    location: Location::new(2, 1),
                },
                name: "f",
                parameters: [
                    (
                        "a",
                        Type::Long {
                            signed_flag: true,
                            array_flag: false,
                            array_len: None,
                            location: Location::new(2, 7),
                        },
                    ),
                    (
                        "b",
                        Type::Float {
                            array_flag: false,
                            array_len: None,
                            location: Location::new(2, 15),
                        },
                    ),
                ]
                .iter()
                .cloned()
                .collect(),
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Continue(Location::new(2, 26))),
                        Box::new(Statement::Break(Location::new(2, 36))),
                    ],
                    location: Location::new(2, 24),
                },
                location: Location::new(2, 1),
            },
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_return() {
        let source = "double f() { return;}
signed short f() { return 1;}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                r#type: Type::Double {
                    array_flag: false,
                    array_len: None,
                    location: Location::new(1, 1),
                },
                name: "f",
                parameters: IndexMap::new(),
                body: Statement::Block {
                    statements: vec![Box::new(Statement::Return {
                        expression: None,
                        location: Location::new(1, 14),
                    })],
                    location: Location::new(1, 12),
                },
                location: Location::new(1, 1),
            },
            Function {
                r#type: Type::Short {
                    signed_flag: true,
                    array_flag: false,
                    array_len: None,
                    location: Location::new(2, 1),
                },
                name: "f",
                parameters: IndexMap::new(),
                body: Statement::Block {
                    statements: vec![Box::new(Statement::Return {
                        expression: Some(Expression::IntConst {
                            value: 1,
                            location: Location::new(2, 27),
                        }),
                        location: Location::new(2, 20),
                    })],
                    location: Location::new(2, 18),
                },
                location: Location::new(2, 1),
            },
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_def() {
        let source =
            "signed int f() { int a = 1, b, c[0], d[] = {}, e[1] = { 1 }, f[2] = { 1, 2 }; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::Def {
                    declarators: vec![
                        (
                            Type::Int {
                                signed_flag: true,
                                array_flag: false,
                                array_len: None,
                                location: Location::new(1, 18),
                            },
                            "a",
                            Some(Expression::IntConst {
                                value: 1,
                                location: Location::new(1, 26),
                            }),
                        ),
                        (
                            Type::Int {
                                signed_flag: true,
                                array_flag: false,
                                array_len: None,
                                location: Location::new(1, 18),
                            },
                            "b",
                            None,
                        ),
                        (
                            Type::Int {
                                signed_flag: true,
                                array_flag: true,
                                array_len: Some(0),
                                location: Location::new(1, 18),
                            },
                            "c",
                            None,
                        ),
                        (
                            Type::Int {
                                signed_flag: true,
                                array_flag: true,
                                array_len: None,
                                location: Location::new(1, 18),
                            },
                            "d",
                            Some(Expression::InitList {
                                expressions: vec![],
                                location: Location::new(1, 44),
                            }),
                        ),
                        (
                            Type::Int {
                                signed_flag: true,
                                array_flag: true,
                                array_len: Some(1),
                                location: Location::new(1, 18),
                            },
                            "e",
                            Some(Expression::InitList {
                                expressions: vec![Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::new(1, 57),
                                })],
                                location: Location::new(1, 55),
                            }),
                        ),
                        (
                            Type::Int {
                                signed_flag: true,
                                array_flag: true,
                                array_len: Some(2),
                                location: Location::new(1, 18),
                            },
                            "f",
                            Some(Expression::InitList {
                                expressions: vec![
                                    Box::new(Expression::IntConst {
                                        value: 1,
                                        location: Location::new(1, 71),
                                    }),
                                    Box::new(Expression::IntConst {
                                        value: 2,
                                        location: Location::new(1, 74),
                                    }),
                                ],
                                location: Location::new(1, 69),
                            }),
                        ),
                    ],
                    location: Location::new(1, 18),
                })],
                location: Location::new(1, 16),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_while() {
        let source = "signed long f() { while (1) break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Long {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::While {
                    condition: Expression::IntConst {
                        value: 1,
                        location: Location::new(1, 26),
                    },
                    body: Box::new(Statement::Break(Location::new(1, 29))),
                    location: Location::new(1, 19),
                })],
                location: Location::new(1, 17),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_do() {
        let source = "unsigned short f() { do break; while (1); }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Short {
                signed_flag: false,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::Do {
                    condition: Expression::IntConst {
                        value: 1,
                        location: Location::new(1, 39),
                    },
                    body: Box::new(Statement::Break(Location::new(1, 25))),
                    location: Location::new(1, 22),
                })],
                location: Location::new(1, 20),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_for() {
        let source = "unsigned int f() { for (; 1; ) break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: false,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::For {
                    initialization: None,
                    condition: Some(Expression::IntConst {
                        value: 1,
                        location: Location::new(1, 27),
                    }),
                    increment: None,
                    body: Box::new(Statement::Break(Location::new(1, 32))),
                    location: Location::new(1, 20),
                })],
                location: Location::new(1, 18),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_if() {
        let source = "unsigned long f() { if (1) break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Long {
                signed_flag: false,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::If {
                    condition: Expression::IntConst {
                        value: 1,
                        location: Location::new(1, 25),
                    },
                    body: Box::new(Statement::Break(Location::new(1, 28))),
                    alternative: None,
                    location: Location::new(1, 21),
                })],
                location: Location::new(1, 19),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_if_else() {
        let source = "int f() { if (1) break; else continue; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::If {
                    condition: Expression::IntConst {
                        value: 1,
                        location: Location::new(1, 15),
                    },
                    body: Box::new(Statement::Break(Location::new(1, 18))),
                    alternative: Some(Box::new(Statement::Continue(Location::new(1, 30)))),
                    location: Location::new(1, 11),
                })],
                location: Location::new(1, 9),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_switch() {
        let source = "int f() { switch (1) {case 1: break;} }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::Switch {
                    expression: Expression::IntConst {
                        value: 1,
                        location: Location::new(1, 19),
                    },
                    branches: vec![(
                        Expression::IntConst {
                            value: 1,
                            location: Location::new(1, 28),
                        },
                        vec![Box::new(Statement::Break(Location::new(1, 31)))],
                    )],
                    default: None,
                    location: Location::new(1, 11),
                })],
                location: Location::new(1, 9),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_switch_default() {
        let source = "int f() { switch (1) {case 1: break; default: continue;} }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::Switch {
                    expression: Expression::IntConst {
                        value: 1,
                        location: Location::new(1, 19),
                    },
                    branches: vec![(
                        Expression::IntConst {
                            value: 1,
                            location: Location::new(1, 28),
                        },
                        vec![Box::new(Statement::Break(Location::new(1, 31)))],
                    )],
                    default: Some(vec![Box::new(Statement::Continue(Location::new(1, 47)))]),
                    location: Location::new(1, 11),
                })],
                location: Location::new(1, 9),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn expression_basic() {
        let source = "int f() { a; 1; 1.0; 'b'; \"c\"; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![
                    Box::new(Statement::Expr(Expression::Ident {
                        value: "a",
                        location: Location::new(1, 11),
                    })),
                    Box::new(Statement::Expr(Expression::IntConst {
                        value: 1,
                        location: Location::new(1, 14),
                    })),
                    Box::new(Statement::Expr(Expression::FloatConst {
                        value: 1.0,
                        location: Location::new(1, 17),
                    })),
                    Box::new(Statement::Expr(Expression::CharConst {
                        value: "'b'",
                        location: Location::new(1, 22),
                    })),
                    Box::new(Statement::Expr(Expression::StrConst {
                        value: "\"c\"",
                        location: Location::new(1, 27),
                    })),
                ],
                location: Location::new(1, 9),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn expression_arithmetic() {
        let source = "int f() { 0=++1+--2-3++*4--/(5%6)%7; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::Expr(Expression::Infix {
                    left: Box::new(Expression::IntConst {
                        value: 0,
                        location: Location::new(1, 11),
                    }),
                    operator: "=",
                    right: Box::new(Expression::Infix {
                        left: Box::new(Expression::Infix {
                            left: Box::new(Expression::Prefix {
                                operator: "++",
                                expression: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::new(1, 15),
                                }),
                                location: Location::new(1, 13),
                            }),
                            operator: "+",
                            right: Box::new(Expression::Prefix {
                                operator: "--",
                                expression: Box::new(Expression::IntConst {
                                    value: 2,
                                    location: Location::new(1, 19),
                                }),
                                location: Location::new(1, 17),
                            }),
                        }),
                        operator: "-",
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::Infix {
                                left: Box::new(Expression::Infix {
                                    left: Box::new(Expression::Suffix {
                                        operator: "++",
                                        expression: Box::new(Expression::IntConst {
                                            value: 3,
                                            location: Location::new(1, 21),
                                        }),
                                    }),
                                    operator: "*",
                                    right: Box::new(Expression::Suffix {
                                        operator: "--",
                                        expression: Box::new(Expression::IntConst {
                                            value: 4,
                                            location: Location::new(1, 25),
                                        }),
                                    }),
                                }),
                                operator: "/",
                                right: Box::new(Expression::Infix {
                                    left: Box::new(Expression::IntConst {
                                        value: 5,
                                        location: Location::new(1, 30),
                                    }),
                                    operator: "%",
                                    right: Box::new(Expression::IntConst {
                                        value: 6,
                                        location: Location::new(1, 32),
                                    }),
                                }),
                            }),
                            operator: "%",
                            right: Box::new(Expression::IntConst {
                                value: 7,
                                location: Location::new(1, 35),
                            }),
                        }),
                    }),
                }))],
                location: Location::new(1, 9),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn expression_relationship() {
        let source = "int f() { 0==1!=2<3>4<=5>=6; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::Expr(Expression::Infix {
                    left: Box::new(Expression::Infix {
                        left: Box::new(Expression::IntConst {
                            value: 0,
                            location: Location::new(1, 11),
                        }),
                        operator: "==",
                        right: Box::new(Expression::IntConst {
                            value: 1,
                            location: Location::new(1, 14),
                        }),
                    }),
                    operator: "!=",
                    right: Box::new(Expression::Infix {
                        left: Box::new(Expression::Infix {
                            left: Box::new(Expression::Infix {
                                left: Box::new(Expression::Infix {
                                    left: Box::new(Expression::IntConst {
                                        value: 2,
                                        location: Location::new(1, 17),
                                    }),
                                    operator: "<",
                                    right: Box::new(Expression::IntConst {
                                        value: 3,
                                        location: Location::new(1, 19),
                                    }),
                                }),
                                operator: ">",
                                right: Box::new(Expression::IntConst {
                                    value: 4,
                                    location: Location::new(1, 21),
                                }),
                            }),
                            operator: "<=",
                            right: Box::new(Expression::IntConst {
                                value: 5,
                                location: Location::new(1, 24),
                            }),
                        }),
                        operator: ">=",
                        right: Box::new(Expression::IntConst {
                            value: 6,
                            location: Location::new(1, 27),
                        }),
                    }),
                }))],
                location: Location::new(1, 9),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn expression_assignment_bool() {
        let source = "int f() { !0||+1&&-2+=3-=4*=5/=6%=7; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::Expr(Expression::Infix {
                    left: Box::new(Expression::Infix {
                        left: Box::new(Expression::Prefix {
                            operator: "!",
                            expression: Box::new(Expression::IntConst {
                                value: 0,
                                location: Location::new(1, 12),
                            }),
                            location: Location::new(1, 11),
                        }),
                        operator: "||",
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::Prefix {
                                operator: "+",
                                expression: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::new(1, 16),
                                }),
                                location: Location::new(1, 15),
                            }),
                            operator: "&&",
                            right: Box::new(Expression::Prefix {
                                operator: "-",
                                expression: Box::new(Expression::IntConst {
                                    value: 2,
                                    location: Location::new(1, 20),
                                }),
                                location: Location::new(1, 19),
                            }),
                        }),
                    }),
                    operator: "+=",
                    right: Box::new(Expression::Infix {
                        left: Box::new(Expression::IntConst {
                            value: 3,
                            location: Location::new(1, 23),
                        }),
                        operator: "-=",
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::IntConst {
                                value: 4,
                                location: Location::new(1, 26),
                            }),
                            operator: "*=",
                            right: Box::new(Expression::Infix {
                                left: Box::new(Expression::IntConst {
                                    value: 5,
                                    location: Location::new(1, 29),
                                }),
                                operator: "/=",
                                right: Box::new(Expression::Infix {
                                    left: Box::new(Expression::IntConst {
                                        value: 6,
                                        location: Location::new(1, 32),
                                    }),
                                    operator: "%=",
                                    right: Box::new(Expression::IntConst {
                                        value: 7,
                                        location: Location::new(1, 35),
                                    }),
                                }),
                            }),
                        }),
                    }),
                }))],
                location: Location::new(1, 9),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn dummy_type() {
        let source = "f(a) { a = 1; b = 2;}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::T {
                dummy_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(0, 0),
            },
            name: "f",
            parameters: [(
                "a",
                Type::T {
                    dummy_flag: true,
                    array_flag: false,
                    array_len: None,
                    location: Location::new(0, 0),
                },
            )]
            .iter()
            .cloned()
            .collect(),
            body: Statement::Block {
                statements: vec![
                    Box::new(Statement::Expr(Expression::Infix {
                        left: Box::new(Expression::Ident {
                            value: "a",
                            location: Location::new(1, 8),
                        }),
                        operator: "=",
                        right: Box::new(Expression::IntConst {
                            value: 1,
                            location: Location::new(1, 12),
                        }),
                    })),
                    Box::new(Statement::Def {
                        declarators: vec![(
                            Type::T {
                                dummy_flag: true,
                                array_flag: false,
                                array_len: None,
                                location: Location::new(0, 0),
                            },
                            "b",
                            Some(Expression::IntConst {
                                value: 2,
                                location: Location::new(1, 19),
                            }),
                        )],
                        location: Location::new(1, 15),
                    }),
                ],
                location: Location::new(1, 6),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn index_call() {
        let source = "int f() { a[0]+b(1); }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![Function {
            r#type: Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                location: Location::new(1, 1),
            },
            name: "f",
            parameters: IndexMap::new(),
            body: Statement::Block {
                statements: vec![Box::new(Statement::Expr(Expression::Infix {
                    left: Box::new(Expression::Index {
                        expression: Box::new(Expression::Ident {
                            value: "a",
                            location: Location::new(1, 11),
                        }),
                        index: Box::new(Expression::IntConst {
                            value: 0,
                            location: Location::new(1, 13),
                        }),
                    }),
                    operator: "+",
                    right: Box::new(Expression::Call {
                        expression: Box::new(Expression::Ident {
                            value: "b",
                            location: Location::new(1, 16),
                        }),
                        arguments: vec![Box::new(Expression::IntConst {
                            value: 1,
                            location: Location::new(1, 18),
                        })],
                    }),
                }))],
                location: Location::new(1, 9),
            },
            location: Location::new(1, 1),
        }];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }
}
