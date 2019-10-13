use std::collections::HashSet;

use crate::structure::{ Location, Locate, Error, Token, Type, Expression, Statement, Function };

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
        for index in (0..self.envs.len()).rev() {
            match self.envs[index].contains(name) {
                true => return true,
                false => {},
            }
        }
        false
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
                    Err(()) => self.tokens.clear(),
                },
                None => break,
            }
        }
        (self.generic_ast.take().unwrap(), self.errors.take().unwrap())
    }

    fn parse_types(&mut self) -> Vec<Type> {
        let mut types = Vec::new();
        loop {
            match self.tokens.pop() {
                Some(Token::T(loc)) => types.push(Type::T(loc)),
                Some(Token::Void(loc)) => types.push(Type::Void(loc)),
                Some(Token::Char(loc)) => types.push(Type::Char(loc)),
                Some(Token::Short(loc)) => types.push(Type::Short(loc)),
                Some(Token::Int(loc)) => types.push(Type::Int(loc)),
                Some(Token::Long(loc)) => types.push(Type::Long(loc)),
                Some(Token::Float(loc)) => types.push(Type::Float(loc)),
                Some(Token::Double(loc)) => types.push(Type::Double(loc)),
                Some(Token::Signed(loc)) => types.push(Type::Signed(loc)),
                Some(Token::Unsigned(loc)) => types.push(Type::Unsigned(loc)),
                Some(tk) => { self.tokens.push(tk); break },
                None => break,
            }
        }
        if types.len() == 0 {
            types.push(Type::T(Location::empty()));
        }
        types
    }

    fn assert_token(&mut self, name: &str) -> Result<(), ()> {
        match self.tokens.pop() {
            Some(tk) => {
                if format!("{:?}", tk).starts_with(name) {
                    Ok(())
                } else {
                    self.push_error(&format!("Expect {}, get `{:?}`.", name, tk), Some(&tk));
                    self.tokens.push(tk);
                    Err(())
                }
            },
            None => {
                self.push_error("Expect `;`, get EOF.", None);
                Err(())
            },
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
            Some(Token::EqualTo(_)) => 9,
            Some(Token::NotEqualTo(_)) => 9,
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
        self.errors.as_mut().unwrap().push(
            Error::Parsing {
                message: message.to_string(),
                location,
            }
        );
    }

    fn parse_function(&mut self) -> Result<Function<'a>, ()> {
        self.environment.enter();
        let location = match self.tokens.last() {
            Some(tk) => tk.locate(),
            None => Location::empty(),
        };
        let types = self.parse_types();
        let name = match self.tokens.pop() {
            Some(Token::Ident { literal, location }) => {
                Expression::Ident {
                    value: literal,
                    location: location.clone(),
                }
            },
            Some(tk) => {
                self.push_error(&format!("Expect function name, get `{:?}`.", tk), Some(&tk));
                return Err(());
            },
            None => {
                self.push_error("Expect function name, get EOF.", None);
                return Err(());
            },
        };
        match self.tokens.pop() {
            Some(Token::LParen(_)) => {},
            Some(tk) => {
                self.push_error(&format!("Expect `(`, get `{:?}`.", tk), Some(&tk));
                return Err(());
            },
            None => {
                self.push_error("Expect `(`, get EOF.", None);
                return Err(());
            },
        }
        let mut parameters = Vec::new();
        match self.tokens.last() {
            Some(Token::RParen(_)) => { self.tokens.pop(); },
            _ => {
                loop {
                    let types = self.parse_types();
                    let name = match self.tokens.pop() {
                        Some(Token::Ident { literal, location }) => {
                            self.environment.define(literal);
                            Expression::Ident {
                                value: literal,
                                location,
                            }
                        },
                        Some(tk) => {
                            self.push_error(&format!("Expect parameter, get `{:?}.`", tk), Some(&tk));
                            return Err(());
                        },
                        None => {
                            self.push_error("Expect parameter, get EOF.", None);
                            return Err(());
                        },
                    };
                    parameters.push((types, name));
                    match self.tokens.pop() {
                        Some(Token::Comma(_)) => {},
                        Some(Token::RParen(_)) => { break },
                        Some(tk) => {
                            self.push_error(&format!("Expect `,` or `)`, get `{:?}`.", tk), Some(&tk));
                            return Err(());
                        },
                        None => {
                            self.push_error("Expect `,` or `)`, get EOF.", None);
                            return Err(());
                        },
                    }
                }
            },
        }
        let body = self.parse_statement()?;
        self.environment.leave();
        Ok(Function { types, name, parameters, body, location })
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

            Some(type_ @ Token::T(_))
            | Some(type_ @ Token::Void(_))
            | Some(type_ @ Token::Char(_))
            | Some(type_ @ Token::Short(_))
            | Some(type_ @ Token::Int(_))
            | Some(type_ @ Token::Long(_))
            | Some(type_ @ Token::Float(_))
            | Some(type_ @ Token::Double(_))
            | Some(type_ @ Token::Signed(_))
            | Some(type_ @ Token::Unsigned(_)) => self.parse_statement_def(type_),

            Some(tk) => {
                self.tokens.push(tk);
                self.parse_statement_expr()
            },

            None => {
                self.push_error("Expect statement, get EOF.", None);
                Err(())
            },
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
            return Ok(Statement::Return { expr: None, location });
        }
        match self.parse_expression(0) {
            Err(_) => {
                match self.assert_token("Semicolon") {
                    _ => Err(()),
                }
            },
            Ok(expr) => {
                match self.assert_token("Semicolon") {
                    Ok(_) => Ok(Statement::Return { expr: Some(expr), location }),
                    Err(_) => Err(()),
                }
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
                    break
                },
                Some(_) => {
                    match self.parse_statement() {
                        Ok(stmt) => statements.push(Box::new(stmt)),
                        Err(_) => {},
                    }
                },
                None => {
                    self.push_error("Expect `}`, get EOF.", None);
                    return Err(());
                },
            }
        }
        self.environment.leave();
        Ok(Statement::Block { statements, location })
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
            _ => { Some(self.parse_expression(0)?) }
        };
        self.assert_token("Semicolon")?;
        let condition = match self.tokens.last() {
            Some(Token::Semicolon(_)) => None,
            _ => { Some(self.parse_expression(0)?) }
        };
        self.assert_token("Semicolon")?;
        let increment = match self.tokens.last() {
            Some(Token::RParen(_)) => None,
            _ => { Some(self.parse_expression(0)?) }
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
            },
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
                },
                Some(Token::Default(_)) => {
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
                },
                Some(Token::RBrace(_)) => break,
                Some(tk) => {
                    self.push_error(&format!("Expect `case` or `default`, get `{:?}`.", tk), Some(&tk));
                    self.tokens.push(tk);
                    return Err(());
                },
                None => {
                    self.push_error("Expect `case` or `default`, get EOF.", None);
                    return Err(());
                },
            }
        }
        Ok(Statement::Switch {
            expression,
            branches,
            default,
            location,
        })
    }

    fn parse_statement_def(&mut self, type_: Token<'a>) -> Result<Statement<'a>, ()> {
        let location = type_.locate();
        self.tokens.push(type_);
        let types = self.parse_types();
        let mut declarators = Vec::new();
        match self.tokens.last() {
            Some(Token::Semicolon(_)) => { self.tokens.pop(); },
            _ => {
                loop {
                    match self.tokens.pop() {
                        Some(Token::Ident { literal, location }) =>  {
                            self.environment.define(literal);
                            let ident = Expression::Ident { value: literal, location };
                            if let Some(Token::Equal(_)) = self.tokens.last() {
                                self.tokens.pop();
                                let value = self.parse_expression(0)?;
                                declarators.push((ident, Some(value)));
                            } else {
                                declarators.push((ident, None));
                            }
                            match self.tokens.pop() {
                                Some(Token::Comma(_)) => {},
                                Some(Token::Semicolon(_)) => break,
                                Some(tk) => self.tokens.push(tk),
                                None => {},
                            }
                        },
                        Some(tk) => {
                            self.push_error("Expect Ident, get {:?}.", Some(&tk));
                            self.tokens.push(tk);
                            return Err(());
                        },
                        None => {
                            self.push_error("Expect Ident, get EOF.", None);
                            return Err(());
                        },
                    }    
                }
            },
        }
        Ok(Statement::Def {
            types,
            declarators,
            location,
        })
    }

    fn parse_statement_expr(&mut self) -> Result<Statement<'a>, ()> {
        let expr = self.parse_expression(0)?;
        self.assert_token("Semicolon")?;
        match expr {
            Expression::Infix { left, operator: "=", right } => {
                match *left {
                    Expression::Ident { value, location } => {
                        if !self.environment.is_defined(value) {
                            self.environment.define(value);
                            Ok(Statement::Def {
                                types: vec![Type::T(Location::empty())],
                                declarators: vec![(
                                    Expression::Ident { value, location: location.clone() },
                                    Some(*right),
                                )],
                                location,
                            })
                        } else {
                            Ok(Statement::Expr(
                                Expression::Infix {
                                    left: Box::new(Expression::Ident { value, location }),
                                    operator: "=",
                                    right,
                                }
                            ))
                        }
                    },
                    _ => {
                        Ok(Statement::Expr(
                            Expression::Infix { left, operator: "=", right }
                        ))
                    },
                }
            },
            _ => Ok(Statement::Expr(expr)),
        }
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
            Some(Token::Ident { literal, location }) => {
                Ok(Expression::Ident { value: literal, location })
            },
            Some(Token::IntConst { literal, location }) => {
                Ok(Expression::IntConst { value: literal.parse().unwrap(), location })
            },
            Some(Token::FloatingConst { literal, location }) => {
                Ok(Expression::FloatingConst { value: literal.parse().unwrap(), location })
            },
            Some(Token::CharConst { literal, location }) => {
                Ok(Expression::CharConst { value: literal, location })
            },
            Some(Token::StrConst { literal, location }) => {
                Ok(Expression::StrConst { value: literal, location })
            },
            Some(Token::Not(location)) => {
                Ok(Expression::Prefix {
                    operator: "!",
                    expression: Box::new(self.parse_expression(precedence)?),
                    location,
                })
            },
            Some(Token::Plus(location)) => {
                Ok(Expression::Prefix {
                    operator: "+",
                    expression: Box::new(self.parse_expression(precedence)?),
                    location,
                })
            },
            Some(Token::Minus(location)) => {
                Ok(Expression::Prefix {
                    operator: "-",
                    expression: Box::new(self.parse_expression(precedence)?),
                    location,
                })
            },
            Some(Token::BiPlus(location)) => {
                Ok(Expression::Prefix {
                    operator: "++",
                    expression: Box::new(self.parse_expression(precedence)?),
                    location,
                })
            },
            Some(Token::BiMinus(location)) => {
                Ok(Expression::Prefix {
                    operator: "--",
                    expression: Box::new(self.parse_expression(precedence)?),
                    location,
                })
            },
            Some(Token::LParen(_)) => {
                let expr = self.parse_expression(0);
                self.assert_token("RParen")?;
                expr
            },
            Some(tk) => {
                self.push_error(&format!("Expect prefix operator, get `{:?}`.", tk), Some(&tk));
                return Err(());
            },
            None => {
                self.push_error("Expect prefix operator`, get EOF.", None);
                return Err(());
            },
        }
    }

    fn parse_infix(&mut self, precedence: u8, left: Expression<'a>) -> Result<Expression<'a>, ()> {
        let op = self.tokens.pop();
        let expr = match op {
            Some(Token::Equal(_)) => {
                let right = self.parse_expression(precedence-1)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "=",
                    right: Box::new(right),
                }
            },
            Some(Token::PlusEq(_)) => {
                let right = self.parse_expression(precedence-1)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "+=",
                    right: Box::new(right),
                }
            },
            Some(Token::MinusEq(_)) => {
                let right = self.parse_expression(precedence-1)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "-=",
                    right: Box::new(right),
                }
            },
            Some(Token::AsteriskEq(_)) => {
                let right = self.parse_expression(precedence-1)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "*=",
                    right: Box::new(right),
                }
            },
            Some(Token::SlashEq(_)) => {
                let right = self.parse_expression(precedence-1)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "/=",
                    right: Box::new(right),
                }
            },
            Some(Token::PercentEq(_)) => {
                let right = self.parse_expression(precedence-1)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "%=",
                    right: Box::new(right),
                }
            },
            Some(Token::Or(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "||",
                    right: Box::new(right),
                }
            },
            Some(Token::And(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "&&",
                    right: Box::new(right),
                }
            },
            Some(Token::Small(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "<",
                    right: Box::new(right),
                }
            },
            Some(Token::Large(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: ">",
                    right: Box::new(right),
                }
            },
            Some(Token::SmallEq(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "<=",
                    right: Box::new(right),
                }
            },
            Some(Token::LargeEq(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: ">=",
                    right: Box::new(right),
                }
            },
            Some(Token::EqualTo(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "==",
                    right: Box::new(right),
                }
            },
            Some(Token::NotEqualTo(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "!=",
                    right: Box::new(right),
                }
            },
            Some(Token::Plus(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "+",
                    right: Box::new(right),
                }
            },
            Some(Token::Minus(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "-",
                    right: Box::new(right),
                }
            },
            Some(Token::Asterisk(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "*",
                    right: Box::new(right),
                }
            },
            Some(Token::Slash(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "/",
                    right: Box::new(right),
                }
            },
            Some(Token::Percent(_)) => {
                let right = self.parse_expression(precedence)?;
                Expression::Infix {
                    left: Box::new(left),
                    operator: "%",
                    right: Box::new(right),
                }
            },
            Some(Token::BiPlus(_)) => {
                Expression::Suffix {
                    operator: "++",
                    expression: Box::new(left),
                }
            },
            Some(Token::BiMinus(_)) => {
                Expression::Suffix {
                    operator: "--",
                    expression: Box::new(left),
                }
            },
            Some(Token::LBracket(_)) => {
                let index = self.parse_expression(0)?;
                self.assert_token("RBracket")?;
                Expression::Index {
                    name: Box::new(left),
                    index: Box::new(index),
                }
            },
            Some(Token::LParen(_)) => {
                let mut arguments = Vec::new();
                match self.tokens.last() {
                    Some(Token::RParen(_)) => { self.tokens.pop(); },
                    _ => {
                        loop {
                            let arg = self.parse_expression(0)?;
                            arguments.push(Box::new(arg));
                            match self.tokens.pop() {
                                Some(Token::Comma(_)) => {},
                                Some(Token::RParen(_)) => { break },
                                Some(tk) => {
                                    self.push_error(&format!("Expect `,` or `)`, get `{:?}`.", tk), Some(&tk));
                                    return Err(());
                                },
                                None => {
                                    self.push_error("Expect `,` or `)`, get EOF.", None);
                                    return Err(());
                                },
                            }
                        }
                    },
                }
                Expression::Call {
                    name: Box::new(left),
                    arguments,
                }
            },
            Some(tk) => {
                self.push_error(&format!("Expect infix operator, get `{:?}`.", tk), Some(&tk));
                return Err(());
            },
            None => {
                self.push_error("Expect infix operator`, get EOF.", None);
                return Err(());
            },
        };
        return Ok(expr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    
    #[test]
    fn function_empty() {
        let source = "int f() {}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn function_single() {
        let source = "int f(int a) { continue; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![
                    (
                        vec![
                            Type::Int(Location::new(1, 7)),
                        ],
                        Expression::Ident {
                            value: "a",
                            location: Location::new(1, 11),
                        },
                    ),
                ],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Continue(Location::new(1, 16))),
                    ],
                    location: Location::new(1, 14),
                },
                location: Location::new(1, 1),
            },
        ];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn function_multiple() {
        let source = "int f() {}
T void char short int long float double signed unsigned f(int a, int b) { continue; break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
            Function {
                types: vec![
                    Type::T(Location::new(2, 1)),
                    Type::Void(Location::new(2, 3)),
                    Type::Char(Location::new(2, 8)),
                    Type::Short(Location::new(2, 13)),
                    Type::Int(Location::new(2, 19)),
                    Type::Long(Location::new(2, 23)),
                    Type::Float(Location::new(2, 28)),
                    Type::Double(Location::new(2, 34)),
                    Type::Signed(Location::new(2, 41)),
                    Type::Unsigned(Location::new(2, 48)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(2, 57),
                },
                parameters: vec![
                    (
                        vec![
                            Type::Int(Location::new(2, 59)),
                        ],
                        Expression::Ident {
                            value: "a",
                            location: Location::new(2, 63),
                        },
                    ),
                    (
                        vec![
                            Type::Int(Location::new(2, 66)),
                        ],
                        Expression::Ident {
                            value: "b",
                            location: Location::new(2, 70),
                        },
                    ),
                ],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Continue(Location::new(2, 75))),
                        Box::new(Statement::Break(Location::new(2, 85))),
                    ],
                    location: Location::new(2, 73),
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
        let source = "int f() { return;}
int f() { return 1;}";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Return {
                            expr: None,
                            location: Location::new(1, 11),
                        }),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
            Function {
                types: vec![
                    Type::Int(Location::new(2, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(2, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Return {
                            expr: Some(Expression::IntConst {
                                value: 1,
                                location: Location::new(2, 18),
                            }),
                            location: Location::new(2, 11),
                        }),
                    ],
                    location: Location::new(2, 9),
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
        let source = "int f() { int a = 1, b; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Def {
                            types: vec![
                                Type::Int(Location::new(1, 11)),
                            ],
                            declarators: vec![
                                (
                                    Expression::Ident {
                                        value: "a",
                                        location: Location::new(1, 15),
                                    },
                                    Some(Expression::IntConst {
                                        value: 1,
                                        location: Location::new(1, 19),
                                    }),
                                ),
                                (
                                    Expression::Ident {
                                        value: "b",
                                        location: Location::new(1, 22),
                                    },
                                    None,
                                ),
                            ],
                            location: Location::new(1, 11),
                        }),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_while() {
        let source = "int f() { while (1) break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::While {
                            condition: Expression::IntConst {
                                value: 1,
                                location: Location::new(1, 18),
                            },
                            body: Box::new(Statement::Break(Location::new(1, 21))),
                            location: Location::new(1, 11),
                        }),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_do() {
        let source = "int f() { do break; while (1); }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Do {
                            condition: Expression::IntConst {
                                value: 1,
                                location: Location::new(1, 28),
                            },
                            body: Box::new(Statement::Break(Location::new(1, 14))),
                            location: Location::new(1, 11),
                        }),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_for() {
        let source = "int f() { for (; 1; ) break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::For {
                            initialization: None,
                            condition: Some(Expression::IntConst {
                                value: 1,
                                location: Location::new(1, 18),
                            }),
                            increment: None,
                            body: Box::new(Statement::Break(Location::new(1, 23))),
                            location: Location::new(1, 11),
                        }),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }

    #[test]
    fn statement_if() {
        let source = "int f() { if (1) break; }";
        let expected_errors = vec![];
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::If {
                            condition: Expression::IntConst {
                                value: 1,
                                location: Location::new(1, 15),
                            },
                            body: Box::new(Statement::Break(Location::new(1, 18))),
                            alternative: None,
                            location: Location::new(1, 11),
                        }),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

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
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::If {
                            condition: Expression::IntConst {
                                value: 1,
                                location: Location::new(1, 15),
                            },
                            body: Box::new(Statement::Break(Location::new(1, 18))),
                            alternative: Some(Box::new(Statement::Continue(Location::new(1, 30)))),
                            location: Location::new(1, 11),
                        }),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

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
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Switch {
                            expression: Expression::IntConst {
                                value: 1,
                                location: Location::new(1, 19),
                            },
                            branches: vec![
                                (
                                    Expression::IntConst {
                                        value: 1,
                                        location: Location::new(1, 28),
                                    },
                                    vec![
                                        Box::new(Statement::Break(Location::new(1, 31))),
                                    ],
                                )
                            ],
                            default: None,
                            location: Location::new(1, 11),
                        }),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

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
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Switch {
                            expression: Expression::IntConst {
                                value: 1,
                                location: Location::new(1, 19),
                            },
                            branches: vec![
                                (
                                    Expression::IntConst {
                                        value: 1,
                                        location: Location::new(1, 28),
                                    },
                                    vec![
                                        Box::new(Statement::Break(Location::new(1, 31))),
                                    ],
                                )
                            ],
                            default: Some(vec![
                                Box::new(Statement::Continue(Location::new(1, 47))),
                            ]),
                            location: Location::new(1, 11),
                        }),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

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
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Expr(
                            Expression::Ident {
                                value: "a",
                                location: Location::new(1, 11),
                            }
                        )),
                        Box::new(Statement::Expr(
                            Expression::IntConst {
                                value: 1,
                                location: Location::new(1, 14),
                            }
                        )),
                        Box::new(Statement::Expr(
                            Expression::FloatingConst {
                                value: 1.0,
                                location: Location::new(1, 17),
                            }
                        )),
                        Box::new(Statement::Expr(
                            Expression::CharConst {
                                value: "b",
                                location: Location::new(1, 22),
                            }
                        )),
                        Box::new(Statement::Expr(
                            Expression::StrConst {
                                value: "c",
                                location: Location::new(1, 27),
                            }
                        )),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

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
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Expr(
                            Expression::Infix {
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
                            },
                        )),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

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
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Expr(
                            Expression::Infix {
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
                            },
                        )),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

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
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Expr(
                            Expression::Infix {
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
                            },
                        )),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

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
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::T(Location::empty()),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 1),
                },
                parameters: vec![
                    (
                        vec![
                            Type::T(Location::empty()),
                        ],
                        Expression::Ident {
                            value: "a",
                            location: Location::new(1, 3),
                        },
                    ),
                ],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Expr(
                            Expression::Infix {
                                left: Box::new(Expression::Ident {
                                    value: "a",
                                    location: Location::new(1, 8),
                                }),
                                operator: "=",
                                right: Box::new(Expression::IntConst {
                                    value: 1,
                                    location: Location::new(1, 12),
                                }),
                            },
                        )),
                        Box::new(Statement::Def {
                            types: vec![
                                Type::T(
                                    Location::empty(),
                                ),
                            ],
                            declarators: vec![
                                (
                                    Expression::Ident {
                                        value: "b",
                                        location: Location::new(1, 15),
                                    },
                                    Some(
                                        Expression::IntConst {
                                            value: 2,
                                            location: Location::new(1, 19),
                                        },
                                    ),
                                ),
                            ],
                            location: Location::new(1, 15),
                        }),
                    ],
                    location: Location::new(1, 6),
                },
                location: Location::new(1, 1),
            },
        ];

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
        let expected_generic_ast = vec![
            Function {
                types: vec![
                    Type::Int(Location::new(1, 1)),
                ],
                name: Expression::Ident {
                    value: "f",
                    location: Location::new(1, 5),
                },
                parameters: vec![],
                body: Statement::Block {
                    statements: vec![
                        Box::new(Statement::Expr(
                            Expression::Infix {
                                left: Box::new(Expression::Index {
                                    name: Box::new(Expression::Ident {
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
                                    name: Box::new(Expression::Ident {
                                        value: "b",
                                        location: Location::new(1, 16),
                                    }),
                                    arguments: vec![
                                        Box::new(Expression::IntConst {
                                            value: 1,
                                            location: Location::new(1, 18),
                                        }),
                                    ],
                                }),
                            },
                        )),
                    ],
                    location: Location::new(1, 9),
                },
                location: Location::new(1, 1),
            },
        ];

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(generic_ast, expected_generic_ast);
    }
}
