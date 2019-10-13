use crate::structure::{ Location, Locate, Error, Token, Type, Expression, Statement, Function };

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    errors: Option<Vec<Error>>,
    generic_ast: Option<Vec<Function<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn new(mut tokens: Vec<Token<'a>>, errors: Vec<Error>) -> Parser<'a> {
        tokens.reverse();
        Parser {
            tokens,
            errors: Some(errors),
            generic_ast: Some(Vec::new()),
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
                    self.push_error(&format!("Expect `;`, get `{:?}`.", tk), Some(&tk));
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
            Some(Token::Plus(_)) => 15,
            Some(Token::Minus(_)) => 15,
            Some(Token::BiPlus(_)) => 15,
            Some(Token::BiMinus(_)) => 15,
            _ => 0,
        }
    }

    fn get_infix_precedence(&self) -> u8 {
        match self.tokens.last() {
            Some(Token::Plus(_)) => 12,
            Some(Token::Minus(_)) => 12,
            Some(Token::Asterisk(_)) => 13,
            Some(Token::Slash(_)) => 13,
            Some(Token::Percent(_)) => 13,
            // Some(Token::BiPlus(_)) => 16,
            // Some(Token::BiMinus(_)) => 16,
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
        match self.parse_expression(self.get_infix_precedence()) {
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
        let mut statements = Vec::new();
        loop {          
            match self.tokens.last() {
                Some(Token::RBrace(_)) => {
                    self.tokens.pop();
                    break
                },
                Some(tk) => {
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
        Ok(Statement::Block { statements, location })
    }

    fn parse_statement_while(&mut self, location: Location) -> Result<Statement<'a>, ()> {
        self.assert_token("LParen")?;
        let condition = self.parse_expression(self.get_infix_precedence())?;
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
        let condition = self.parse_expression(self.get_infix_precedence())?;
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
            Some(Token::Semicolon(loc)) => None,
            _ => { Some(self.parse_expression(self.get_infix_precedence())?) }
        };
        let condition = match self.tokens.last() {
            Some(Token::Semicolon(loc)) => None,
            _ => { Some(self.parse_expression(self.get_infix_precedence())?) }
        };
        self.assert_token("Semicolon")?;
        let increment = match self.tokens.last() {
            Some(Token::Semicolon(loc)) => None,
            _ => { Some(self.parse_expression(self.get_infix_precedence())?) }
        };
        self.assert_token("Semicolon")?;
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
        let condition = self.parse_expression(self.get_infix_precedence())?;
        self.assert_token("RParen")?;
        let body = self.parse_statement()?;
        let alternative = match self.tokens.last() {
            Some(Token::Else(loc)) => {
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
        let expression = self.parse_expression(self.get_infix_precedence())?;
        self.assert_token("RParen")?;
        self.assert_token("LBrace")?;
        let mut default = None;
        let mut branches = Vec::new();
        loop {
            match self.tokens.pop() {
                Some(Token::Case(loc)) => {
                    let expr = self.parse_expression(self.get_infix_precedence())?;
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
                Some(Token::Default(loc)) => {
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
                Some(Token::RBrace(loc)) => break,
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
                            let ident = Expression::Ident { value: literal, location };
                            if let Some(Token::Equal(_)) = self.tokens.last() {
                                self.tokens.pop();
                                let value = self.parse_expression(self.get_infix_precedence())?;
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
        let expr = self.parse_expression(self.get_infix_precedence())?;
        self.assert_token("Semicolon")?;
        Ok(Statement::Expr(expr))
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<Expression<'a>, ()> {
        let mut left = self.parse_prefix()?;
        println!("{:?} {} {}", left, precedence, self.get_infix_precedence());
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
            _ => panic!("!!!"),
        }
    }

    fn parse_infix(&mut self, precedence: u8, left: Expression<'a>) -> Result<Expression<'a>, ()> {
        let op = self.tokens.pop();
        let expr = match op {
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
            Some(Token::BiPlus(_)) => {
                Expression::Suffix {
                    operator: "--",
                    expression: Box::new(left),
                }
            },
            _ => panic!("@@@"),
        };
        return Ok(expr);
    }
}



    

//     pub struct Environment {
//         env: HashSet<String>,
//         outer: Option<Box<Environment>>,
//     }

//     impl Environment {
//         pub fn new() -> Environment {
//             Environment {
//                 env: HashSet::new(),
//                 outer: None,
//             }
//         }

//         // pub fn init(outer: Environment) -> Environment {
//         //     Environment {
//         //         env: HashSet::new(),
//         //         outer: Some(Box::new(outer)),
//         //     }
//         // }

//         pub fn set(&mut self, name: &str) {
//             env.insert(String::from(name));
//         }

//         pub fn defines(&self, name: &str) -> bool {
//             match env.contains(name) {
//                 true => true,
//                 false => match &outer {
//                     Some(e) => e.defines(name),
//                     None => false,
//                 },
//             }
//         }
//     }
// }


// pub struct NameResolver {}

// impl NameResolver {
//     pub fn new() -> NameResolver {
//         NameResolver {}
//     }

//     pub fn run<'a>(&self, branched_ast: Vec<Branch<'a>>) -> Vec<Statement<'a>> {
//         let mut env = Environment::new();
//         let mut generic_ast = Vec::new();
//         for branch in branched_ast.into_iter() {
//             generic_ast.push(resolve_branch(branch, &mut env));
//         }
//         generic_ast
//     }

//     fn resolve_branch<'a>(&self, branch: Branch<'a>, env: &mut Environment) -> Statement<'a> {
//         match branch {
//             Branch::Single { stmt } => {
//                 resolve_stmt(&stmt, env).unwrap();
//                 stmt
//             },
//             Branch::Double { original, modified } => {
//                 match resolve_stmt(&original, env) {
//                     Ok(_) => original,
//                     Err(_) => modified,
//                 }
//             },
//         }
//     }

//     fn resolve_stmt<'a>(&self, stmt: &Statement<'a>, env: &mut Environment) -> Result<(), ()> {
//         match stmt {
//             Statement::Expr { expr } => resolve_expr(&expr, env),
//             Statement::Def { type_: _, ident, value } => {
//                 match resolve_expr(&value, env) {
//                     Ok(_) => {
//                         let value = match ident {
//                             Expression::Ident { value, line_no: _, char_no: _ } => value,
//                             _ => panic!("Impossible"),
//                         };
//                         env.set(value);
//                         Ok(())
//                     },
//                     e => e,
//                 }
//             },
//         }
//     }

//     fn resolve_expr<'a>(&self, expr: &Expression<'a>, env: &mut Environment) -> Result<(), ()> {
//         match expr {
//             Expression::Assign { ident, value } => {
//                 resolve_expr(&value, env).unwrap();
//                 match **ident {
//                     Expression::Ident { value, line_no: _, char_no: _ } => {
//                         if env.defines(value) {
//                             Ok(())
//                         } else {
//                             Err(())
//                         }
//                     },
//                     _ => panic!("Impossible"),
//                 }
//             },
//             _ => Ok(()),
//         }
//     }
// }