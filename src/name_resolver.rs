use std::collections::HashSet;

use super::branched_parser::Expression;
use super::branched_parser::Statement;
use super::branched_parser::Branch;

pub struct Environment {
    env: HashSet<String>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            env: HashSet::new(),
            outer: None,
        }
    }

    // pub fn init(outer: Environment) -> Environment {
    //     Environment {
    //         env: HashSet::new(),
    //         outer: Some(Box::new(outer)),
    //     }
    // }

    pub fn set(&mut self, name: &str) {
        self.env.insert(String::from(name));
    }

    pub fn defines(&self, name: &str) -> bool {
        match self.env.contains(name) {
            true => true,
            false => match &self.outer {
                Some(e) => e.defines(name),
                None => false,
            },
        }
    }
}

pub struct NameResolver {}

impl NameResolver {
    pub fn new() -> NameResolver {
        NameResolver {}
    }

    pub fn run<'a>(&self, branched_ast: Vec<Branch<'a>>) -> Vec<Statement<'a>> {
        let mut env = Environment::new();
        let mut generic_ast = Vec::new();
        for branch in branched_ast.into_iter() {
            generic_ast.push(self.resolve_branch(branch, &mut env));
        }
        generic_ast
    }

    fn resolve_branch<'a>(&self, branch: Branch<'a>, env: &mut Environment) -> Statement<'a> {
        match branch {
            Branch::Single { stmt } => {
                self.resolve_stmt(&stmt, env).unwrap();
                stmt
            },
            Branch::Double { original, modified } => {
                match self.resolve_stmt(&original, env) {
                    Ok(_) => original,
                    Err(_) => modified,
                }
            },
        }
    }

    fn resolve_stmt<'a>(&self, stmt: &Statement<'a>, env: &mut Environment) -> Result<(), ()> {
        match stmt {
            Statement::Expr { expr } => self.resolve_expr(&expr, env),
            Statement::Def { type_: _, ident, value } => {
                match self.resolve_expr(&value, env) {
                    Ok(_) => {
                        let value = match ident {
                            Expression::Ident { value, line_no: _, char_no: _ } => value,
                            _ => panic!("Impossible"),
                        };
                        env.set(value);
                        Ok(())
                    },
                    e => e,
                }
            },
        }
    }

    fn resolve_expr<'a>(&self, expr: &Expression<'a>, env: &mut Environment) -> Result<(), ()> {
        match expr {
            Expression::Assign { ident, value } => {
                self.resolve_expr(&value, env).unwrap();
                match **ident {
                    Expression::Ident { value, line_no: _, char_no: _ } => {
                        if env.defines(value) {
                            Ok(())
                        } else {
                            Err(())
                        }
                    },
                    _ => panic!("Impossible"),
                }
            },
            _ => Ok(()),
        }
    }
}
