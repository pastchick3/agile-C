use crate::structure::{ Type, Expression, Statement, Function };

pub struct Serializer<'a> {
    ast: Vec<Function<'a>>,
    transformed_source: Option<String>,
    ident_level: usize,
}

impl<'a> Serializer<'a> {
    pub fn new(ast: Vec<Function<'a>>) -> Serializer {
        Serializer {
            ast,
            transformed_source: Some(String::new()),
            ident_level: 0,
        }
    }

    pub fn run(&mut self) -> String {
        loop {
            match self.ast.pop() {
                Some(func) => self.serialize_function(func),
                None => break,
            }
        }
        self.transformed_source.take().unwrap()
    }
    
    fn push_str(&mut self, s: &str) {
        self.transformed_source.as_mut().unwrap().push_str(s);
    }

    fn push_str_space(&mut self, s: &str) {
        self.push_str(s);
        self.push_str(" ");
    }

    fn push_str_newline(&mut self, s: &str) {
        self.push_str(s);
        self.push_str("\n");
    }

    fn pop_char(&mut self) {
        self.transformed_source.as_mut().unwrap().pop();
    }

    fn serialize_function(&mut self, func: Function<'a>) {
        let Function { type_, name, parameters, body, location: _ } = func;
        self.serialize_type(type_);
        self.push_str_space(name);
        self.pop_char();
        self.push_str("(");
        if !parameters.is_empty() {
            for (param, type_) in parameters.into_iter() {
                self.serialize_type(type_);
                self.push_str(param);
                self.push_str_space(",");
            }
            self.pop_char();
            self.pop_char();
        }
        self.push_str_space(")");
        self.serialize_statement(body);
    }

    fn serialize_type(&mut self, type_: Type) {
        match type_ {
            Type::T(_) => self.push_str_space("T"),
            Type::Void(_) => self.push_str_space("void"),
            Type::Char(_) => self.push_str_space("char"),
            Type::Short(_) => self.push_str_space("short"),
            Type::Int(_) => self.push_str_space("int"),
            Type::Long(_) => self.push_str_space("long"),
            Type::Float(_) => self.push_str_space("float"),
            Type::Double(_) => self.push_str_space("double"),
            Type::Signed(_) => self.push_str_space("signed"),
            Type::Unsigned(_) => self.push_str_space("unsigned"),
        }
    }

    fn serialize_expression(&mut self, expr: Expression<'a>) {
        match expr {
            Expression::Ident { value, location: _ } => self.push_str_space(value),
            Expression::IntConst { value, location: _ } => self.push_str_space(&value.to_string()),
            Expression::FloatingConst { value, location: _ } => self.push_str_space(&value.to_string()),
            Expression::CharConst { value, location: _ } => self.push_str_space(value),
            Expression::StrConst { value, location: _ } => self.push_str_space(value),
            Expression::Prefix { operator, expression, location: _ } => {
                self.push_str(operator);
                self.serialize_expression(*expression);
            },
            Expression::Infix { left, operator, right } => {
                self.serialize_expression(*left);
                self.push_str_space(operator);
                self.serialize_expression(*right);
            },
            Expression::Suffix { operator, expression } => {
                self.serialize_expression(*expression);
                self.pop_char();
                self.push_str_space(operator);
            },
            Expression::Index { name, index } => {
                self.serialize_expression(*name);
                self.pop_char();
                self.push_str("[");
                self.serialize_expression(*index);
                self.pop_char();
                self.push_str_space("]");
            },
            Expression::Call { name, arguments } => {
                self.serialize_expression(*name);
                self.pop_char();
                self.push_str("(");
                if !arguments.is_empty() {
                    for arg in arguments.into_iter() {
                        self.serialize_expression(*arg);
                        self.pop_char();
                        self.push_str_space(",");
                    }
                    self.pop_char();
                    self.pop_char();
                }
                self.push_str_space(")");
            },
        }
    }

    fn serialize_statement(&mut self, stmt: Statement<'a>) {
        if self.transformed_source.as_ref().unwrap().ends_with("\n") {
            self.push_str(&" ".repeat(self.ident_level*4));
        }
        match stmt {
            Statement::Continue(_) => self.push_str_newline("continue;"),
            Statement::Break(_) => self.push_str_newline("break;"),
            Statement::Expr(expr) => {
                self.serialize_expression(expr);
                self.pop_char();
                self.push_str_newline(";");
            },
            Statement::Return { expr, location: _ } => {
                match expr {
                    Some(ex) => {
                        self.push_str_space("return");
                        self.serialize_expression(ex);
                        self.pop_char();
                        self.push_str_newline(";");
                    },
                    None => self.push_str_newline("return;"),
                }
            },
            Statement::Block { statements, location: _ } => {
                self.push_str_newline("{");
                self.ident_level += 1;
                if statements.is_empty() {
                    self.pop_char();
                } else {
                    for st in statements.into_iter() {
                        self.serialize_statement(*st);
                    }
                }
                self.ident_level -= 1;
                self.push_str(&" ".repeat(self.ident_level*4));
                self.push_str_newline("}");
            },
            Statement::Def { type_, declarators, location: _ } => {
                self.serialize_type(type_);
                for (ident, init) in declarators.into_iter() {
                    self.push_str_space(ident);
                    match init {
                        Some(ex) => {
                            self.push_str_space("=");
                            self.serialize_expression(ex);
                        },
                        None => {},
                    }
                    self.pop_char();
                    self.push_str_space(",");
                }
                self.pop_char();
                self.pop_char();
                self.push_str_newline(";")
            },
            Statement::While { condition, body, location: _ } => {
                self.push_str_space("while");
                self.push_str("(");
                self.serialize_expression(condition);
                self.pop_char();
                self.push_str_space(")");
                self.serialize_statement(*body);
            },
            Statement::Do { condition, body, location: _ } => {
                self.push_str_space("do");
                self.serialize_statement(*body);
                self.pop_char();
                self.push_str(" ");
                self.push_str_space("while");
                self.push_str("(");
                self.serialize_expression(condition);
                self.pop_char();
                self.push_str(")");
                self.push_str_newline(";");
            },
            Statement::For { initialization, condition, increment, body, location: _ } => {
                self.push_str_space("for");
                self.push_str("(");
                match initialization {
                    Some(ex) => {
                        self.serialize_expression(ex);
                        self.pop_char();
                    },
                    None => self.push_str_space(""),
                }
                self.push_str_space(";");
                match condition {
                    Some(ex) => {
                        self.serialize_expression(ex);
                        self.pop_char();
                    },
                    None => {},
                }
                self.push_str_space(";");
                match increment {
                    Some(ex) => {
                        self.serialize_expression(ex);
                        self.pop_char();
                    },
                    None => {},
                }
                self.push_str_space(")");
                self.serialize_statement(*body);
            },
            Statement::If { condition, body, alternative, location: _ } => {
                self.push_str_space("if");
                self.push_str("(");
                self.serialize_expression(condition);
                self.pop_char();
                self.push_str_space(")");
                self.serialize_statement(*body);
                match alternative {
                    Some(st) => {
                        self.pop_char();
                        self.push_str_space(" else");
                        self.serialize_statement(*st);
                    },
                    None => {},
                }
            },
            Statement::Switch { expression, branches, default, location: _ } => {
                self.push_str_space("switch");
                self.push_str("(");
                self.serialize_expression(expression);
                self.pop_char();
                self.push_str_space(")");
                self.push_str_newline("{");
                self.ident_level += 1;
                for (label, sts) in branches.into_iter() {
                    self.push_str(&" ".repeat(self.ident_level*4));
                    self.push_str_space("case");
                    self.serialize_expression(label);
                    self.pop_char();
                    self.push_str_newline(":");
                    self.ident_level += 1;
                    for st in sts.into_iter() {
                        self.serialize_statement(*st);
                    }
                    self.ident_level -= 1;
                }
                match default {
                    Some(sts) => {
                        self.push_str(&" ".repeat(self.ident_level*4));
                        self.push_str_newline("default:");
                        self.ident_level += 1;
                        for st in sts.into_iter() {
                            self.serialize_statement(*st);
                        }
                        self.ident_level -= 1;
                    },
                    None => {},
                }
                self.ident_level -= 1;
                self.push_str(&" ".repeat(self.ident_level*4));
                self.push_str_newline("}");
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::resolver::Resolver;
    
    #[test]
    fn function_empty() {
        let source = "int f() {}";
        let expected_transformed_source = "int f() {}\n";

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        let (ast, _errors) = Resolver::new(generic_ast, errors).run();
        let transformed_source = Serializer::new(ast).run();
        assert_eq!(transformed_source, expected_transformed_source);
    }

    #[test]
    fn function_single() {
        let source = "int f(int a) {}";
        let expected_transformed_source = "int f(int a) {}\n";

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        let (ast, _errors) = Resolver::new(generic_ast, errors).run();
        let transformed_source = Serializer::new(ast).run();
        assert_eq!(transformed_source, expected_transformed_source);
    }

    #[test]
    fn function_multiple() {
        let source = "int f(int a, float b) {}";
        let expected_transformed_source = "int f(int a, float b) {}\n";

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        let (ast, _errors) = Resolver::new(generic_ast, errors).run();
        let transformed_source = Serializer::new(ast).run();
        assert_eq!(transformed_source, expected_transformed_source);
    }

    #[test]
    fn expression() {
        // Put `\"c\"[0];` in when index is fully implemented.
        let source = "int f(int a) {
            f(++a--);
            1 + 1.1 - 'b';
        }";
        let expected_transformed_source = "int f(int a) {
    f(++a--);
    1 + 1.1 - 'b';
}\n";

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        let (ast, _errors) = Resolver::new(generic_ast, errors).run();
        let transformed_source = Serializer::new(ast).run();
        assert_eq!(transformed_source, expected_transformed_source);
    }

    #[test]
    fn statement() {
        let source = "int f() {
            int a;
            int b = 1;
            int c, d = 2;

            while (1) {
                continue;
            }

            do {
                break;
            } while (1);

            for (a = 1; a < 2; a++) a++;
            for ( ; ; ) a++;
            
            if (1) 2;
            if (1) 2; else 3;

            switch (a) {
                case 1:
                    1;
                default:
                    2;
            }

            {
                1;
            }

            return 0;
            return;
        }";
        let expected_transformed_source = "int f() {
    int a;
    int b = 1;
    int c, d = 2;
    while (1) {
        continue;
    }
    do {
        break;
    } while (1);
    for (a = 1; a < 2; a++) a++;
    for ( ; ; ) a++;
    if (1) 2;
    if (1) 2; else 3;
    switch (a) {
        case 1:
            1;
        default:
            2;
    }
    {
        1;
    }
    return 0;
    return;
}\n";

        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        let (ast, _errors) = Resolver::new(generic_ast, errors).run();
        let transformed_source = Serializer::new(ast).run();
        assert_eq!(transformed_source, expected_transformed_source);
    }
}
