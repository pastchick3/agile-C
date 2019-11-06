//! A serializer serializing a complete AST to a output string.

use crate::structure::{Array, Expression, Function, Pointer, Statement, Type};

pub struct Serializer {
    ast: Option<Vec<Function>>,
    transformed_source: Option<String>,
    ident_level: usize,
}

impl Serializer {
    pub fn new(ast: Vec<Function>) -> Serializer {
        Serializer {
            ast: Some(ast),
            transformed_source: Some(String::new()),
            ident_level: 0,
        }
    }

    pub fn run(&mut self) -> String {
        self.ast
            .take()
            .unwrap()
            .into_iter()
            .for_each(|func| self.serialize_function(func));
        self.pop_char();
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

    fn serialize_function(&mut self, func: Function) {
        let Function {
            r#type,
            name,
            parameters,
            body,
            ..
        } = func;
        self.serialize_type(r#type);
        self.push_str_space(&name);
        self.pop_char();
        self.push_str("(");
        if !parameters.is_empty() {
            for (param, r#type) in parameters {
                self.serialize_type(r#type);
                self.push_str(&param);
                self.push_str_space(",");
            }
            self.pop_char();
            self.pop_char();
        }
        self.push_str_space(")");
        self.serialize_statement(body);
        self.push_str_newline("");
    }

    fn serialize_type(&mut self, r#type: Type) {
        match r#type {
            Type::T { .. } => self.push_str_space("T"),
            Type::Void { .. } => self.push_str_space("void"),
            Type::Char { .. } => self.push_str_space("char"),
            Type::Short { signed_flag, .. } => {
                if signed_flag {
                    self.push_str_space("short");
                } else {
                    self.push_str_space("unsigned short");
                }
            }
            Type::Int { signed_flag, .. } => {
                if signed_flag {
                    self.push_str_space("int");
                } else {
                    self.push_str_space("unsigned int");
                }
            }
            Type::Long { signed_flag, .. } => {
                if signed_flag {
                    self.push_str_space("long");
                } else {
                    self.push_str_space("unsigned long");
                }
            }
            Type::Float { .. } => self.push_str_space("float"),
            Type::Double { .. } => self.push_str_space("double"),
            Type::Struct { name, members, .. } => {
                self.push_str_space("struct");
                self.push_str_space(&name);
                if members.is_empty() {
                    self.push_str_space("{}");
                } else {
                    self.push_str_newline("{");
                    self.ident_level += 1;
                    for (member, r#type) in members {
                        self.push_str(&" ".repeat(self.ident_level * 4));
                        self.serialize_type(r#type.clone());
                        if r#type.get_pointer_flag() {
                            self.push_str("*");
                        }
                        self.push_str_space(&member);
                        let (array_flag, array_len) = r#type.get_array();
                        if array_flag {
                            self.push_str("[");
                            if let Some(len) = array_len {
                                self.push_str(&len.to_string());
                            }
                            self.push_str_space("]");
                        }
                        self.pop_char();
                        self.push_str_newline(";")
                    }
                    self.ident_level -= 1;
                    self.push_str(&" ".repeat(self.ident_level * 4));
                    self.push_str_space("}");
                }
            }
        }
    }

    fn serialize_expression(&mut self, expr: Expression) {
        match expr {
            Expression::Ident { value, .. } => self.push_str_space(&value),
            Expression::IntConst { value, .. } => self.push_str_space(&value.to_string()),
            Expression::FloatConst { value, .. } => self.push_str_space(&value.to_string()),
            Expression::CharConst { value, .. } => self.push_str_space(&value),
            Expression::StrConst { value, .. } => self.push_str_space(&value),
            Expression::Prefix {
                operator,
                expression,
                ..
            } => {
                self.push_str(operator);
                self.serialize_expression(*expression);
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                self.serialize_expression(*left);
                if let "." | "->" = operator {
                    self.pop_char();
                    self.push_str(operator);
                } else {
                    self.push_str_space(operator);
                }
                self.serialize_expression(*right);
            }
            Expression::Suffix {
                operator,
                expression,
            } => {
                self.serialize_expression(*expression);
                self.pop_char();
                self.push_str_space(operator);
            }
            Expression::Group {expression, ..} => {
                self.push_str("(");
                self.serialize_expression(*expression);
                self.pop_char();
                self.push_str_space(")");
            }
            Expression::Index { expression, index } => {
                self.serialize_expression(*expression);
                self.pop_char();
                self.push_str("[");
                self.serialize_expression(*index);
                self.pop_char();
                self.push_str_space("]");
            }
            Expression::Call {
                expression,
                arguments,
            } => {
                self.serialize_expression(*expression);
                self.pop_char();
                self.push_str("(");
                if !arguments.is_empty() {
                    for arg in arguments {
                        self.serialize_expression(arg);
                        self.pop_char();
                        self.push_str_space(",");
                    }
                    self.pop_char();
                    self.pop_char();
                }
                self.push_str_space(")");
            }
            Expression::InitList { expressions, .. } => {
                if expressions.is_empty() {
                    return self.push_str_space("{}");
                }
                self.push_str_space("{");
                for expr in expressions {
                    self.serialize_expression(expr);
                    self.pop_char();
                    self.push_str_space(",");
                }
                self.pop_char();
                self.pop_char();
                self.push_str_space(" }");
            }
        }
    }

    fn serialize_statement(&mut self, stmt: Statement) {
        if self.transformed_source.as_ref().unwrap().ends_with('\n') {
            self.push_str(&" ".repeat(self.ident_level * 4));
        }
        match stmt {
            Statement::Null(_) => self.push_str_newline(";"),
            Statement::Continue(_) => self.push_str_newline("continue;"),
            Statement::Break(_) => self.push_str_newline("break;"),
            Statement::Expr(expr) => {
                self.serialize_expression(expr);
                self.pop_char();
                self.push_str_newline(";");
            }
            Statement::Return { expression, .. } => match expression {
                Some(expr) => {
                    self.push_str_space("return");
                    self.serialize_expression(expr);
                    self.pop_char();
                    self.push_str_newline(";");
                }
                None => self.push_str_newline("return;"),
            },
            Statement::Block { statements, .. } => {
                self.push_str_newline("{");
                self.ident_level += 1;
                if statements.is_empty() {
                    self.pop_char();
                } else {
                    for stmt in statements {
                        self.serialize_statement(stmt);
                    }
                }
                self.ident_level -= 1;
                self.push_str(&" ".repeat(self.ident_level * 4));
                self.push_str_newline("}");
            }
            Statement::Def { declarators, .. } => {
                let r#type = declarators.last().unwrap().0.clone();
                self.serialize_type(r#type);
                for (r#type, ident, init) in declarators {
                    if r#type.get_pointer_flag() {
                        self.push_str("*");
                    }
                    self.push_str_space(&ident);
                    let (array_flag, array_len) = r#type.get_array();
                    if array_flag {
                        self.pop_char();
                        self.push_str("[");
                        if let Some(len) = array_len {
                            self.push_str(&len.to_string());
                        }
                        self.push_str_space("]");
                    }
                    if let Some(ex) = init {
                        self.push_str_space("=");
                        self.serialize_expression(ex);
                    }
                    self.pop_char();
                    self.push_str_space(",");
                }
                self.pop_char();
                self.pop_char();
                self.push_str_newline(";")
            }
            Statement::While {
                condition, body, ..
            } => {
                self.push_str_space("while");
                self.push_str("(");
                self.serialize_expression(condition);
                self.pop_char();
                self.push_str_space(")");
                self.serialize_statement(*body);
            }
            Statement::Do {
                condition, body, ..
            } => {
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
            }
            Statement::For {
                initialization,
                condition,
                increment,
                body,
                ..
            } => {
                self.push_str_space("for");
                self.push_str("(");
                match initialization {
                    Some(ex) => {
                        self.serialize_statement(*ex);
                        self.pop_char();
                        self.pop_char();
                    }
                    None => self.push_str_space(""),
                }
                self.push_str_space(";");
                if let Some(ex) = condition {
                    self.serialize_expression(ex);
                    self.pop_char();
                }
                self.push_str_space(";");
                if let Some(ex) = increment {
                    self.serialize_expression(ex);
                    self.pop_char();
                }
                self.push_str_space(")");
                self.serialize_statement(*body);
            }
            Statement::If {
                condition,
                body,
                alternative,
                ..
            } => {
                self.push_str_space("if");
                self.push_str("(");
                self.serialize_expression(condition);
                self.pop_char();
                self.push_str_space(")");
                self.serialize_statement(*body);
                if let Some(st) = alternative {
                    self.pop_char();
                    self.push_str_space(" else");
                    self.serialize_statement(*st);
                }
            }
            Statement::Switch {
                expression,
                branches,
                default,
                ..
            } => {
                self.push_str_space("switch");
                self.push_str("(");
                self.serialize_expression(expression);
                self.pop_char();
                self.push_str_space(")");
                self.push_str_newline("{");
                self.ident_level += 1;
                for (label, stmts) in branches {
                    self.push_str(&" ".repeat(self.ident_level * 4));
                    self.push_str_space("case");
                    self.serialize_expression(label);
                    self.pop_char();
                    self.push_str_newline(":");
                    self.ident_level += 1;
                    for stmt in stmts {
                        self.serialize_statement(stmt);
                    }
                    self.ident_level -= 1;
                }
                if let Some(stmts) = default {
                    self.push_str(&" ".repeat(self.ident_level * 4));
                    self.push_str_newline("default:");
                    self.ident_level += 1;
                    for stmt in stmts {
                        self.serialize_statement(stmt);
                    }
                    self.ident_level -= 1;
                }
                self.ident_level -= 1;
                self.push_str(&" ".repeat(self.ident_level * 4));
                self.push_str_newline("}");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::preprocessor::Preprocessor;
    use crate::resolver::Resolver;

    #[test]
    fn function() {
        let source = "
            void f() {}
            void f(int a) {}
            void f(int a, unsigned int b) {}
        ";
        let expected_transformed_source = "void f() {}

void f(int a) {}

void f(int a, unsigned int b) {}
";
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
        let transformed_source = Serializer::new(ast).run();
        assert!(errors.is_empty());
        assert_eq!(&transformed_source, expected_transformed_source);
    }

    #[test]
    fn expression() {
        let source = "
            void f(int a) {
                a;
                1;
                1.1;
                '1';
                \"1\";
                +1 + (a++);
                \"1\"[0];
                f(1);
                a.b;
                c->d;
            }
        ";
        let expected_transformed_source = "void f(int a) {
    a;
    1;
    1.1;
    '1';
    \"1\";
    +1 + (a++);
    \"1\"[0];
    f(1);
    a.b;
    c->d;
}
";
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
        let transformed_source = Serializer::new(ast).run();
        assert!(errors.is_empty());
        assert_eq!(&transformed_source, expected_transformed_source);
    }

    #[test]
    fn statement() {
        let source = "
            void f() {
                {
                    ;
                }

                T a = 1, a;
                char a[1];
                short a[] = { 1 };
                unsigned short a[1] = { 1 };
                int a[2] = { 1, 2 };
                unsigned int *a;
                long *a = &a;
                unsigned long a = *a;
                float a;
                double a = 1;
                
                struct A {} a1;
                struct B {
                    int a;
                } b1 = { 1 };
                struct C {
                    int a;
                    float b;
                } c1 = { 1, 2 };

                return;
            }
            
            int f(int a) {
                while (1) {
                    continue;
                }

                do {
                    break;
                } while (1);

                for (a = 1; a < 2; a++) 1;

                for ( ; ; ) 1;
                
                if (1) 1;

                if (1) 1; else 1;

                switch (1) {
                    case 1:
                        1;
                }

                switch (1) {
                    case 1:
                        1;
                    default:
                        1;
                }

                return 0;
            }
        ";
        let expected_transformed_source = "void f() {
    {
        ;
    }
    T a = 1, a;
    char a[1];
    short a[] = { 1 };
    unsigned short a[1] = { 1 };
    int a[2] = { 1, 2 };
    unsigned int *a;
    long *a = &a;
    unsigned long a = *a;
    float a;
    double a = 1;
    struct A {} a1;
    struct B {
        int a;
    } b1 = { 1 };
    struct C {
        int a;
        float b;
    } c1 = { 1, 2 };
    return;
}

int f(int a) {
    while (1) {
        continue;
    }
    do {
        break;
    } while (1);
    for (a = 1; a < 2; a++) 1;
    for ( ; ; ) 1;
    if (1) 1;
    if (1) 1; else 1;
    switch (1) {
        case 1:
            1;
    }
    switch (1) {
        case 1:
            1;
        default:
            1;
    }
    return 0;
}
";
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
        let transformed_source = Serializer::new(ast).run();
        assert!(errors.is_empty());
        assert_eq!(&transformed_source, expected_transformed_source);
    }
}
