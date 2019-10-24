use crate::structure::{Array, Expression, Function, Statement, Type};

pub struct Serializer<'a> {
    ast: Option<Vec<Function<'a>>>,
    transformed_source: Option<String>,
    ident_level: usize,
}

impl<'a> Serializer<'a> {
    pub fn new(ast: Vec<Function<'a>>) -> Serializer {
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

    fn serialize_function(&mut self, func: Function<'a>) {
        let Function {
            r#type,
            name,
            parameters,
            body,
            ..
        } = func;
        self.serialize_type(r#type);
        self.push_str_space(name);
        self.pop_char();
        self.push_str("(");
        if !parameters.is_empty() {
            for (param, r#type) in parameters {
                self.serialize_type(r#type);
                self.push_str(param);
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
        }
    }

    fn serialize_expression(&mut self, expr: Expression<'a>) {
        match expr {
            Expression::Ident { value, .. } => self.push_str_space(value),
            Expression::IntConst { value, .. } => self.push_str_space(&value.to_string()),
            Expression::FloatConst { value, .. } => self.push_str_space(&value.to_string()),
            Expression::CharConst { value, .. } => self.push_str_space(value),
            Expression::StrConst { value, .. } => self.push_str_space(value),
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
                self.push_str_space(operator);
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

    fn serialize_statement(&mut self, stmt: Statement<'a>) {
        if self.transformed_source.as_ref().unwrap().ends_with('\n') {
            self.push_str(&" ".repeat(self.ident_level * 4));
        }
        match stmt {
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
                let r#type = declarators.last().unwrap().0;
                self.serialize_type(r#type);
                for (r#type, ident, init) in declarators {
                    let (array_flag, array_len) = r#type.get_array();
                    if array_flag {
                        self.push_str(ident);
                        self.push_str("[");
                        if let Some(len) = array_len {
                            self.push_str(&len.to_string());
                        }
                        self.push_str_space("]");
                    } else {
                        self.push_str_space(ident);
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
                        self.serialize_expression(ex);
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
    use crate::resolver::Resolver;

    #[test]
    fn types() {
        let source = "void f() {}
char f(int a) {}
short f(int a, float b) {}
int f() {}
long f() {}
unsigned short f() {}
unsigned int f() {}
unsigned long f() {}
float f() {}
double f() {}";
        let expected_transformed_source = "void f() {}

char f(int a) {}

short f(int a, float b) {}

int f() {}

long f() {}

unsigned short f() {}

unsigned int f() {}

unsigned long f() {}

float f() {}

double f() {}\n";
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        let (ast, _errors) = Resolver::new(generic_ast, errors).run();
        let transformed_source = Serializer::new(ast).run();
        assert_eq!(transformed_source, expected_transformed_source);
    }

    #[test]
    fn expression() {
        let source = "int f(int a) {
            a;
            1;
            1.1;
            '1';
            \"1\";
            +1 + a++;
            \"1\"[0];
            f(1);
        }";
        let expected_transformed_source = "int f(int a) {
    a;
    1;
    1.1;
    '1';
    \"1\";
    +1 + a++;
    \"1\"[0];
    f(1);
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
            int e[] = {}, f[1] = { 1 }, g[2] = { 1, 2 };

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
    int e[] = {}, f[1] = { 1 }, g[2] = { 1, 2 };
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
