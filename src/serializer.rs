use crate::structure::{Expression, Function, Statement, StaticObject, Type};

pub struct Serializer {
    ast: Option<Vec<StaticObject>>,
    transformed_source: Option<String>,
    ident_level: usize,
}

/// The serializer construct a string from a valid AST.
impl Serializer {
    pub fn new(ast: Vec<StaticObject>) -> Self {
        Serializer {
            ast: Some(ast),
            transformed_source: Some(String::new()),
            ident_level: 0,
        }
    }

    /// Construct a string from a valid AST.
    pub fn run(&mut self) -> String {
        self.ast
            .take()
            .unwrap()
            .iter()
            .for_each(|obj| self.serialize_static_object(obj));
        self.pop_char();
        self.transformed_source.take().unwrap()
    }

    /// Append s to the back of the output string.
    fn push_str(&mut self, s: &str) {
        self.transformed_source.as_mut().unwrap().push_str(s);
    }

    /// Append s and then a whitespace to the back of the output string.
    fn push_str_space(&mut self, s: &str) {
        self.push_str(s);
        self.push_str(" ");
    }

    /// Append s and then a newline to the back of the output string.
    fn push_str_newline(&mut self, s: &str) {
        self.push_str(s);
        self.push_str("\n");
    }

    /// Remove the last char from the output string.
    fn pop_char(&mut self) {
        self.transformed_source.as_mut().unwrap().pop();
    }

    /// This function will automatically add a newline when done.
    fn serialize_static_object(&mut self, object: &StaticObject) {
        match object {
            StaticObject::Type(type_) => {
                self.serialize_type(type_);
                self.pop_char();
                self.push_str_newline(";");
            }
            StaticObject::Function(func) => self.serialize_function(func),
        }
        self.push_str_newline("");
    }

    /// This function will automatically add nothing when done.
    fn serialize_function(&mut self, func: &Function) {
        let Function {
            return_type,
            name,
            parameters,
            ellipsis,
            body,
            ..
        } = func;

        // Serialize the return type.
        self.serialize_type(&return_type.borrow());
        self.serialize_pointer_marker(&return_type.borrow());

        // Serialize the function name.
        self.push_str_space(name);
        self.pop_char();

        // Serialize the parameter list.
        self.push_str("(");
        if !parameters.is_empty() {
            for (param, type_) in parameters {
                self.serialize_type(&type_.borrow());
                self.serialize_pointer_marker(&type_.borrow());
                self.push_str(param);
                self.serialize_array_marker(&type_.borrow());
                self.push_str_space(",");
            }
            self.pop_char();
            self.pop_char();
        } else if !ellipsis {
            self.push_str("void");
        }
        self.push_str_space(")");

        // Serialize the function body.
        self.serialize_statement(body);
    }

    /// This function will automatically add a whitespace when done.
    ///
    /// This function will only serialize the base type of pointers and arrays.
    fn serialize_type(&mut self, type_: &Type) {
        match type_ {
            Type::T(Some(typ)) => self.serialize_type(typ),
            Type::Void(_) => self.push_str_space("void"),
            Type::Char(_) => self.push_str_space("char"),
            Type::Double(_) => self.push_str_space("double"),
            Type::Float(_) => self.push_str_space("float"),
            Type::Long(_) => self.push_str_space("long"),
            Type::UnsignedLong(_) => self.push_str_space("unsigned long"),
            Type::Int(_) => self.push_str_space("int"),
            Type::UnsignedInt(_) => self.push_str_space("unsigned int"),
            Type::Short(_) => self.push_str_space("short"),
            Type::UnsignedShort(_) => self.push_str_space("unsigned short"),
            Type::Pointer { refer, .. } => self.serialize_type(refer),
            Type::Array { content, .. } => self.serialize_type(content),
            Type::Struct { name, members, .. } => {
                self.push_str_space("struct");
                self.push_str_space(name);
                if members.is_empty() {
                    self.push_str_space("{}");
                } else {
                    self.push_str_newline("{");
                    self.ident_level += 1;
                    for (member, type_) in members {
                        self.push_str(&" ".repeat(self.ident_level * 4));
                        self.serialize_type(type_);
                        self.push_str_space(member);
                        self.pop_char();
                        self.push_str_newline(";")
                    }
                    self.ident_level -= 1;
                    self.push_str(&" ".repeat(self.ident_level * 4));
                    self.push_str_space("}");
                }
            }
            typ => panic!("Try to serialize a dummy type `{}`.", typ),
        }
    }

    /// This function will automatically add nothing when done.
    fn serialize_pointer_marker(&mut self, type_: &Type) {
        match type_ {
            Type::Array { content, .. } => {
                // In a complex type combining arrays and pointers,
                // pointers are nested deeper than arrays.
                self.serialize_pointer_marker(content);
            }
            Type::Pointer { refer, .. } => {
                self.serialize_pointer_marker(refer);
                self.push_str("*");
            }
            _ => (),
        }
    }

    /// This function will automatically add nothing when done.
    fn serialize_array_marker(&mut self, type_: &Type) {
        match type_ {
            Type::Array {
                content, length, ..
            } => {
                self.serialize_array_marker(content);
                if let Some(len) = length {
                    self.push_str("[");
                    self.push_str(&len.to_string());
                    self.push_str("]");
                } else {
                    self.push_str("[]");
                }
            }
            _ => (),
        }
    }

    /// This function will automatically add a newline when done.
    fn serialize_statement(&mut self, statement: &Statement) {
        // Not every statement starts a new line, for example, the initializer
        // in the for loop.
        if self.transformed_source.as_ref().unwrap().ends_with('\n') {
            self.push_str(&" ".repeat(self.ident_level * 4));
        }

        match statement {
            Statement::Null(_) => self.push_str_newline(";"),
            Statement::Expr(expr) => {
                self.serialize_expression(expr);
                self.pop_char();
                self.push_str_newline(";");
            }
            Statement::Continue(_) => self.push_str_newline("continue;"),
            Statement::Break(_) => self.push_str_newline("break;"),
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
                if statements.is_empty() {
                    self.push_str_newline("{}");
                } else {
                    self.push_str_newline("{");
                    self.ident_level += 1;
                    for stmt in statements {
                        self.serialize_statement(stmt);
                    }
                    self.ident_level -= 1;
                    self.push_str(&" ".repeat(self.ident_level * 4));
                    self.push_str_newline("}");
                }
            }
            Statement::Def {
                base_type,
                declarators,
                ..
            } => {
                self.serialize_type(&base_type.borrow());
                for (type_, ident, init) in declarators {
                    self.serialize_pointer_marker(&type_.borrow());
                    self.push_str(ident);
                    self.serialize_array_marker(&type_.borrow());
                    if let Some(expr) = init {
                        self.push_str(" ");
                        self.push_str_space("=");
                        self.serialize_expression(expr);
                        self.pop_char();
                    }
                    self.push_str_space(",");
                }
                self.pop_char();
                self.pop_char();
                self.push_str_newline(";");
            }
            Statement::While {
                condition, body, ..
            } => {
                self.push_str_space("while");
                self.push_str("(");
                self.serialize_expression(condition);
                self.pop_char();
                self.push_str_space(")");
                self.serialize_statement(body);
            }
            Statement::Do {
                condition, body, ..
            } => {
                self.push_str_space("do");
                self.serialize_statement(body);
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
                    Some(stmt) => {
                        self.serialize_statement(stmt);
                        self.pop_char();
                        self.pop_char();
                    }
                    None => self.push_str_space(""),
                }
                self.push_str_space(";");
                if let Some(expr) = condition {
                    self.serialize_expression(expr);
                    self.pop_char();
                }
                self.push_str_space(";");
                if let Some(expr) = increment {
                    self.serialize_expression(expr);
                    self.pop_char();
                }
                self.push_str_space(")");
                self.serialize_statement(body);
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
                self.serialize_statement(body);
                if let Some(stmt) = alternative {
                    self.pop_char();
                    self.push_str(" else ");
                    self.serialize_statement(stmt);
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

                // Serialize an empty switch.
                if branches.is_empty() && default.is_none() {
                    self.push_str_newline("{}");
                    return;
                }

                // Serialize branches.
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

                // Serialize the default branch.
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

    /// This function will automatically add a whitespace when done.
    fn serialize_expression(&mut self, expression: &Expression) {
        match expression {
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
                self.serialize_expression(expression);
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                self.serialize_expression(left);
                if let &"." | &"->" = operator {
                    self.pop_char();
                    self.push_str(operator);
                } else {
                    self.push_str_space(operator);
                }
                self.serialize_expression(right);
            }
            Expression::Postfix {
                operator,
                expression,
            } => {
                self.serialize_expression(expression);
                self.pop_char();
                self.push_str_space(operator);
            }
            Expression::Group { expression, .. } => {
                self.push_str("(");
                self.serialize_expression(expression);
                self.pop_char();
                self.push_str_space(")");
            }
            Expression::Index { expression, index } => {
                self.serialize_expression(expression);
                self.pop_char();
                self.push_str("[");
                self.serialize_expression(index);
                self.pop_char();
                self.push_str_space("]");
            }
            Expression::Call {
                expression,
                arguments,
            } => {
                self.serialize_expression(expression);
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
            Expression::InitList { initializers, .. } => {
                if initializers.is_empty() {
                    return self.push_str_space("{}");
                }
                self.push_str_space("{");
                for (member, expr) in initializers {
                    if let Some(name) = member {
                        self.push_str(".");
                        self.push_str_space(name);
                        self.push_str_space("=");
                    }
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::preprocessor::Preprocessor;
    use crate::resolver::Resolver;

    #[test]
    fn struct_() {
        let source = "
            struct A {};
        ";
        let expected_errors = vec![];
        let expected_transformed_source = "struct A {};\n";
        let mut errors = Vec::new();
        let lines = Preprocessor::new("file", source, &mut errors)
            .run()
            .unwrap();
        let tokens = Lexer::new(lines, &mut errors).run().unwrap();
        let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
        let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
        let transformed_source = Serializer::new(ast).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(&transformed_source, expected_transformed_source);
    }

    #[test]
    fn function() {
        let source = "
            int *f() {}
            void f(void) {}
            void f(int a) {}
            void f(int a, unsigned int b) {}
        ";
        let expected_transformed_source = "int *f() {}

void f(void) {}

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
    fn statement() {
        // Notice `struct A` (a2) will be expanded.
        let source = "
            void f() {
                {
                    ;
                }
            
                char a[1];
                short b[] = { 1 };
                unsigned short c[1] = { 1 };
                int d[2] = { 1, 2 };
                unsigned int *e;
                long f, *g = &f;
                unsigned long h;
                float i = 1;
                double j, k = 1;

                int **arr[1][2];
                
                struct A {} a1;
                struct B {
                    int a;
                } b1 = { .a = 1 };
                struct C {
                    int a;
                    float b;
                } c1 = { .a = 1, .b = 2 };
                struct A a2;
            
                return;
            }
            
            int m(int a) {
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
    char a[1];
    short b[] = { 1 };
    unsigned short c[1] = { 1 };
    int d[2] = { 1, 2 };
    unsigned int *e;
    long f, *g = &f;
    unsigned long h;
    float i = 1;
    double j, k = 1;
    int **arr[1][2];
    struct A {} a1;
    struct B {
        int a;
    } b1 = { .a = 1 };
    struct C {
        int a;
        float b;
    } c1 = { .a = 1, .b = 2 };
    struct A {} a2;
    return;
}

int m(int a) {
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

    #[test]
    fn expression() {
        // Notice `struct S` (var) will be expanded.
        let source = "
            struct S {
                int mem;
            };
            
            void f(int a) {
                a;
                1;
                1.1;
                '1';
                \"1\";
                +1 + (a++);
                int arr[] = { 1 };
                arr[0];
                f(1);
                struct S var, *ptr;
                var.mem;
                ptr->mem;
            }
        ";
        let expected_transformed_source = "struct S {
    int mem;
};

void f(int a) {
    a;
    1;
    1.1;
    '1';
    \"1\";
    +1 + (a++);
    int arr[] = { 1 };
    arr[0];
    f(1);
    struct S {
        int mem;
    } var, *ptr;
    var.mem;
    ptr->mem;
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
