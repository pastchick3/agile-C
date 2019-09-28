use super::branched_parser::Type;
use super::branched_parser::Expression;
use super::branched_parser::Statement;

pub struct Serializer {}

impl Serializer {
    pub fn new() -> Serializer {
        Serializer {}
    }

    pub fn run<'a>(&self, ast: Vec<Statement<'a>>) -> String {
        let mut trans_source = String::new();
        for stmt in ast.into_iter() {
            trans_source += &self.serialize_stmt(stmt);
        }
        trans_source
    }

    fn serialize_stmt<'a>(&self, stmt: Statement<'a>) -> String {
        match stmt {
            Statement::Def { type_, ident, value } => {
                let mut stmt_str = match type_ {
                    Type::Var { line_no: _, char_no: _ } => String::from("var"),
                    Type::Int { line_no: _, char_no: _ } => String::from("int"),
                    Type::Long { line_no: _, char_no: _ } => String::from("long"),
                };
                stmt_str += " ";
                stmt_str += &self.serialize_expr(ident);
                stmt_str += " = ";
                stmt_str += &self.serialize_expr(value);
                stmt_str + ";"
            },
            _ => panic!("Not implemented"),
        }
    }

    fn serialize_expr<'a>(&self, expr: Expression<'a>) -> String {
        match expr {
            Expression::Ident { value, line_no: _, char_no: _ } => String::from(value),
            Expression::Num { value, line_no: _, char_no: _ } => value.to_string(),
            Expression::Str { value, line_no: _, char_no: _ } => String::from(value),
            _ => panic!("Not implemented"),
        }
    }
}
