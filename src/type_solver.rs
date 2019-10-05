use super::branched_parser::Statement;
use super::branched_parser::Type;

pub struct TypeResolver {}

impl TypeResolver {
    pub fn new() -> TypeResolver {
        TypeResolver {}
    }

    pub fn run<'a>(&self, generic_ast: Vec<Statement<'a>>) -> Vec<Statement<'a>> {
        let mut ast = Vec::new();
        for stmt in generic_ast.into_iter() {
            ast.push(self.resolve_stmt(stmt));
        }
        ast
    }

    fn resolve_stmt<'a>(&self, stmt: Statement<'a>) -> Statement<'a> {
        match stmt {
            Statement::Def { type_, ident, value } => {
                match type_ {
                    Type::Var { line_no, char_no } => Statement::Def { type_: Type::Long { line_no, char_no }, ident, value },
                    Type::Int { line_no, char_no } => Statement::Def { type_: Type::Int { line_no, char_no }, ident, value },
                    Type::Long { line_no, char_no } => Statement::Def { type_: Type::Long { line_no, char_no }, ident, value },
                }
            },
            s => s,
        }
    }
}
