mod structure;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

pub struct Transpiler {
    source: String,
}

impl Transpiler {
    pub fn new(source: &str) -> Result<Transpiler, String> {
        if source.is_empty() {
            Err("Empty source file is not allowed.".to_string())
        } else {
            Ok(Transpiler { source: source.to_string() })
        }
    }

    pub fn run(&self) -> Result<String, String> {
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&self.source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        if errors.len() == 0 {
            Ok(format!("{:#?}", generic_ast))
        } else {
            Err(format!("{:#?}", errors))
        }
    }
}
