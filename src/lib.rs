mod lexer;
mod parser;
mod resolver;
mod serializer;
mod structure;

use lexer::Lexer;
use parser::Parser;
use resolver::Resolver;
use serializer::Serializer;

pub struct Transpiler {
    source: String,
}

impl Transpiler {
    pub fn new(source: &str) -> Result<Transpiler, String> {
        if source.is_empty() {
            Err("Empty source file is not allowed.".to_string())
        } else {
            Ok(Transpiler {
                source: source.to_string(),
            })
        }
    }

    pub fn run(&self) -> Result<String, String> {
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&self.source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        let (ast, errors) = Resolver::new(generic_ast, errors).run();
        let transformed_source = Serializer::new(ast).run();
        if errors.len() == 0 {
            Ok(transformed_source)
        } else {
            Err(errors.iter().fold("\n".to_string(), |mut msg, err| {
                msg.push_str(&format!("    {}\n", err));
                msg.pop();
                msg
            }))
        }
    }
}
