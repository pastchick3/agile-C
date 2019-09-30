mod structure;
mod lexer;

pub struct Transpiler {
    source: String,
}

impl Transpiler {
    pub fn new(source: &str) -> Transpiler {
        Transpiler {
            source: source.to_string(),
        }
    }

    pub fn run(&self) -> Result<String, String> {
        let mut errors = Vec::new();
        let token_stream = Lexer::run(&self.source, &mut errors);
        if errors.len() == 0 {
            Ok(format!("{:#?}", token_stream))
        } else {
            Error(format!("{:#?}", errors))
        }
    }
}
