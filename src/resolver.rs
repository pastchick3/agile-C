use crate::structure::{ Error, Function };

pub struct Resolver<'a> {
    generic_ast: Vec<Function<'a>>,
    errors: Option<Vec<Error>>,
    ast: Option<Vec<Function<'a>>>,
}

impl<'a> Resolver<'a> {
    pub fn new(generic_ast: Vec<Function<'a>>, errors: Vec<Error>) -> Resolver {
        Resolver {
            generic_ast,
            errors: Some(errors),
            ast: Some(Vec::new()),
        }
    }

    pub fn run(&mut self) -> (Vec<Function<'a>>, Vec<Error>) {
        loop {
            match self.generic_ast.pop() {
                Some(func) => self.ast.as_mut().unwrap().push(func),
                None => break,
            }
        }
        self.ast.as_mut().unwrap().reverse();
        (self.ast.take().unwrap(), self.errors.take().unwrap())
    }
}
