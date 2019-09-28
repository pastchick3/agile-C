use super::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Type {
    Var { line_no: usize, char_no: usize },
    Int { line_no: usize, char_no: usize },
    Long { line_no: usize, char_no: usize },
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Ident { value: &'a str, line_no: usize, char_no: usize },
    Num { value: i32, line_no: usize, char_no: usize },
    Str { value: &'a str, line_no: usize, char_no: usize },
    Assign { ident: Box<Expression<'a>>, value: Box<Expression<'a>> },
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Expr { expr: Expression<'a> },
    Def { type_: Type, ident: Expression<'a>, value: Expression<'a> },
}

#[derive(Debug, PartialEq)]
pub enum Branch<'a> {
    Single { stmt: Statement<'a> },
    Double { original: Statement<'a>, modified: Statement<'a> },
}

pub struct BranchedParser<'a> {
    token_stream: Vec<Token<'a>>,
    cur_index: usize,
}

impl<'a> BranchedParser<'a> {
    pub fn new(token_stream: Vec<Token<'a>>) -> BranchedParser {
        BranchedParser {
            token_stream,
            cur_index: 0,
        }
    }

    fn cur_tk(&self) -> &Token<'a> {
        &self.token_stream[self.cur_index]
    }

    fn peek_tk(&self) -> &Token<'a> {
        &self.token_stream[self.cur_index+1]
    }

    fn forward(&mut self) {
        self.cur_index += 1;
    }

    fn backward(&mut self) {
        self.cur_index -= 1;
    }

    fn assert_tk(&mut self, name: &str) {
        let tk = self.cur_tk();
        let s = format!("{:?}", tk);
        if s.starts_with(name) {
            self.forward();
        } else {
            panic!("Expect Token `{:?}`, get Token `{:?}`", name, tk);
        }
    }

    pub fn run(&mut self) -> Vec<Branch<'a>> {
        let mut ast = Vec::new();
        while self.cur_index < self.token_stream.len() - 1 {
            ast.push(self.parse_branch());
        }
        self.assert_tk("EOF");
        ast
    }

    fn parse_branch(&mut self) -> Branch<'a> {
        match self.parse_statment() {
            Statement::Expr { expr: Expression::Assign { ident, value } } => {
                let (ident_1, ident_2) = {
                    match *ident {
                        Expression::Ident { value, line_no, char_no } => (
                            Expression::Ident { value, line_no, char_no },
                            Expression::Ident { value, line_no, char_no },
                        ),
                        _ => panic!("Not impossible."),
                    }
                };
                let (value_1, value_2) = {
                    match *value {
                        Expression::Num { value, line_no, char_no } => (
                            Expression::Num { value, line_no, char_no },
                            Expression::Num { value, line_no, char_no },
                        ),
                        _ => panic!("Not implemented."),
                    }
                };
                Branch::Double {
                    original: Statement::Expr { expr: Expression::Assign {
                        ident: Box::new(ident_1),
                        value: Box::new(value_1),
                    }},
                    modified: Statement::Def {
                        type_: Type::Var { line_no: 0, char_no: 0 },
                        ident: ident_2,
                        value: value_2,
                    },
                }
            },
            stmt => Branch::Single { stmt },
        }
    }

    fn parse_statment(&mut self) -> Statement<'a> {
        let stmt = match self.cur_tk() {
            Token::Int { literal: _, line_no: _, char_no: _ } => self.parse_stmt_def(),
            _ => self.parse_stmt_expr(),
        };
        self.assert_tk("Semicolon");
        stmt
    }

    fn parse_stmt_def(&mut self) -> Statement<'a> {
        let type_ = match self.cur_tk() {
            Token::Int { literal: _, line_no: ln, char_no: cn } => Type::Int { line_no: *ln, char_no: *cn },
            _ => panic!("Unknown Token `{:?}` for type_ when parsing Statement::Def", self.cur_tk()),
        };
        self.forward();
        let ident = match self.cur_tk() {
            Token::Ident { literal: li, line_no: ln, char_no: cn } => Expression::Ident { value: *li, line_no: *ln, char_no: *cn },
            _ => panic!("Expect Token::Ident get `{:?}` for ident when parsing Statement::Def", self.cur_tk()),
        };
        self.forward();
        self.assert_tk("Eq");
        let value = self.parse_expr();
        Statement::Def { type_, ident, value }
    }

    fn parse_stmt_expr(&mut self) -> Statement<'a> {
        Statement::Expr { expr: self.parse_expr() }
    }

    fn parse_expr(&mut self) -> Expression<'a> {
        let expr = match self.cur_tk() {
            Token::Ident { literal: li, line_no: ln, char_no: cn } => {
                let ident = Expression::Ident { value: *li, line_no: *ln, char_no: *cn };
                match self.peek_tk() {
                    Token::Eq { literal: _, line_no: _, char_no: _ } => self.parse_expr_assign(ident),
                    _ => ident,
                }
            },
            Token::Num { literal: li, line_no: ln, char_no: cn } => {
                Expression::Num { value: li.parse().unwrap(), line_no: *ln, char_no: *cn }
            },
            Token::Str { literal: li, line_no: ln, char_no: cn } => {
                Expression::Str { value: *li, line_no: *ln, char_no: *cn }
            },
            tk => panic!("Unknown Token `{:?}` when parsing Expression", tk),
        };
        if let Expression::Assign { ident: _, value: _ } = expr {
            self.backward();
        }
        self.forward();
        expr
    }

    fn parse_expr_assign(&mut self, ident: Expression<'a>) -> Expression<'a> {
        self.forward();
        self.assert_tk("Eq");
        let value = self.parse_expr();
        Expression::Assign { ident: Box::new(ident), value: Box::new(value) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::lexer::Lexer;
    
    #[test]
    fn lexer() {
        let input = "int a = 1;
b = \"2\";
        ";
        let output = vec!(
            Statement::Def {
                type_: Type::Int {
                    line_no: 1,
                    char_no: 1,
                },
                ident: Expression::Ident {
                    value: "a",
                    line_no: 1,
                    char_no: 5,
                },
                value: Expression::Num {
                    value: 1,
                    line_no: 1,
                    char_no: 9,
                },
            },
            Statement::Expr {
                expr: Expression::Assign {
                    ident: Box::new(Expression::Ident {
                        value: "b",
                        line_no: 2,
                        char_no: 1,
                    }),
                    value: Box::new(Expression::Str {
                        value: "2",
                        line_no: 2,
                        char_no: 5,
                    }),
                },
            },
        );
        let lexer = Lexer::new(&input);
        let token_stream = lexer.run();
        let mut parser = Parser::new(token_stream);
        let ast = parser.run();
        assert_eq!(ast, output);
    }
}

