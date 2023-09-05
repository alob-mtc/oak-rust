use crate::lexer::token::Token;

struct Parser {
    tokens: Vec<Token>,
    index: i32,
    min_binary_prec: Vec<i32>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            min_binary_prec: Vec::new(),
        }
    }
}

// utils
impl Parser {}
