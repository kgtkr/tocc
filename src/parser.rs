use crate::ast::Expr;
use crate::token::Token;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}
