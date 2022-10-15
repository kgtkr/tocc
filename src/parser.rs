use crate::ast::Expr;
use crate::pos::Pos;
use crate::token::Token;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("{pos} - invalid character: {token}")]
    InvalidToken { pos: Pos, token: Token },
}
#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}
