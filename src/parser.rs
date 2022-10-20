use crate::ast::{Expr, ExprPayload};
use crate::loc::Loc;
use crate::token::{Token, TokenPayload};
use guard::guard;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("{loc} - invalid token: {token}, expected: {expected}")]
    InvalidToken {
        loc: Loc,
        token: Token,
        expected: String,
    },
}
#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, idx: 0 }
    }

    fn inc_idx(&mut self) {
        self.idx += 1;
    }

    fn peek(&self) -> &Token {
        // 最後のトークンはEOF
        self.tokens.get(self.idx).unwrap()
    }

    fn int_lit(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek().clone();
        guard!(let TokenPayload::IntLit(i) = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: "int literal".to_string(),
            });
        });
        self.inc_idx();
        Ok(Expr {
            loc: token.loc.clone(),
            payload: ExprPayload::IntLit(*i),
        })
    }

    pub fn eof(&mut self) -> Result<(), ParseError> {
        let token = self.peek();
        guard!(let TokenPayload::EOF = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: "EOF".to_string(),
            });
        });
        self.inc_idx();
        Ok(())
    }

    pub fn expr(&mut self) -> Result<Expr, ParseError> {
        self.int_lit()
    }
}
