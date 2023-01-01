use crate::loc::Loc;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Token {
    pub payload: TokenPayload,
    pub loc: Loc,
    // eofの場合は<EOF>
    pub value: String,
}

#[derive(Debug, Clone)]
pub enum TokenPayload {
    IntLit(i64),
    Ident(String),
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
    Return,
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
