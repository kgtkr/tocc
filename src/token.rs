use crate::loc::Loc;
use derive_more::Display;

#[derive(Debug, Clone, Display)]
#[display(fmt = "{loc} - {value}")]
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
    Plus,
    Minus,
}
