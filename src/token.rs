use crate::loc::Loc;

#[derive(Debug, Clone)]
pub struct Token {
    pub payload: TokenPayload,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum TokenPayload {
    IntLit(i64),
    EOF,
}
