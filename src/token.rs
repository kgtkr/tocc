use crate::pos::Pos;

#[derive(Debug, Clone)]
pub struct Token {
    pub payload: TokenPayload,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub enum TokenPayload {
    IntLit(i64),
}
