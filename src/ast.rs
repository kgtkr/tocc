use crate::pos::Pos;

#[derive(Debug, Clone)]
pub enum ExprPayload {
    IntLit(i64),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub pos: Pos,
    pub payload: ExprPayload,
}
