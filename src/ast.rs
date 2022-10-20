use crate::loc::Loc;

#[derive(Debug, Clone)]
pub enum ExprPayload {
    IntLit(i64),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub loc: Loc,
    pub payload: ExprPayload,
}
