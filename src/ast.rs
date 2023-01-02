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

#[derive(Debug, Clone)]
pub struct Stmt {
    pub loc: Loc,
    pub payload: StmtPayload,
}

#[derive(Debug, Clone)]
pub enum StmtPayload {
    Expr(Expr),
    Return(Expr),
    Compound(Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub loc: Loc,
    pub payload: DeclPayload,
}

#[derive(Debug, Clone)]
pub enum DeclPayload {
    Func { name: String, body: Vec<Stmt> },
}

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}
