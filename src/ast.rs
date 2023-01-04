use crate::loc::Loc;

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub loc: Loc,
    pub payload: DeclPayload,
}

#[derive(Debug, Clone)]
pub enum DeclPayload {
    Func(DeclFunc),
}

#[derive(Debug, Clone)]
pub struct DeclFunc {
    pub name: String,
    pub body: StmtCompound,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub loc: Loc,
    pub payload: StmtPayload,
}

#[derive(Debug, Clone)]
pub enum StmtPayload {
    Expr(StmtExpr),
    Return(StmtReturn),
    Compound(StmtCompound),
}

#[derive(Debug, Clone)]
pub struct StmtExpr {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct StmtReturn {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct StmtCompound {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub loc: Loc,
    pub payload: ExprPayload,
}

#[derive(Debug, Clone)]
pub enum ExprPayload {
    IntLit(ExprIntLit),
}

#[derive(Debug, Clone)]
pub struct ExprIntLit {
    pub value: i64,
}
