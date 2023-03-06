use crate::loc::Loc;

#[derive(Debug, Clone)]
pub enum Type {
    Int(TypeInt),
}

#[derive(Debug, Clone)]
pub struct TypeInt {
    pub int_loc: Loc,
}

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
    pub typ: Type,
    pub ident_loc: Loc,
    pub ident: String,
    pub params: Vec<DeclParam>,
    pub body: StmtCompound,
}

#[derive(Debug, Clone)]
pub struct DeclParam {
    pub typ: Type,
    pub ident: String,
    pub ident_loc: Loc,
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
    VarDecl(StmtVarDecl),
    If(StmtIf),
    While(StmtWhile),
    For(StmtFor),
}

#[derive(Debug, Clone)]
pub struct StmtExpr {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct StmtReturn {
    pub return_loc: Loc,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct StmtCompound {
    pub lbrace_loc: Loc,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StmtVarDecl {
    pub typ: Type,
    pub ident: String,
    pub ident_loc: Loc,
}

#[derive(Debug, Clone)]
pub struct StmtIf {
    pub if_loc: Loc,
    pub cond: Expr,
    pub then: Box<Stmt>,
    pub else_: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct StmtWhile {
    pub while_loc: Loc,
    pub cond: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StmtFor {
    pub for_loc: Loc,
    pub init: Option<Expr>,
    pub cond: Option<Expr>,
    pub step: Option<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub loc: Loc,
    pub payload: ExprPayload,
}

#[derive(Debug, Clone)]
pub enum ExprPayload {
    IntLit(ExprIntLit),
    Add(ExprAdd),
    Sub(ExprSub),
    Mul(ExprMul),
    Div(ExprDiv),
    Neg(ExprNeg),
    Eq(ExprEq),
    Ne(ExprNe),
    Lt(ExprLt),
    Le(ExprLe),
    Gt(ExprGt),
    Ge(ExprGe),
    LValue(ExprLValue),
    Assign(ExprAssign),
    Call(ExprCall),
}

#[derive(Debug, Clone)]
pub enum ExprLValue {
    Var(LValueVar),
}

#[derive(Debug, Clone)]
pub struct ExprIntLit {
    pub value: i64,
    pub value_loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ExprAdd {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprSub {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprMul {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprDiv {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprNeg {
    pub minus_loc: Loc,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprEq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprNe {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprLt {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprLe {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprGt {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprGe {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct LValueVar {
    pub ident: String,
    pub ident_loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ExprAssign {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub ident: String,
    pub ident_loc: Loc,
    pub args: Vec<Expr>,
}
