use crate::loc::{Loc, Locatable};

#[derive(Debug, Clone)]
pub enum Type {
    Int(TypeInt),
}

impl Locatable for Type {
    fn loc(&self) -> &Loc {
        match self {
            Type::Int(x) => x.loc(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeInt {
    pub int_loc: Loc,
}

impl Locatable for TypeInt {
    fn loc(&self) -> &Loc {
        &self.int_loc
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Func(DeclFunc),
}

impl Locatable for Decl {
    fn loc(&self) -> &Loc {
        match self {
            Decl::Func(x) => x.loc(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DeclFunc {
    pub typ: Type,
    pub ident_loc: Loc,
    pub ident: String,
    pub params: Vec<DeclParam>,
    pub body: StmtCompound,
}

impl Locatable for DeclFunc {
    fn loc(&self) -> &Loc {
        self.typ.loc()
    }
}

#[derive(Debug, Clone)]
pub struct DeclParam {
    pub typ: Type,
    pub ident: String,
    pub ident_loc: Loc,
}

impl Locatable for DeclParam {
    fn loc(&self) -> &Loc {
        self.typ.loc()
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(StmtExpr),
    Return(StmtReturn),
    Compound(StmtCompound),
    VarDecl(StmtVarDecl),
    If(StmtIf),
    While(StmtWhile),
    For(StmtFor),
}

impl Locatable for Stmt {
    fn loc(&self) -> &Loc {
        match self {
            Stmt::Expr(x) => x.loc(),
            Stmt::Return(x) => x.loc(),
            Stmt::Compound(x) => x.loc(),
            Stmt::VarDecl(x) => x.loc(),
            Stmt::If(x) => x.loc(),
            Stmt::While(x) => x.loc(),
            Stmt::For(x) => x.loc(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StmtExpr {
    pub expr: Expr,
}

impl Locatable for StmtExpr {
    fn loc(&self) -> &Loc {
        self.expr.loc()
    }
}

#[derive(Debug, Clone)]
pub struct StmtReturn {
    pub return_loc: Loc,
    pub expr: Expr,
}

impl Locatable for StmtReturn {
    fn loc(&self) -> &Loc {
        &self.return_loc
    }
}

#[derive(Debug, Clone)]
pub struct StmtCompound {
    pub lbrace_loc: Loc,
    pub stmts: Vec<Stmt>,
}

impl Locatable for StmtCompound {
    fn loc(&self) -> &Loc {
        &self.lbrace_loc
    }
}

#[derive(Debug, Clone)]
pub struct StmtVarDecl {
    pub typ: Type,
    pub ident: String,
    pub ident_loc: Loc,
}

impl Locatable for StmtVarDecl {
    fn loc(&self) -> &Loc {
        self.typ.loc()
    }
}

#[derive(Debug, Clone)]
pub struct StmtIf {
    pub if_loc: Loc,
    pub cond: Expr,
    pub then: Box<Stmt>,
    pub else_: Option<Box<Stmt>>,
}

impl Locatable for StmtIf {
    fn loc(&self) -> &Loc {
        &self.if_loc
    }
}

#[derive(Debug, Clone)]
pub struct StmtWhile {
    pub while_loc: Loc,
    pub cond: Expr,
    pub body: Box<Stmt>,
}

impl Locatable for StmtWhile {
    fn loc(&self) -> &Loc {
        &self.while_loc
    }
}

#[derive(Debug, Clone)]
pub struct StmtFor {
    pub for_loc: Loc,
    pub init: Option<Expr>,
    pub cond: Option<Expr>,
    pub step: Option<Expr>,
    pub body: Box<Stmt>,
}

impl Locatable for StmtFor {
    fn loc(&self) -> &Loc {
        &self.for_loc
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
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

impl Locatable for Expr {
    fn loc(&self) -> &Loc {
        match self {
            Expr::IntLit(x) => x.loc(),
            Expr::Add(x) => x.loc(),
            Expr::Sub(x) => x.loc(),
            Expr::Mul(x) => x.loc(),
            Expr::Div(x) => x.loc(),
            Expr::Neg(x) => x.loc(),
            Expr::Eq(x) => x.loc(),
            Expr::Ne(x) => x.loc(),
            Expr::Lt(x) => x.loc(),
            Expr::Le(x) => x.loc(),
            Expr::Gt(x) => x.loc(),
            Expr::Ge(x) => x.loc(),
            Expr::LValue(x) => x.loc(),
            Expr::Assign(x) => x.loc(),
            Expr::Call(x) => x.loc(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprLValue {
    Var(LValueVar),
}

impl Locatable for ExprLValue {
    fn loc(&self) -> &Loc {
        match self {
            ExprLValue::Var(x) => x.loc(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprIntLit {
    pub value: i64,
    pub value_loc: Loc,
}

impl Locatable for ExprIntLit {
    fn loc(&self) -> &Loc {
        &self.value_loc
    }
}

#[derive(Debug, Clone)]
pub struct ExprAdd {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprAdd {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprSub {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprSub {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprMul {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprMul {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprDiv {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprDiv {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprNeg {
    pub minus_loc: Loc,
    pub expr: Box<Expr>,
}

impl Locatable for ExprNeg {
    fn loc(&self) -> &Loc {
        &self.minus_loc
    }
}

#[derive(Debug, Clone)]
pub struct ExprEq {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprEq {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprNe {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprNe {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprLt {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprLt {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprLe {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprLe {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprGt {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprGt {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprGe {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprGe {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct LValueVar {
    pub ident: String,
    pub ident_loc: Loc,
}

impl Locatable for LValueVar {
    fn loc(&self) -> &Loc {
        &self.ident_loc
    }
}

#[derive(Debug, Clone)]
pub struct ExprAssign {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Locatable for ExprAssign {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub ident: String,
    pub ident_loc: Loc,
    pub args: Vec<Expr>,
}

impl Locatable for ExprCall {
    fn loc(&self) -> &Loc {
        &self.ident_loc
    }
}
