use crate::loc::{Loc, Locatable};

#[derive(Debug, Clone)]
pub enum Type {
    Int(TypeInt),
    Ptr(TypePtr),
}

impl Locatable for Type {
    fn loc(&self) -> &Loc {
        match self {
            Type::Int(x) => x.loc(),
            Type::Ptr(x) => x.loc(),
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
pub struct TypePtr {
    pub typ: Box<Type>,
}

impl Locatable for TypePtr {
    fn loc(&self) -> &Loc {
        self.typ.loc()
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
    pub sig: FuncSig,
    pub body: Option<StmtCompound>,
}

impl Locatable for DeclFunc {
    fn loc(&self) -> &Loc {
        self.sig.loc()
    }
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub typ: Type,
    pub ident_loc: Loc,
    pub ident: String,
    pub params: Vec<FuncParam>,
}

impl Locatable for FuncSig {
    fn loc(&self) -> &Loc {
        self.typ.loc()
    }
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub typ: Type,
    pub ident: String,
    pub ident_loc: Loc,
}

impl Locatable for FuncParam {
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
    BinOp(ExprBinOp),
    Neg(ExprNeg),
    Addr(ExprAddr),
    LValue(ExprLValue),
    Call(ExprCall),
}

impl Locatable for Expr {
    fn loc(&self) -> &Loc {
        match self {
            Expr::IntLit(x) => x.loc(),
            Expr::BinOp(x) => x.loc(),
            Expr::Neg(x) => x.loc(),
            Expr::LValue(x) => x.loc(),
            Expr::Call(x) => x.loc(),
            Expr::Addr(x) => x.loc(),
        }
    }
}

impl Expr {
    pub fn id(&self) -> usize {
        match self {
            Expr::IntLit(x) => x.id,
            Expr::BinOp(x) => x.id,
            Expr::Neg(x) => x.id,
            Expr::LValue(x) => x.id(),
            Expr::Call(x) => x.id,
            Expr::Addr(x) => x.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Assign,
}

#[derive(Debug, Clone)]
pub struct ExprBinOp {
    pub op: BinOp,
    pub op_loc: Loc,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub id: usize,
}

impl Locatable for ExprBinOp {
    fn loc(&self) -> &Loc {
        self.lhs.loc()
    }
}

#[derive(Debug, Clone)]
pub enum ExprLValue {
    Var(LValueVar),
    Deref(LValueDeref),
}

impl Locatable for ExprLValue {
    fn loc(&self) -> &Loc {
        match self {
            ExprLValue::Var(x) => x.loc(),
            ExprLValue::Deref(x) => x.loc(),
        }
    }
}

impl ExprLValue {
    pub fn id(&self) -> usize {
        match self {
            ExprLValue::Var(x) => x.id,
            ExprLValue::Deref(x) => x.id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprIntLit {
    pub value: i64,
    pub value_loc: Loc,
    pub id: usize,
}

impl Locatable for ExprIntLit {
    fn loc(&self) -> &Loc {
        &self.value_loc
    }
}

#[derive(Debug, Clone)]
pub struct ExprNeg {
    pub minus_loc: Loc,
    pub expr: Box<Expr>,
    pub id: usize,
}

impl Locatable for ExprNeg {
    fn loc(&self) -> &Loc {
        &self.minus_loc
    }
}

#[derive(Debug, Clone)]
pub struct LValueVar {
    pub ident: String,
    pub ident_loc: Loc,
    pub id: usize,
}

impl Locatable for LValueVar {
    fn loc(&self) -> &Loc {
        &self.ident_loc
    }
}

#[derive(Debug, Clone)]
pub struct LValueDeref {
    pub star_loc: Loc,
    pub expr: Box<Expr>,
    pub id: usize,
}

impl Locatable for LValueDeref {
    fn loc(&self) -> &Loc {
        &self.star_loc
    }
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub ident: String,
    pub ident_loc: Loc,
    pub args: Vec<Expr>,
    pub id: usize,
}

impl Locatable for ExprCall {
    fn loc(&self) -> &Loc {
        &self.ident_loc
    }
}

#[derive(Debug, Clone)]
pub struct ExprAddr {
    pub amp_loc: Loc,
    pub expr: Box<Expr>,
    pub id: usize,
}

impl Locatable for ExprAddr {
    fn loc(&self) -> &Loc {
        &self.amp_loc
    }
}

#[derive(Debug, Clone)]
pub struct ExprDeref {
    pub star_loc: Loc,
    pub expr: Box<Expr>,
    pub id: usize,
}

impl Locatable for ExprDeref {
    fn loc(&self) -> &Loc {
        &self.star_loc
    }
}
