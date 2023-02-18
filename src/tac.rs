use crate::loc::Loc;

#[derive(Debug, Clone)]
pub enum Type {
    Int,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Int => 4,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Local {
    pub ty: Type,
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
    pub name: String,
    pub locals: Vec<Local>,
    pub instrs: Vec<Instr>,
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub loc: Loc,
    pub payload: InstrPayload,
}

#[derive(Debug, Clone)]
pub enum InstrPayload {
    Return(InstrReturn),
    IntConst(InstrIntConst),
    Add(InstrAdd),
    Sub(InstrSub),
    Mul(InstrMul),
    Div(InstrDiv),
    Neg(InstrNeg),
    Eq(InstrEq),
    Ne(InstrNe),
    Lt(InstrLt),
    Le(InstrLe),
}

#[derive(Debug, Clone)]
pub struct InstrReturn {
    pub src: usize,
}

#[derive(Debug, Clone)]
pub struct InstrIntConst {
    pub dst: usize,
    pub value: i64,
}

#[derive(Debug, Clone)]
pub struct InstrAdd {
    pub dst: usize,
    pub lhs: usize,
    pub rhs: usize,
}

#[derive(Debug, Clone)]
pub struct InstrSub {
    pub dst: usize,
    pub lhs: usize,
    pub rhs: usize,
}

#[derive(Debug, Clone)]
pub struct InstrMul {
    pub dst: usize,
    pub lhs: usize,
    pub rhs: usize,
}

#[derive(Debug, Clone)]
pub struct InstrDiv {
    pub dst: usize,
    pub lhs: usize,
    pub rhs: usize,
}

#[derive(Debug, Clone)]
pub struct InstrNeg {
    pub dst: usize,
    pub src: usize,
}

#[derive(Debug, Clone)]
pub struct InstrEq {
    pub dst: usize,
    pub lhs: usize,
    pub rhs: usize,
}

#[derive(Debug, Clone)]
pub struct InstrNe {
    pub dst: usize,
    pub lhs: usize,
    pub rhs: usize,
}

#[derive(Debug, Clone)]
pub struct InstrLt {
    pub dst: usize,
    pub lhs: usize,
    pub rhs: usize,
}

#[derive(Debug, Clone)]
pub struct InstrLe {
    pub dst: usize,
    pub lhs: usize,
    pub rhs: usize,
}
