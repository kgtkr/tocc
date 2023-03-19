use crate::Bit;
use derive_more::Unwrap;

#[derive(Debug, Clone, Unwrap)]
pub enum Type {
    Int(TypeInt),
    Ptr(TypePtr),
}

impl Type {
    pub fn to_bit(&self) -> Bit {
        match self {
            Type::Int(_) => Bit::Bit32,
            Type::Ptr(_) => Bit::Bit64,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeInt {}

#[derive(Debug, Clone)]
pub struct TypePtr {
    pub typ: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct Local {
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Func(DeclFunc),
}

#[derive(Debug, Clone)]
pub struct DeclFunc {
    pub ident: String,
    // localsの先頭args_countに引数が入る
    pub args_count: usize,
    pub locals: Vec<Local>,
    pub instrs: Vec<Instr>,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Return(InstrReturn),
    IntConst(InstrIntConst),
    BinOp(InstrBinOp),
    Neg(InstrNeg),
    LocalAddr(InstrLocalAddr),
    Deref(InstrDeref),
    AssignIndirect(InstrAssignIndirect),
    Label(InstrLabel),
    Jump(InstrJump),
    JumpIf(InstrJumpIf),
    JumpIfNot(InstrJumpIfNot),
    Call(InstrCall),
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
}

#[derive(Debug, Clone)]
pub struct InstrBinOp {
    pub dst: usize,
    pub op: BinOp,
    pub lhs: usize,
    pub rhs: usize,
}

#[derive(Debug, Clone)]
pub struct InstrNeg {
    pub dst: usize,
    pub src: usize,
}

#[derive(Debug, Clone)]
pub struct InstrLocalAddr {
    pub dst: usize,
    pub src: usize,
}

#[derive(Debug, Clone)]
pub struct InstrDeref {
    pub dst: usize,
    pub src: usize,
}

#[derive(Debug, Clone)]
pub struct InstrAssignIndirect {
    pub dst: usize,
    pub src: usize,
}

#[derive(Debug, Clone)]
pub struct InstrLabel {
    pub label: usize,
}

#[derive(Debug, Clone)]
pub struct InstrJump {
    pub label: usize,
}

#[derive(Debug, Clone)]
pub struct InstrJumpIf {
    pub label: usize,
    pub cond: usize,
}

#[derive(Debug, Clone)]
pub struct InstrJumpIfNot {
    pub label: usize,
    pub cond: usize,
}

#[derive(Debug, Clone)]
pub struct InstrCall {
    pub dst: usize,
    pub ident: String,
    pub args: Vec<usize>,
}
