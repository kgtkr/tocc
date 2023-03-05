use crate::Bit;

#[derive(Debug, Clone)]
pub struct Local {
    pub bit: Bit,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub payload: DeclPayload,
}

#[derive(Debug, Clone)]
pub enum DeclPayload {
    Func(DeclFunc),
}

#[derive(Debug, Clone)]
pub struct DeclFunc {
    pub name: String,
    // localsの先頭args_countに引数が入る
    pub args_count: usize,
    pub locals: Vec<Local>,
    pub instrs: Vec<Instr>,
}

#[derive(Debug, Clone)]
pub struct Instr {
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
    pub name: String,
    pub args: Vec<usize>,
}
