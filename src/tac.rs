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
    pub locals_count: usize,
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
