use std::collections::{HashMap, HashSet};

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
    pub bbs: Vec<BB>,
}

#[derive(Debug, Clone)]
// Basic Block
pub struct BB {
    pub idx: usize,
    pub instrs: Vec<Instr>,
    pub term: BBTerm,
}

impl BB {
    pub fn local_usage(&self) -> LocalUsage {
        let mut usage = self.term.local_usage();
        for instr in &self.instrs {
            usage.merge(&instr.local_usage());
        }
        usage
    }
}

#[derive(Debug, Clone)]
pub struct LocalUsage {
    pub used: HashSet<usize>,
    pub defined: HashSet<usize>,
    pub referenced: HashSet<usize>,
}

impl LocalUsage {
    pub fn merge(&mut self, other: &LocalUsage) {
        self.used.extend(&other.used);
        self.defined.extend(&other.defined);
        self.referenced.extend(&other.referenced);
    }
}

#[derive(Debug, Clone)]
// terminator
pub enum BBTerm {
    Jump {
        idx: usize,
    },
    JumpIf {
        cond: usize,
        then_idx: usize,
        else_idx: usize,
    },
    Return {
        src: usize,
    },
}

impl BBTerm {
    pub fn dummy() -> Self {
        BBTerm::Jump { idx: 0 }
    }

    pub fn local_usage(&self) -> LocalUsage {
        match self {
            BBTerm::Jump { .. } => LocalUsage {
                used: HashSet::new(),
                defined: HashSet::new(),
                referenced: HashSet::new(),
            },
            BBTerm::JumpIf { cond, .. } => LocalUsage {
                used: HashSet::from([*cond]),
                defined: HashSet::new(),
                referenced: HashSet::new(),
            },
            BBTerm::Return { src } => LocalUsage {
                used: HashSet::from([*src]),
                defined: HashSet::new(),
                referenced: HashSet::new(),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    IntConst(InstrIntConst),
    BinOp(InstrBinOp),
    UnOp(InstrUnOp),
    AssignIndirect(InstrAssignIndirect),
    Call(InstrCall),
    // AssignIndirectで代用できるが最適化のため
    AssignLocal(InstrAssignLocal),
    Nop,
}

impl Instr {
    pub fn local_usage(&self) -> LocalUsage {
        match self {
            Instr::IntConst(_) => LocalUsage {
                used: HashSet::new(),
                defined: HashSet::new(),
                referenced: HashSet::new(),
            },
            Instr::BinOp(InstrBinOp { lhs, rhs, dst, .. }) => LocalUsage {
                used: HashSet::from([*lhs, *rhs]),
                defined: HashSet::from([*dst]),
                referenced: HashSet::new(),
            },
            Instr::UnOp(InstrUnOp { src, dst, op, .. }) => match op {
                UnOp::LocalAddr => LocalUsage {
                    used: HashSet::new(),
                    defined: HashSet::from([*dst]),
                    referenced: HashSet::from([*src]),
                },
                UnOp::Deref => LocalUsage {
                    used: HashSet::from([*src]),
                    defined: HashSet::from([*dst]),
                    referenced: HashSet::new(),
                },
                UnOp::Neg => LocalUsage {
                    used: HashSet::from([*src]),
                    defined: HashSet::from([*dst]),
                    referenced: HashSet::new(),
                },
            },
            Instr::AssignIndirect(InstrAssignIndirect { dst_ref, src }) => LocalUsage {
                used: HashSet::from([*dst_ref, *src]),
                defined: HashSet::new(),
                referenced: HashSet::new(),
            },
            Instr::Call(InstrCall { args, .. }) => LocalUsage {
                used: args.iter().copied().collect(),
                defined: HashSet::new(),
                referenced: HashSet::new(),
            },
            Instr::AssignLocal(InstrAssignLocal { dst, src }) => LocalUsage {
                used: HashSet::from([*src]),
                defined: HashSet::from([*dst]),
                referenced: HashSet::new(),
            },
            Instr::Nop => LocalUsage {
                used: HashSet::new(),
                defined: HashSet::new(),
                referenced: HashSet::new(),
            },
        }
    }
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

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Neg,
    LocalAddr,
    Deref,
}

#[derive(Debug, Clone)]
pub struct InstrUnOp {
    pub dst: usize,
    pub op: UnOp,
    pub src: usize,
}

#[derive(Debug, Clone)]
pub struct InstrAssignIndirect {
    pub dst_ref: usize,
    pub src: usize,
}

#[derive(Debug, Clone)]
pub struct InstrCall {
    pub dst: usize,
    pub ident: String,
    pub args: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct InstrAssignLocal {
    pub dst: usize,
    pub src: usize,
}

pub fn optimize(prog: &mut Program) {
    for decl in &mut prog.decls {
        match decl {
            Decl::Func(decl) => {
                for bb in &mut decl.bbs {
                    // AssignIndirect -> AssignLocal
                    for i in 0..bb.instrs.len().checked_sub(1).unwrap_or(0) {
                        // TODO: LocalAddrとAssignIndirectが連続して並んでるとは限らないので動かない
                        match (&bb.instrs[i], &bb.instrs[i + 1]) {
                            (
                                Instr::UnOp(InstrUnOp {
                                    op: UnOp::LocalAddr,
                                    src: src1,
                                    dst: dst1,
                                }),
                                Instr::AssignIndirect(InstrAssignIndirect {
                                    src: src2,
                                    dst_ref: dst2,
                                }),
                            ) if dst1 == dst2 => {
                                bb.instrs[i] = Instr::AssignLocal(InstrAssignLocal {
                                    dst: *src1,
                                    src: *src2,
                                });
                                bb.instrs[i + 1] = Instr::Nop;
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }
}
