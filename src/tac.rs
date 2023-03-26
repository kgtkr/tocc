use std::collections::HashSet;

use crate::{reg_alloc, Bit};
use derive_more::{From, Into, Unwrap};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Into, From)]
pub struct BBId(usize);

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
    // 0～6
    pub reg: Option<usize>,
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
    pub bbs: Vec<BB>, // id順ではなく"適切な"順序で並んでいる場合がある。非空(少なくともreturn文があるため)
    pub entry: BBId,
}

#[derive(Debug, Clone)]
// Basic Block
pub struct BB {
    pub id: BBId,
    // Termで終わる
    pub instrs: Vec<Instr>,
}

impl BB {
    pub fn local_usage(&self) -> LocalUsage {
        let mut usage = LocalUsage::new();
        for instr in &self.instrs {
            let mut instr_usage = instr.local_usage();
            // 基本ブロック内でkillされている変数はブロック全体のgenに含めない
            instr_usage.gen.retain(|x| !usage.kill.contains(x));
            usage.merge(&instr_usage);
        }
        usage
    }

    pub fn term(&self) -> &InstrTerm {
        match self.instrs.last() {
            Some(Instr::Term(term)) => term,
            _ => panic!("BB does not have a terminator"),
        }
    }

    pub fn term_mut(&mut self) -> &mut InstrTerm {
        match self.instrs.last_mut() {
            Some(Instr::Term(term)) => term,
            _ => panic!("BB does not have a terminator"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalUsage {
    pub gen: HashSet<usize>,
    pub kill: HashSet<usize>,
    pub referenced: HashSet<usize>,
}

impl LocalUsage {
    pub fn new() -> Self {
        LocalUsage {
            gen: HashSet::new(),
            kill: HashSet::new(),
            referenced: HashSet::new(),
        }
    }

    pub fn merge(&mut self, other: &LocalUsage) {
        self.gen.extend(&other.gen);
        self.kill.extend(&other.kill);
        self.referenced.extend(&other.referenced);
    }
}

#[derive(Debug, Clone)]
// terminator
pub enum InstrTerm {
    Jump {
        id: BBId,
    },
    JumpIf {
        cond: usize,
        then_id: BBId,
        else_id: BBId,
    },
    Return {
        src: usize,
    },
}

impl InstrTerm {
    pub fn dummy() -> Self {
        InstrTerm::Jump { id: BBId::from(0) }
    }

    pub fn nexts(&self) -> Vec<BBId> {
        match self {
            InstrTerm::Jump { id } => vec![*id],
            InstrTerm::JumpIf {
                then_id, else_id, ..
            } => vec![*then_id, *else_id],
            InstrTerm::Return { .. } => vec![],
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
    // 最後に必ず入り、途中に入ることはない
    Term(InstrTerm),
    // entry BBの最初でのみ利用可能
    SetArg(InstrSetArg),
    Nop,
}

impl Instr {
    pub fn local_usage(&self) -> LocalUsage {
        match self {
            Instr::IntConst(_) => LocalUsage {
                gen: HashSet::new(),
                kill: HashSet::new(),
                referenced: HashSet::new(),
            },
            Instr::BinOp(InstrBinOp { lhs, rhs, dst, .. }) => LocalUsage {
                gen: HashSet::from([*lhs, *rhs]),
                kill: HashSet::from([*dst]),
                referenced: HashSet::new(),
            },
            Instr::UnOp(InstrUnOp { src, dst, op, .. }) => match op {
                UnOp::LocalAddr => LocalUsage {
                    gen: HashSet::new(),
                    kill: HashSet::from([*dst]),
                    referenced: HashSet::from([*src]),
                },
                UnOp::Deref => LocalUsage {
                    gen: HashSet::from([*src]),
                    kill: HashSet::from([*dst]),
                    referenced: HashSet::new(),
                },
                UnOp::Neg => LocalUsage {
                    gen: HashSet::from([*src]),
                    kill: HashSet::from([*dst]),
                    referenced: HashSet::new(),
                },
            },
            Instr::AssignIndirect(InstrAssignIndirect { dst_ref, src }) => LocalUsage {
                gen: HashSet::from([*dst_ref, *src]),
                kill: HashSet::new(),
                referenced: HashSet::new(),
            },
            Instr::Call(InstrCall { args, .. }) => LocalUsage {
                gen: args.iter().copied().collect(),
                kill: HashSet::new(),
                referenced: HashSet::new(),
            },
            Instr::AssignLocal(InstrAssignLocal { dst, src }) => LocalUsage {
                gen: HashSet::from([*src]),
                kill: HashSet::from([*dst]),
                referenced: HashSet::new(),
            },
            Instr::Nop => LocalUsage {
                gen: HashSet::new(),
                kill: HashSet::new(),
                referenced: HashSet::new(),
            },
            Instr::Term(term) => match term {
                InstrTerm::Jump { .. } => LocalUsage {
                    gen: HashSet::new(),
                    kill: HashSet::new(),
                    referenced: HashSet::new(),
                },
                InstrTerm::JumpIf { cond, .. } => LocalUsage {
                    gen: HashSet::from([*cond]),
                    kill: HashSet::new(),
                    referenced: HashSet::new(),
                },
                InstrTerm::Return { src } => LocalUsage {
                    gen: HashSet::from([*src]),
                    kill: HashSet::new(),
                    referenced: HashSet::new(),
                },
            },
            Instr::SetArg(InstrSetArg { dst, .. }) => LocalUsage {
                gen: HashSet::new(),
                kill: HashSet::from([*dst]),
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
    pub save_regs: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct InstrAssignLocal {
    pub dst: usize,
    pub src: usize,
}

#[derive(Debug, Clone)]
pub struct InstrSetArg {
    pub dst: usize,
    pub idx: usize,
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

                reg_alloc(decl);
            }
        }
    }
}
