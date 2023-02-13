use crate::clang::{
    self, DeclPayload, Expr, ExprIntLit, ExprPayload, Program, Stmt, StmtCompound, StmtExpr,
    StmtPayload, StmtReturn,
};
use crate::loc::Loc;
use crate::tac;
#[derive(Debug)]
struct InstrGenerator {
    locals_count: usize,
    instrs: Vec<tac::Instr>,
}

impl InstrGenerator {
    fn new() -> InstrGenerator {
        InstrGenerator {
            locals_count: 0,
            instrs: Vec::new(),
        }
    }

    fn generate_local(&mut self) -> usize {
        let local = self.locals_count;
        self.locals_count += 1;
        local
    }

    fn stmt(&mut self, stmt: Stmt) {
        use StmtPayload::*;
        match stmt.payload {
            Expr(x) => {
                self.stmt_expr(x);
            }
            Return(x) => {
                self.stmt_return(stmt.loc, x);
            }
            Compound(x) => self.stmt_compound(x),
        }
    }

    fn stmt_expr(&mut self, x: StmtExpr) {
        self.expr(x.expr);
    }

    fn stmt_return(&mut self, loc: Loc, x: StmtReturn) {
        let src = self.expr(x.expr);
        self.instrs.push(tac::Instr {
            loc,
            payload: tac::InstrPayload::Return(tac::InstrReturn { src }),
        });
    }

    fn stmt_compound(&mut self, x: StmtCompound) {
        for stmt in x.stmts {
            self.stmt(stmt);
        }
    }

    fn expr(&mut self, expr: Expr) -> usize {
        use ExprPayload::*;
        match expr.payload {
            IntLit(x) => self.expr_int_lit(expr.loc, x),
            Add(x) => self.expr_add(expr.loc, x),
            Sub(x) => self.expr_sub(expr.loc, x),
            Mul(x) => self.expr_mul(expr.loc, x),
            Div(x) => self.expr_div(expr.loc, x),
        }
    }

    fn expr_int_lit(&mut self, loc: Loc, x: ExprIntLit) -> usize {
        let dst = self.generate_local();
        self.instrs.push(tac::Instr {
            loc,
            payload: tac::InstrPayload::IntConst(tac::InstrIntConst {
                dst,
                value: x.value,
            }),
        });
        dst
    }

    fn expr_add(&mut self, loc: Loc, x: clang::ExprAdd) -> usize {
        let dst = self.generate_local();
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            loc,
            payload: tac::InstrPayload::Add(tac::InstrAdd { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_sub(&mut self, loc: Loc, x: clang::ExprSub) -> usize {
        let dst = self.generate_local();
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            loc,
            payload: tac::InstrPayload::Sub(tac::InstrSub { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_mul(&mut self, loc: Loc, x: clang::ExprMul) -> usize {
        let dst = self.generate_local();
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            loc,
            payload: tac::InstrPayload::Mul(tac::InstrMul { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_div(&mut self, loc: Loc, x: clang::ExprDiv) -> usize {
        let dst = self.generate_local();
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            loc,
            payload: tac::InstrPayload::Div(tac::InstrDiv { dst, lhs, rhs }),
        });
        dst
    }
}

pub fn generate(program: Program) -> tac::Program {
    let decls = program
        .decls
        .into_iter()
        .map(|decl| {
            use DeclPayload::*;
            match decl.payload {
                Func(x) => {
                    let mut gen = InstrGenerator::new();
                    gen.stmt_compound(x.body);
                    tac::Decl {
                        loc: decl.loc,
                        payload: tac::DeclPayload::Func(tac::DeclFunc {
                            name: x.name,
                            locals_count: gen.locals_count,
                            instrs: gen.instrs,
                        }),
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    tac::Program { decls }
}
