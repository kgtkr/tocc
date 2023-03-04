use std::collections::HashMap;

use crate::clang::{
    self, DeclPayload, Expr, ExprIntLit, ExprPayload, Program, Stmt, StmtCompound, StmtExpr,
    StmtIf, StmtPayload, StmtReturn, StmtVarDecl, Type,
};
use crate::{tac, Bit};
#[derive(Debug)]
struct InstrGenerator {
    locals: Vec<tac::Local>,
    instrs: Vec<tac::Instr>,
    local_idents: HashMap<String, usize>,
    label_count: usize,
}

impl InstrGenerator {
    fn new() -> InstrGenerator {
        InstrGenerator {
            locals: Vec::new(),
            instrs: Vec::new(),
            local_idents: HashMap::new(),
            label_count: 0,
        }
    }

    fn generate_local(&mut self, bit: Bit) -> usize {
        let local = self.locals.len();
        self.locals.push(tac::Local { bit });
        local
    }

    fn generate_label(&mut self) -> usize {
        let label = self.label_count;
        self.label_count += 1;
        label
    }

    fn stmt(&mut self, stmt: Stmt) {
        use StmtPayload::*;
        match stmt.payload {
            Expr(x) => {
                self.stmt_expr(x);
            }
            Return(x) => {
                self.stmt_return(x);
            }
            Compound(x) => self.stmt_compound(x),
            VarDecl(x) => self.stmt_var_decl(x),
            If(x) => self.stmt_if(x),
            While(x) => self.stmt_while(x),
            For(x) => self.stmt_for(x),
        }
    }

    fn stmt_expr(&mut self, x: StmtExpr) {
        self.expr(x.expr);
    }

    fn stmt_return(&mut self, x: StmtReturn) {
        let src = self.expr(x.expr);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Return(tac::InstrReturn { src }),
        });
    }

    fn stmt_compound(&mut self, x: StmtCompound) {
        for stmt in x.stmts {
            self.stmt(stmt);
        }
    }

    fn stmt_var_decl(&mut self, x: StmtVarDecl) {
        let local = self.generate_local(Bit::Bit32);
        self.local_idents.insert(x.name, local);
    }

    fn stmt_if(&mut self, x: StmtIf) {
        let else_label = self.generate_label();
        let end_label = self.generate_label();

        let cond = self.expr(x.cond.clone());

        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::JumpIfNot(tac::InstrJumpIfNot {
                cond: cond,
                label: else_label,
            }),
        });

        self.stmt(*x.then);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Jump(tac::InstrJump { label: end_label }),
        });
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Label(tac::InstrLabel { label: else_label }),
        });
        if let Some(else_) = x.else_ {
            self.stmt(*else_);
        }
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Label(tac::InstrLabel { label: end_label }),
        });
    }

    fn stmt_while(&mut self, x: clang::StmtWhile) {
        let cond_label = self.generate_label();
        let end_label = self.generate_label();

        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Label(tac::InstrLabel { label: cond_label }),
        });
        let cond = self.expr(x.cond.clone());
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::JumpIfNot(tac::InstrJumpIfNot {
                cond: cond,
                label: end_label,
            }),
        });
        self.stmt(*x.body);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Jump(tac::InstrJump { label: cond_label }),
        });
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Label(tac::InstrLabel { label: end_label }),
        });
    }

    fn stmt_for(&mut self, x: clang::StmtFor) {
        let cond_label = self.generate_label();
        let end_label = self.generate_label();

        if let Some(init) = x.init {
            self.expr(init);
        }
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Label(tac::InstrLabel { label: cond_label }),
        });
        if let Some(cond) = x.cond {
            let cond = self.expr(cond);
            self.instrs.push(tac::Instr {
                payload: tac::InstrPayload::JumpIfNot(tac::InstrJumpIfNot {
                    cond: cond,
                    label: end_label,
                }),
            });
        }
        self.stmt(*x.body.clone());
        if let Some(step) = x.step {
            self.expr(step);
        }
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Jump(tac::InstrJump { label: cond_label }),
        });
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Label(tac::InstrLabel { label: end_label }),
        });
    }

    fn expr(&mut self, expr: Expr) -> usize {
        use ExprPayload::*;
        match expr.payload {
            IntLit(x) => self.expr_int_lit(x),
            Add(x) => self.expr_add(x),
            Sub(x) => self.expr_sub(x),
            Mul(x) => self.expr_mul(x),
            Div(x) => self.expr_div(x),
            Neg(x) => self.expr_neg(x),
            Eq(x) => self.expr_eq(x),
            Ne(x) => self.expr_ne(x),
            Lt(x) => self.expr_lt(x),
            Le(x) => self.expr_le(x),
            Gt(x) => self.expr_gt(x),
            Ge(x) => self.expr_ge(x),
            LValue(x) => self.expr_lvalue(x),
            Assign(x) => self.expr_assign(x),
            Call(x) => self.expr_call(x),
        }
    }

    fn expr_int_lit(&mut self, x: ExprIntLit) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::IntConst(tac::InstrIntConst {
                dst,
                value: x.value,
            }),
        });
        dst
    }

    fn expr_add(&mut self, x: clang::ExprAdd) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Add(tac::InstrAdd { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_sub(&mut self, x: clang::ExprSub) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Sub(tac::InstrSub { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_mul(&mut self, x: clang::ExprMul) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Mul(tac::InstrMul { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_div(&mut self, x: clang::ExprDiv) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Div(tac::InstrDiv { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_neg(&mut self, x: clang::ExprNeg) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let src = self.expr(*x.expr);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Neg(tac::InstrNeg { dst, src }),
        });
        dst
    }

    fn expr_eq(&mut self, x: clang::ExprEq) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Eq(tac::InstrEq { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_ne(&mut self, x: clang::ExprNe) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Ne(tac::InstrNe { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_lt(&mut self, x: clang::ExprLt) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Lt(tac::InstrLt { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_le(&mut self, x: clang::ExprLe) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Le(tac::InstrLe { dst, lhs, rhs }),
        });
        dst
    }

    fn expr_gt(&mut self, x: clang::ExprGt) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Lt(tac::InstrLt {
                dst,
                lhs: rhs,
                rhs: lhs,
            }),
        });
        dst
    }

    fn expr_ge(&mut self, x: clang::ExprGe) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Le(tac::InstrLe {
                dst,
                lhs: rhs,
                rhs: lhs,
            }),
        });
        dst
    }

    fn expr_assign(&mut self, x: clang::ExprAssign) -> usize {
        let clang::ExprPayload::LValue(lvalue) = x.lhs.payload else {
            panic!("expected lvalue");
        };
        let dst = self.lvalue(lvalue);
        let src = self.expr(*x.rhs);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::AssignIndirect(tac::InstrAssignIndirect { dst, src }),
        });
        src
    }

    fn expr_lvalue(&mut self, x: clang::ExprLValue) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let src = self.lvalue(x);
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Deref(tac::InstrDeref { dst, src }),
        });
        dst
    }

    fn expr_call(&mut self, x: clang::ExprCall) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let args = x
            .args
            .into_iter()
            .map(|arg| self.expr(arg))
            .collect::<Vec<_>>();
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::Call(tac::InstrCall {
                dst,
                name: x.name,
                args,
            }),
        });
        dst
    }

    fn lvalue(&mut self, x: clang::ExprLValue) -> usize {
        use clang::ExprLValue::*;
        match x {
            Var(x) => self.lvalue_var(x),
        }
    }

    fn lvalue_var(&mut self, x: clang::LValueVar) -> usize {
        let dst = self.generate_local(Bit::Bit64);
        let src = *self.local_idents.get(&x.name).expect("undeclared variable");
        self.instrs.push(tac::Instr {
            payload: tac::InstrPayload::LocalAddr(tac::InstrLocalAddr { dst, src }),
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
                    gen.local_idents.extend(
                        x.params
                            .iter()
                            .enumerate()
                            .map(|(i, param)| (param.name.clone(), i)),
                    );
                    gen.locals.extend(x.params.iter().map(|param| tac::Local {
                        bit: match param.typ {
                            Type::Int => Bit::Bit32,
                        },
                    }));
                    gen.stmt_compound(x.body);
                    tac::Decl {
                        payload: tac::DeclPayload::Func(tac::DeclFunc {
                            name: x.name,
                            args_count: x.params.len(),
                            locals: gen.locals,
                            instrs: gen.instrs,
                        }),
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    tac::Program { decls }
}
