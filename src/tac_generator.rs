use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::mem;

use crate::clang::{
    self, BinOp, Decl, DeclFunc, Expr, ExprBinOp, ExprIntLit, FuncSig, Program, Stmt, StmtCompound,
    StmtExpr, StmtIf, StmtReturn, StmtVarDecl, Type, TypePtr,
};
use crate::loc::{Loc, Locatable};
use crate::tac::{self, BBId};
use crate::Bit;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
#[error("{loc}: {message}")]
pub struct CodegenError {
    loc: Loc,
    message: String,
}

fn convert_type(typ: &Type) -> tac::Type {
    match typ {
        Type::Int(_) => tac::Type::Int(Bit::Bit32),
        Type::Ptr(_) => tac::Type::Int(Bit::Bit64),
    }
}

#[derive(Debug)]
struct FuncGenerator<'a> {
    locals: Vec<tac::Local>,
    instrs: Vec<tac::Instr>,
    local_idents: HashMap<String, usize>,
    bbs: Vec<tac::BB>,
    expr_types: &'a HashMap<usize, Type>,
}

impl<'a> FuncGenerator<'a> {
    fn gen(
        expr_types: &'a HashMap<usize, Type>,
        func: clang::DeclFunc,
    ) -> Result<tac::Func, CodegenError> {
        let mut gen = FuncGenerator {
            locals: Vec::new(),
            instrs: Vec::new(),
            local_idents: HashMap::new(),
            bbs: Vec::new(),
            expr_types,
        };
        for (idx, param) in func.sig.params.iter().enumerate() {
            let arg = gen.add_named_local(
                param.ident.clone(),
                &param.ident_loc,
                tac::Type::Int(Bit::Bit32),
            )?;
            gen.instrs
                .push(tac::Instr::SetArg(tac::InstrSetArg { dst: arg, idx }));
        }
        gen.stmt_compound(
            func.body.unwrap(), /* TODO: 型自体を宣言と定義で変えたい */
        )?;
        let entry = gen.bbs[0].id;
        Ok(tac::Func {
            ident: func.sig.ident,
            args_count: func.sig.params.len(),
            locals: gen.locals,
            bbs: gen.bbs,
            entry,
        })
    }

    fn add_named_local(
        &mut self,
        ident: String,
        ident_loc: &Loc,
        typ: tac::Type,
    ) -> Result<usize, CodegenError> {
        let local = self.generate_local(typ);
        if let Entry::Vacant(entry) = self.local_idents.entry(ident.clone()) {
            entry.insert(local);
            Ok(local)
        } else {
            Err(CodegenError {
                loc: ident_loc.clone(),
                message: format!("local variable {} is already defined", ident),
            })
        }
    }

    fn generate_local(&mut self, typ: tac::Type) -> usize {
        let local = self.locals.len();
        self.locals.push(tac::Local { typ, reg: None });
        local
    }

    fn new_bb(&mut self, term: tac::InstrTerm) -> usize {
        let idx = self.bbs.len();
        let mut instrs = mem::take(&mut self.instrs);
        instrs.push(tac::Instr::Term(term));
        self.bbs.push(tac::BB {
            id: BBId::from(idx),
            instrs,
        });
        idx
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), CodegenError> {
        use Stmt::*;
        match stmt {
            Expr(x) => {
                self.stmt_expr(x)?;
            }
            Return(x) => {
                self.stmt_return(x)?;
            }
            Compound(x) => self.stmt_compound(x)?,
            VarDecl(x) => self.stmt_var_decl(x)?,
            If(x) => self.stmt_if(x)?,
            While(x) => self.stmt_while(x)?,
            For(x) => self.stmt_for(x)?,
        }
        Ok(())
    }

    fn stmt_expr(&mut self, x: StmtExpr) -> Result<usize, CodegenError> {
        self.expr(x.expr)
    }

    fn stmt_return(&mut self, x: StmtReturn) -> Result<(), CodegenError> {
        let src = self.expr(x.expr)?;
        self.new_bb(tac::InstrTerm::Return { src });
        Ok(())
    }

    fn stmt_compound(&mut self, x: StmtCompound) -> Result<(), CodegenError> {
        let prev_local_idents = self.local_idents.clone();
        for stmt in x.stmts {
            self.stmt(stmt)?;
        }
        self.local_idents = prev_local_idents;
        Ok(())
    }

    fn stmt_var_decl(&mut self, x: StmtVarDecl) -> Result<(), CodegenError> {
        self.add_named_local(x.ident, &x.ident_loc, convert_type(&x.typ))?;
        Ok(())
    }

    fn stmt_if(&mut self, x: StmtIf) -> Result<(), CodegenError> {
        let cond = self.expr(x.cond.clone())?;
        let cond_dummy_bb_idx = self.new_bb(tac::InstrTerm::dummy());

        let then_bb_idx = self.bbs.len();
        self.stmt(*x.then)?;
        let then_dummy_bb_idx = self.new_bb(tac::InstrTerm::dummy());

        let else_bb_idx = self.bbs.len();
        if let Some(else_) = x.else_ {
            self.stmt(*else_)?;
        }
        let else_dummy_bb_idx = self.new_bb(tac::InstrTerm::dummy());
        let next_bb_idx = self.bbs.len();

        *self.bbs[cond_dummy_bb_idx].term_mut() = tac::InstrTerm::JumpIf {
            cond,
            then_id: BBId::from(then_bb_idx),
            else_id: BBId::from(else_bb_idx),
        };
        *self.bbs[then_dummy_bb_idx].term_mut() = tac::InstrTerm::Jump {
            id: BBId::from(next_bb_idx),
        };
        *self.bbs[else_dummy_bb_idx].term_mut() = tac::InstrTerm::Jump {
            id: BBId::from(next_bb_idx),
        };

        Ok(())
    }

    fn stmt_while(&mut self, x: clang::StmtWhile) -> Result<(), CodegenError> {
        let start_dummy_bb_idx = self.new_bb(tac::InstrTerm::dummy());

        let cond_bb_idx = self.bbs.len();
        let cond = self.expr(x.cond.clone())?;
        let cond_dummy_bb_idx = self.new_bb(tac::InstrTerm::dummy());

        let body_bb_idx = self.bbs.len();
        self.stmt(*x.body)?;
        let body_dummy_bb_idx = self.new_bb(tac::InstrTerm::dummy());

        let next_bb_idx = self.bbs.len();

        *self.bbs[start_dummy_bb_idx].term_mut() = tac::InstrTerm::Jump {
            id: BBId::from(cond_bb_idx),
        };
        *self.bbs[cond_dummy_bb_idx].term_mut() = tac::InstrTerm::JumpIf {
            cond,
            then_id: BBId::from(body_bb_idx),
            else_id: BBId::from(next_bb_idx),
        };
        *self.bbs[body_dummy_bb_idx].term_mut() = tac::InstrTerm::Jump {
            id: BBId::from(cond_bb_idx),
        };
        Ok(())
    }

    fn stmt_for(&mut self, x: clang::StmtFor) -> Result<(), CodegenError> {
        if let Some(init) = x.init {
            self.expr(init)?;
        }
        let start_dummy_bb_idx = self.new_bb(tac::InstrTerm::dummy());

        let cond_bb_idx = self.bbs.len();
        let cond = if let Some(cond) = x.cond {
            self.expr(cond)?
        } else {
            let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
            self.instrs
                .push(tac::Instr::IntConst(tac::InstrIntConst { dst, value: 1 }));
            dst
        };
        let cond_dummy_bb_idx = self.new_bb(tac::InstrTerm::dummy());

        let body_bb_idx = self.bbs.len();
        self.stmt(*x.body.clone())?;
        if let Some(step) = x.step {
            self.expr(step)?;
        }
        let body_dummy_bb_idx = self.new_bb(tac::InstrTerm::dummy());

        let next_bb_idx = self.bbs.len();

        *self.bbs[start_dummy_bb_idx].term_mut() = tac::InstrTerm::Jump {
            id: BBId::from(cond_bb_idx),
        };
        *self.bbs[cond_dummy_bb_idx].term_mut() = tac::InstrTerm::JumpIf {
            cond,
            then_id: BBId::from(body_bb_idx),
            else_id: BBId::from(next_bb_idx),
        };
        *self.bbs[body_dummy_bb_idx].term_mut() = tac::InstrTerm::Jump {
            id: BBId::from(cond_bb_idx),
        };
        Ok(())
    }

    fn expr(&mut self, expr: Expr) -> Result<usize, CodegenError> {
        use Expr::*;
        match expr {
            IntLit(x) => self.expr_int_lit(x),
            BinOp(x) => self.expr_bin_op(x),
            Neg(x) => self.expr_neg(x),
            LValue(x) => self.expr_lvalue(x),
            Call(x) => self.expr_call(x),
            Addr(x) => self.expr_addr(x),
        }
    }

    fn expr_int_lit(&mut self, x: ExprIntLit) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        self.instrs.push(tac::Instr::IntConst(tac::InstrIntConst {
            dst,
            value: x.value,
        }));
        Ok(dst)
    }

    fn expr_bin_op(&mut self, x: ExprBinOp) -> Result<usize, CodegenError> {
        match x.op {
            BinOp::Add => self.bin_op_add(*x.lhs, *x.rhs, x.op_loc),
            BinOp::Sub => self.bin_op_sub(*x.lhs, *x.rhs, x.op_loc),
            BinOp::Mul => self.bin_op_mul(*x.lhs, *x.rhs),
            BinOp::Div => self.bin_op_div(*x.lhs, *x.rhs),
            BinOp::Eq => self.bin_op_eq(*x.lhs, *x.rhs),
            BinOp::Ne => self.bin_op_ne(*x.lhs, *x.rhs),
            BinOp::Lt => self.bin_op_lt(*x.lhs, *x.rhs),
            BinOp::Le => self.bin_op_le(*x.lhs, *x.rhs),
            BinOp::Gt => self.bin_op_gt(*x.lhs, *x.rhs),
            BinOp::Ge => self.bin_op_ge(*x.lhs, *x.rhs),
            BinOp::Assign => self.bin_op_assign(*x.lhs, *x.rhs),
        }
    }

    fn bin_op_add(&mut self, lhs: Expr, rhs: Expr, op_loc: Loc) -> Result<usize, CodegenError> {
        let l_typ = &self.expr_types[&lhs.id()];
        let r_typ = &self.expr_types[&rhs.id()];

        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;

        match (l_typ, r_typ) {
            (Type::Int(_), Type::Int(_)) => {
                let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst,
                    lhs,
                    rhs,
                    op: tac::BinOp::Add,
                }));
                Ok(dst)
            }
            (Type::Ptr(TypePtr { typ: l_inner_typ }), Type::Int(_)) => {
                let ptr_inner_size = self.generate_local(tac::Type::Int(Bit::Bit32));
                let add_val = self.generate_local(tac::Type::Int(Bit::Bit32));
                let dst = self.generate_local(tac::Type::Int(Bit::Bit64));
                self.instrs.push(tac::Instr::IntConst(tac::InstrIntConst {
                    dst: ptr_inner_size,
                    value: convert_type(l_inner_typ.as_ref()).to_bit().to_size() as i64,
                }));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst: add_val,
                    lhs: rhs,
                    rhs: ptr_inner_size,
                    op: tac::BinOp::Mul,
                }));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst,
                    lhs,
                    rhs: add_val,
                    op: tac::BinOp::Add,
                }));
                Ok(dst)
            }
            (Type::Int(_), Type::Ptr(TypePtr { typ: r_inner_typ })) => {
                let ptr_inner_size = self.generate_local(tac::Type::Int(Bit::Bit32));
                let add_val = self.generate_local(tac::Type::Int(Bit::Bit32));
                let dst = self.generate_local(tac::Type::Int(Bit::Bit64));
                self.instrs.push(tac::Instr::IntConst(tac::InstrIntConst {
                    dst: ptr_inner_size,
                    value: convert_type(r_inner_typ.as_ref()).to_bit().to_size() as i64,
                }));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst: add_val,
                    lhs,
                    rhs: ptr_inner_size,
                    op: tac::BinOp::Mul,
                }));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst,
                    lhs: add_val,
                    rhs,
                    op: tac::BinOp::Add,
                }));
                Ok(dst)
            }
            (_, _) => Err(CodegenError {
                loc: op_loc,
                message: "invalid types for add".to_string(), // TODO: better error message
            }),
        }
    }

    fn bin_op_sub(&mut self, lhs: Expr, rhs: Expr, op_loc: Loc) -> Result<usize, CodegenError> {
        let l_typ = &self.expr_types[&lhs.id()];
        let r_typ = &self.expr_types[&rhs.id()];
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;

        match (l_typ, r_typ) {
            (Type::Int(_), Type::Int(_)) => {
                let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst,
                    lhs,
                    rhs,
                    op: tac::BinOp::Sub,
                }));
                Ok(dst)
            }
            (Type::Ptr(TypePtr { typ: l_inner_typ }), Type::Int(_)) => {
                let ptr_inner_size = self.generate_local(tac::Type::Int(Bit::Bit32));
                let add_val = self.generate_local(tac::Type::Int(Bit::Bit32));
                let dst = self.generate_local(tac::Type::Int(Bit::Bit64));
                self.instrs.push(tac::Instr::IntConst(tac::InstrIntConst {
                    dst: ptr_inner_size,
                    value: convert_type(l_inner_typ.as_ref()).to_bit().to_size() as i64,
                }));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst: add_val,
                    lhs: rhs,
                    rhs: ptr_inner_size,
                    op: tac::BinOp::Mul,
                }));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst,
                    lhs,
                    rhs: add_val,
                    op: tac::BinOp::Sub,
                }));
                Ok(dst)
            }
            (Type::Ptr(TypePtr { typ: l_inner_typ }), Type::Ptr(TypePtr { typ: r_inner_typ })) => {
                let sub_val = self.generate_local(tac::Type::Int(Bit::Bit32));
                let ptr_inner_size = self.generate_local(tac::Type::Int(Bit::Bit32));
                let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst: sub_val,
                    lhs,
                    rhs,
                    op: tac::BinOp::Sub,
                }));
                self.instrs.push(tac::Instr::IntConst(tac::InstrIntConst {
                    dst: ptr_inner_size,
                    value: convert_type(l_inner_typ.as_ref()).to_bit().to_size() as i64,
                }));
                self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
                    dst,
                    lhs: sub_val,
                    rhs: ptr_inner_size,
                    op: tac::BinOp::Div,
                }));
                Ok(dst)
            }
            (_, _) => Err(CodegenError {
                loc: op_loc,
                message: "invalid types for sub".to_string(), // TODO: better error message
            }),
        }
    }

    fn bin_op_mul(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs,
            rhs,
            op: tac::BinOp::Mul,
        }));
        Ok(dst)
    }

    fn bin_op_div(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs,
            rhs,
            op: tac::BinOp::Div,
        }));
        Ok(dst)
    }

    fn bin_op_eq(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs,
            rhs,
            op: tac::BinOp::Eq,
        }));
        Ok(dst)
    }

    fn bin_op_ne(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs,
            rhs,
            op: tac::BinOp::Ne,
        }));
        Ok(dst)
    }

    fn bin_op_lt(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs,
            rhs,
            op: tac::BinOp::Lt,
        }));
        Ok(dst)
    }

    fn bin_op_le(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs,
            rhs,
            op: tac::BinOp::Le,
        }));
        Ok(dst)
    }

    fn bin_op_gt(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs: rhs,
            rhs: lhs,
            op: tac::BinOp::Lt,
        }));
        Ok(dst)
    }

    fn bin_op_ge(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs: rhs,
            rhs: lhs,
            op: tac::BinOp::Le,
        }));
        Ok(dst)
    }

    fn bin_op_assign(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let clang::Expr::LValue(lvalue) = lhs else {
            return Err(CodegenError {
                loc: lhs.loc().clone(),
                message: "expected lvalue".to_string(),
            });
        };
        if let clang::ExprLValue::Var(var) = lvalue {
            // TODO: 本当はこの分岐なしで後から最適化したいが現状の仕組みでは難しいのでいったん
            let src = self.expr(rhs)?;
            let dst = *self
                .local_idents
                .get(&var.ident)
                .ok_or_else(|| CodegenError {
                    loc: var.ident_loc.clone(),
                    message: format!("undeclared variable `{}`", var.ident),
                })?;
            self.instrs
                .push(tac::Instr::AssignLocal(tac::InstrAssignLocal { dst, src }));
            Ok(src)
        } else {
            let dst = self.lvalue(lvalue)?;
            let src = self.expr(rhs)?;
            self.instrs
                .push(tac::Instr::AssignIndirect(tac::InstrAssignIndirect {
                    dst_ref: dst,
                    src,
                }));
            Ok(src)
        }
    }

    fn expr_neg(&mut self, x: clang::ExprNeg) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(Bit::Bit32));
        let src = self.expr(*x.expr)?;
        self.instrs.push(tac::Instr::UnOp(tac::InstrUnOp {
            dst,
            src,
            op: tac::UnOp::Neg,
        }));
        Ok(dst)
    }

    fn expr_lvalue(&mut self, x: clang::ExprLValue) -> Result<usize, CodegenError> {
        if let clang::ExprLValue::Var(var) = x {
            // TODO: 本当はこの分岐なしで後から最適化したいが現状の仕組みでは難しいのでいったん
            let dst = *self
                .local_idents
                .get(&var.ident)
                .ok_or_else(|| CodegenError {
                    loc: var.ident_loc.clone(),
                    message: format!("undeclared variable `{}`", var.ident),
                })?;
            Ok(dst)
        } else {
            let src_type = &self.expr_types[&x.id()];
            let src = self.lvalue(x)?;
            let dst = self.generate_local(convert_type(src_type));
            self.instrs.push(tac::Instr::UnOp(tac::InstrUnOp {
                dst,
                src,
                op: tac::UnOp::Deref,
            }));
            Ok(dst)
        }
    }

    fn expr_call(&mut self, x: clang::ExprCall) -> Result<usize, CodegenError> {
        let dst_typ = &self.expr_types[&x.id];
        let dst = self.generate_local(convert_type(dst_typ));
        let args = x
            .args
            .into_iter()
            .map(|arg| self.expr(arg))
            .collect::<Result<Vec<_>, _>>()?;
        self.instrs.push(tac::Instr::Call(tac::InstrCall {
            dst,
            ident: x.ident,
            args,
            save_regs: Vec::new(),
        }));
        Ok(dst)
    }

    fn expr_addr(&mut self, x: clang::ExprAddr) -> Result<usize, CodegenError> {
        let clang::Expr::LValue(lvalue) = *x.expr else {
            return Err(CodegenError {
                loc: x.expr.loc().clone(),
                message: "expected lvalue".to_string(),
            });
        };
        self.lvalue(lvalue)
    }

    fn lvalue(&mut self, x: clang::ExprLValue) -> Result<usize, CodegenError> {
        use clang::ExprLValue::*;
        match x {
            Var(x) => self.lvalue_var(x),
            Deref(x) => self.lvalue_deref(x),
        }
    }

    fn lvalue_var(&mut self, x: clang::LValueVar) -> Result<usize, CodegenError> {
        let src = *self
            .local_idents
            .get(&x.ident)
            .ok_or_else(|| CodegenError {
                loc: x.ident_loc.clone(),
                message: format!("undeclared variable `{}`", x.ident),
            })?;

        let dst = self.generate_local(tac::Type::Int(Bit::Bit64));
        self.instrs.push(tac::Instr::UnOp(tac::InstrUnOp {
            dst,
            src,
            op: tac::UnOp::LocalAddr,
        }));
        Ok(dst)
    }

    fn lvalue_deref(&mut self, x: clang::LValueDeref) -> Result<usize, CodegenError> {
        self.expr(*x.expr)
    }
}

#[derive(Debug)]
pub struct ProgramGenerator<'a> {
    funcs: Vec<tac::Func>,
    func_sigs: HashMap<String, FuncSig>,
    expr_types: &'a HashMap<usize, Type>,
}

impl<'a> ProgramGenerator<'a> {
    fn gen(
        expr_types: &'a HashMap<usize, Type>,
        program: Program,
    ) -> Result<tac::Program, CodegenError> {
        let mut gen = Self {
            funcs: Vec::new(),
            func_sigs: HashMap::new(),
            expr_types,
        };

        for decl in program.decls {
            use Decl::*;
            match decl {
                Func(func) => {
                    gen.func(func)?;
                }
            }
        }

        Ok(tac::Program { funcs: gen.funcs })
    }

    fn func(&mut self, func: DeclFunc) -> Result<(), CodegenError> {
        self.func_sigs
            .insert(func.sig.ident.clone(), func.sig.clone());

        if func.body.is_some() {
            self.funcs.push(FuncGenerator::gen(self.expr_types, func)?);
        }

        Ok(())
    }
}

pub fn generate(
    expr_types: &HashMap<usize, Type>,
    program: Program,
) -> Result<tac::Program, CodegenError> {
    ProgramGenerator::gen(expr_types, program)
}
