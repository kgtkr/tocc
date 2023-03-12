use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::clang::{
    self, Decl, Expr, ExprIntLit, Program, Stmt, StmtCompound, StmtExpr, StmtIf, StmtReturn,
    StmtVarDecl,
};
use crate::loc::Loc;
use crate::{tac, Bit};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
#[error("{loc}: {message}")]
pub struct CodegenError {
    loc: Loc,
    message: String,
}

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

    fn add_named_local(
        &mut self,
        ident: String,
        ident_loc: &Loc,
        bit: Bit,
    ) -> Result<usize, CodegenError> {
        let local = self.generate_local(bit);
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

    fn stmt(&mut self, stmt: Stmt) -> Result<(), CodegenError> {
        use Stmt::*;
        match stmt {
            Expr(x) => {
                self.stmt_expr(x);
            }
            Return(x) => {
                self.stmt_return(x);
            }
            Compound(x) => self.stmt_compound(x)?,
            VarDecl(x) => self.stmt_var_decl(x)?,
            If(x) => self.stmt_if(x)?,
            While(x) => self.stmt_while(x)?,
            For(x) => self.stmt_for(x)?,
        }
        Ok(())
    }

    fn stmt_expr(&mut self, x: StmtExpr) {
        self.expr(x.expr);
    }

    fn stmt_return(&mut self, x: StmtReturn) {
        let src = self.expr(x.expr);
        self.instrs
            .push(tac::Instr::Return(tac::InstrReturn { src }));
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
        let bit = match x.typ {
            clang::Type::Int(_) => Bit::Bit32,
            clang::Type::Ptr(_) => Bit::Bit64,
        };
        self.add_named_local(x.ident, &x.ident_loc, bit)?;
        Ok(())
    }

    fn stmt_if(&mut self, x: StmtIf) -> Result<(), CodegenError> {
        let else_label = self.generate_label();
        let end_label = self.generate_label();

        let cond = self.expr(x.cond.clone());

        self.instrs.push(tac::Instr::JumpIfNot(tac::InstrJumpIfNot {
            cond,
            label: else_label,
        }));

        self.stmt(*x.then)?;
        self.instrs
            .push(tac::Instr::Jump(tac::InstrJump { label: end_label }));
        self.instrs
            .push(tac::Instr::Label(tac::InstrLabel { label: else_label }));
        if let Some(else_) = x.else_ {
            self.stmt(*else_)?;
        }
        self.instrs
            .push(tac::Instr::Label(tac::InstrLabel { label: end_label }));
        Ok(())
    }

    fn stmt_while(&mut self, x: clang::StmtWhile) -> Result<(), CodegenError> {
        let cond_label = self.generate_label();
        let end_label = self.generate_label();

        self.instrs
            .push(tac::Instr::Label(tac::InstrLabel { label: cond_label }));
        let cond = self.expr(x.cond.clone());
        self.instrs.push(tac::Instr::JumpIfNot(tac::InstrJumpIfNot {
            cond,
            label: end_label,
        }));
        self.stmt(*x.body)?;
        self.instrs
            .push(tac::Instr::Jump(tac::InstrJump { label: cond_label }));
        self.instrs
            .push(tac::Instr::Label(tac::InstrLabel { label: end_label }));
        Ok(())
    }

    fn stmt_for(&mut self, x: clang::StmtFor) -> Result<(), CodegenError> {
        let cond_label = self.generate_label();
        let end_label = self.generate_label();

        if let Some(init) = x.init {
            self.expr(init);
        }
        self.instrs
            .push(tac::Instr::Label(tac::InstrLabel { label: cond_label }));
        if let Some(cond) = x.cond {
            let cond = self.expr(cond);
            self.instrs.push(tac::Instr::JumpIfNot(tac::InstrJumpIfNot {
                cond,
                label: end_label,
            }));
        }
        self.stmt(*x.body.clone())?;
        if let Some(step) = x.step {
            self.expr(step);
        }
        self.instrs
            .push(tac::Instr::Jump(tac::InstrJump { label: cond_label }));
        self.instrs
            .push(tac::Instr::Label(tac::InstrLabel { label: end_label }));
        Ok(())
    }

    fn expr(&mut self, expr: Expr) -> usize {
        use Expr::*;
        match expr {
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
            Addr(x) => self.expr_addr(x),
        }
    }

    fn expr_int_lit(&mut self, x: ExprIntLit) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        self.instrs.push(tac::Instr::IntConst(tac::InstrIntConst {
            dst,
            value: x.value,
        }));
        dst
    }

    fn expr_add(&mut self, x: clang::ExprAdd) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs
            .push(tac::Instr::Add(tac::InstrAdd { dst, lhs, rhs }));
        dst
    }

    fn expr_sub(&mut self, x: clang::ExprSub) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs
            .push(tac::Instr::Sub(tac::InstrSub { dst, lhs, rhs }));
        dst
    }

    fn expr_mul(&mut self, x: clang::ExprMul) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs
            .push(tac::Instr::Mul(tac::InstrMul { dst, lhs, rhs }));
        dst
    }

    fn expr_div(&mut self, x: clang::ExprDiv) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs
            .push(tac::Instr::Div(tac::InstrDiv { dst, lhs, rhs }));
        dst
    }

    fn expr_neg(&mut self, x: clang::ExprNeg) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let src = self.expr(*x.expr);
        self.instrs
            .push(tac::Instr::Neg(tac::InstrNeg { dst, src }));
        dst
    }

    fn expr_eq(&mut self, x: clang::ExprEq) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs
            .push(tac::Instr::Eq(tac::InstrEq { dst, lhs, rhs }));
        dst
    }

    fn expr_ne(&mut self, x: clang::ExprNe) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs
            .push(tac::Instr::Ne(tac::InstrNe { dst, lhs, rhs }));
        dst
    }

    fn expr_lt(&mut self, x: clang::ExprLt) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs
            .push(tac::Instr::Lt(tac::InstrLt { dst, lhs, rhs }));
        dst
    }

    fn expr_le(&mut self, x: clang::ExprLe) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs
            .push(tac::Instr::Le(tac::InstrLe { dst, lhs, rhs }));
        dst
    }

    fn expr_gt(&mut self, x: clang::ExprGt) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr::Lt(tac::InstrLt {
            dst,
            lhs: rhs,
            rhs: lhs,
        }));
        dst
    }

    fn expr_ge(&mut self, x: clang::ExprGe) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let lhs = self.expr(*x.lhs);
        let rhs = self.expr(*x.rhs);
        self.instrs.push(tac::Instr::Le(tac::InstrLe {
            dst,
            lhs: rhs,
            rhs: lhs,
        }));
        dst
    }

    fn expr_assign(&mut self, x: clang::ExprAssign) -> usize {
        let clang::Expr::LValue(lvalue) = *x.lhs else {
            panic!("expected lvalue");
        };
        let dst = self.lvalue(lvalue);
        let src = self.expr(*x.rhs);
        self.instrs
            .push(tac::Instr::AssignIndirect(tac::InstrAssignIndirect {
                dst,
                src,
            }));
        src
    }

    fn expr_lvalue(&mut self, x: clang::ExprLValue) -> usize {
        let dst = self.generate_local(Bit::Bit32); // TODO:
        let src = self.lvalue(x);
        self.instrs
            .push(tac::Instr::Deref(tac::InstrDeref { dst, src }));
        dst
    }

    fn expr_call(&mut self, x: clang::ExprCall) -> usize {
        let dst = self.generate_local(Bit::Bit32);
        let args = x
            .args
            .into_iter()
            .map(|arg| self.expr(arg))
            .collect::<Vec<_>>();
        self.instrs.push(tac::Instr::Call(tac::InstrCall {
            dst,
            ident: x.ident,
            args,
        }));
        dst
    }

    fn expr_addr(&mut self, x: clang::ExprAddr) -> usize {
        let clang::Expr::LValue(lvalue) = *x.expr else {
            panic!("expected lvalue");
        };
        let dst = self.lvalue(lvalue);
        dst
    }

    fn lvalue(&mut self, x: clang::ExprLValue) -> usize {
        use clang::ExprLValue::*;
        match x {
            Var(x) => self.lvalue_var(x),
            Deref(x) => self.lvalue_deref(x),
        }
    }

    fn lvalue_var(&mut self, x: clang::LValueVar) -> usize {
        let dst = self.generate_local(Bit::Bit64);
        let src = *self
            .local_idents
            .get(&x.ident)
            .expect("undeclared variable");
        self.instrs
            .push(tac::Instr::LocalAddr(tac::InstrLocalAddr { dst, src }));
        dst
    }

    fn lvalue_deref(&mut self, x: clang::LValueDeref) -> usize {
        let dst = self.expr(*x.expr);
        dst
    }
}

pub fn generate(program: Program) -> Result<tac::Program, CodegenError> {
    let decls = program
        .decls
        .into_iter()
        .map(|decl| {
            use Decl::*;
            match decl {
                Func(x) => {
                    let mut gen = InstrGenerator::new();
                    for param in &x.params {
                        gen.add_named_local(param.ident.clone(), &param.ident_loc, Bit::Bit32)?;
                    }
                    gen.stmt_compound(x.body)?;
                    Ok(tac::Decl::Func(tac::DeclFunc {
                        ident: x.ident,
                        args_count: x.params.len(),
                        locals: gen.locals,
                        instrs: gen.instrs,
                    }))
                }
            }
        })
        .collect::<Result<Vec<_>, CodegenError>>()?;

    Ok(tac::Program { decls })
}
