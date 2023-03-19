use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::clang::{
    self, BinOp, Decl, Expr, ExprBinOp, ExprIntLit, Program, Stmt, StmtCompound, StmtExpr, StmtIf,
    StmtReturn, StmtVarDecl, Type,
};
use crate::loc::{Loc, Locatable};
use crate::{tac, Bit};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
#[error("{loc}: {message}")]
pub struct CodegenError {
    loc: Loc,
    message: String,
}

fn convert_type(typ: Type) -> tac::Type {
    match typ {
        Type::Int(_) => tac::Type::Int(tac::TypeInt {}),
        Type::Ptr(ptr) => tac::Type::Ptr(tac::TypePtr {
            typ: Box::new(convert_type(*ptr.typ)),
        }),
    }
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
        self.locals.push(tac::Local { typ });
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
        self.instrs
            .push(tac::Instr::Return(tac::InstrReturn { src }));
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
        self.add_named_local(x.ident, &x.ident_loc, convert_type(x.typ))?;
        Ok(())
    }

    fn stmt_if(&mut self, x: StmtIf) -> Result<(), CodegenError> {
        let else_label = self.generate_label();
        let end_label = self.generate_label();

        let cond = self.expr(x.cond.clone())?;

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
        let cond = self.expr(x.cond.clone())?;
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
            self.expr(init)?;
        }
        self.instrs
            .push(tac::Instr::Label(tac::InstrLabel { label: cond_label }));
        if let Some(cond) = x.cond {
            let cond = self.expr(cond)?;
            self.instrs.push(tac::Instr::JumpIfNot(tac::InstrJumpIfNot {
                cond,
                label: end_label,
            }));
        }
        self.stmt(*x.body.clone())?;
        if let Some(step) = x.step {
            self.expr(step)?;
        }
        self.instrs
            .push(tac::Instr::Jump(tac::InstrJump { label: cond_label }));
        self.instrs
            .push(tac::Instr::Label(tac::InstrLabel { label: end_label }));
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
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
        self.instrs.push(tac::Instr::IntConst(tac::InstrIntConst {
            dst,
            value: x.value,
        }));
        Ok(dst)
    }

    fn expr_bin_op(&mut self, x: ExprBinOp) -> Result<usize, CodegenError> {
        match x.op {
            BinOp::Add => self.bin_op_add(*x.lhs, *x.rhs),
            BinOp::Sub => self.bin_op_sub(*x.lhs, *x.rhs),
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

    fn bin_op_add(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs,
            rhs,
            op: tac::BinOp::Add,
        }));
        Ok(dst)
    }

    fn bin_op_sub(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;
        self.instrs.push(tac::Instr::BinOp(tac::InstrBinOp {
            dst,
            lhs,
            rhs,
            op: tac::BinOp::Sub,
        }));
        Ok(dst)
    }

    fn bin_op_mul(&mut self, lhs: Expr, rhs: Expr) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
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
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
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
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
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
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
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
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
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
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
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
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
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
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
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
        let dst = self.lvalue(lvalue)?;
        let src = self.expr(rhs)?;
        self.instrs
            .push(tac::Instr::AssignIndirect(tac::InstrAssignIndirect {
                dst,
                src,
            }));
        Ok(src)
    }

    fn expr_neg(&mut self, x: clang::ExprNeg) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
        let src = self.expr(*x.expr)?;
        self.instrs
            .push(tac::Instr::Neg(tac::InstrNeg { dst, src }));
        Ok(dst)
    }

    fn expr_lvalue(&mut self, x: clang::ExprLValue) -> Result<usize, CodegenError> {
        let src = self.lvalue(x)?;
        let src_type = self.locals[src].typ.clone();
        let dst = self.generate_local(*src_type.unwrap_ptr().typ);
        self.instrs
            .push(tac::Instr::Deref(tac::InstrDeref { dst, src }));
        Ok(dst)
    }

    fn expr_call(&mut self, x: clang::ExprCall) -> Result<usize, CodegenError> {
        let dst = self.generate_local(tac::Type::Int(tac::TypeInt {}));
        let args = x
            .args
            .into_iter()
            .map(|arg| self.expr(arg))
            .collect::<Result<Vec<_>, _>>()?;
        self.instrs.push(tac::Instr::Call(tac::InstrCall {
            dst,
            ident: x.ident,
            args,
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

        let local = &self.locals[src];
        let dst = self.generate_local(tac::Type::Ptr(tac::TypePtr {
            typ: Box::new(local.typ.clone()),
        }));
        self.instrs
            .push(tac::Instr::LocalAddr(tac::InstrLocalAddr { dst, src }));
        Ok(dst)
    }

    fn lvalue_deref(&mut self, x: clang::LValueDeref) -> Result<usize, CodegenError> {
        self.expr(*x.expr)
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
                        gen.add_named_local(
                            param.ident.clone(),
                            &param.ident_loc,
                            tac::Type::Int(tac::TypeInt {}),
                        )?;
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
