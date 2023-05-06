use std::collections::HashMap;

use crate::{
    clang::{
        BinOp, Decl, DeclFunc, Expr, ExprAddr, ExprBinOp, ExprCall, ExprIntLit, ExprLValue,
        ExprNeg, FuncSig, LValueDeref, LValueVar, Program, Stmt, StmtCompound, StmtExpr, StmtFor,
        StmtIf, StmtReturn, StmtVarDecl, StmtWhile, Type, TypeInt, TypePtr,
    },
    loc::{Loc, Locatable},
    tac_generator::CodegenError,
};
use thiserror::Error;
#[derive(Debug)]
pub struct StaticAnalysisResult {
    pub exprs: HashMap<usize, Type>,
}

#[derive(Error, Debug, Clone)]
pub enum StaticAnalysisError {
    #[error("{loc}: unexpected type: expected {expected}, actual {actual}")]
    UnexpectedType {
        loc: Loc,
        actual: Type,
        expected: String,
    },
}

#[derive(Debug)]
struct ProgramAnalysis {
    funcs: HashMap<String, FuncSig>,
    exprs: HashMap<usize, Type>,
}

impl ProgramAnalysis {
    fn new() -> Self {
        ProgramAnalysis {
            funcs: HashMap::new(),
            exprs: HashMap::new(),
        }
    }

    fn program(&mut self, program: &Program) -> Result<(), StaticAnalysisError> {
        for func in &program.decls {
            use Decl::*;
            match func {
                Func(func) => self.func(func)?,
            }
        }
        Ok(())
    }

    fn func(&mut self, func: &DeclFunc) -> Result<(), StaticAnalysisError> {
        self.funcs.insert(func.sig.ident.clone(), func.sig.clone());
        let mut func_analysis = FuncAnalysis::new(&self.funcs);
        func_analysis.func(func)?;
        self.exprs.extend(func_analysis.exprs);
        Ok(())
    }
}

#[derive(Debug)]
struct FuncAnalysis<'a> {
    funcs: &'a HashMap<String, FuncSig>,
    locals: HashMap<String, Type>,
    exprs: HashMap<usize, Type>,
}

impl<'a> FuncAnalysis<'a> {
    fn new(funcs: &'a HashMap<String, FuncSig>) -> Self {
        FuncAnalysis {
            funcs,
            locals: HashMap::new(),
            exprs: HashMap::new(),
        }
    }

    fn func(&mut self, func: &DeclFunc) -> Result<(), StaticAnalysisError> {
        for param in &func.sig.params {
            self.locals.insert(param.ident.clone(), param.typ.clone());
        }

        if let Some(body) = &func.body {
            self.stmt_compound(body)?;
        }

        Ok(())
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<(), StaticAnalysisError> {
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

    fn stmt_expr(&mut self, x: &StmtExpr) -> Result<(), StaticAnalysisError> {
        self.expr(&x.expr)?;
        Ok(())
    }

    fn stmt_return(&mut self, x: &StmtReturn) -> Result<(), StaticAnalysisError> {
        self.expr(&x.expr)?;
        Ok(())
    }

    fn stmt_compound(&mut self, x: &StmtCompound) -> Result<(), StaticAnalysisError> {
        for stmt in &x.stmts {
            self.stmt(stmt)?;
        }
        Ok(())
    }

    fn stmt_var_decl(&mut self, x: &StmtVarDecl) -> Result<(), StaticAnalysisError> {
        self.locals.insert(x.ident.clone(), x.typ.clone());
        Ok(())
    }

    fn stmt_if(&mut self, x: &StmtIf) -> Result<(), StaticAnalysisError> {
        let cond = self.expr(&x.cond)?;
        self.stmt(&x.then)?;
        if let Some(else_) = &x.else_ {
            self.stmt(else_)?;
        }

        Ok(())
    }

    fn stmt_while(&mut self, x: &StmtWhile) -> Result<(), StaticAnalysisError> {
        self.expr(&x.cond)?;
        self.stmt(&x.body)?;
        Ok(())
    }

    fn stmt_for(&mut self, x: &StmtFor) -> Result<(), StaticAnalysisError> {
        if let Some(init) = &x.init {
            self.expr(init)?;
        }
        if let Some(cond) = &x.cond {
            self.expr(cond)?;
        }
        if let Some(step) = &x.step {
            self.expr(step)?;
        }
        self.stmt(&x.body)?;
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Result<(), StaticAnalysisError> {
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

    fn expr_int_lit(&mut self, x: &ExprIntLit) -> Result<(), StaticAnalysisError> {
        self.exprs.insert(
            x.id,
            Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
        );
        Ok(())
    }

    fn expr_bin_op(&mut self, x: &ExprBinOp) -> Result<(), StaticAnalysisError> {
        use BinOp::*;
        match x.op {
            Add => self.bin_op_add(x.id, &x.lhs, &x.rhs),
            Sub => self.bin_op_sub(x.id, &x.lhs, &x.rhs),
            Mul => self.bin_op_mul(x.id, &x.lhs, &x.rhs),
            Div => self.bin_op_div(x.id, &x.lhs, &x.rhs),
            Eq => self.bin_op_eq(x.id, &x.lhs, &x.rhs),
            Ne => self.bin_op_ne(x.id, &x.lhs, &x.rhs),
            Lt => self.bin_op_lt(x.id, &x.lhs, &x.rhs),
            Le => self.bin_op_le(x.id, &x.lhs, &x.rhs),
            Gt => self.bin_op_gt(x.id, &x.lhs, &x.rhs),
            Ge => self.bin_op_ge(x.id, &x.lhs, &x.rhs),
            Assign => self.bin_op_assign(x.id, &x.lhs, &x.rhs),
        }
    }

    fn bin_op_add(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;
        let l_typ = &self.exprs[&lhs.id()];
        let r_typ = &self.exprs[&rhs.id()];
        let typ = match (l_typ, r_typ) {
            (Type::Int(_), Type::Int(_)) => Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
            (Type::Ptr(l), Type::Int(_)) => Type::Ptr(l.clone()),
            (Type::Int(_), Type::Ptr(r)) => Type::Ptr(r.clone()),
            (Type::Ptr(_), Type::Ptr(_)) => {
                return Err(StaticAnalysisError::UnexpectedType {
                    loc: rhs.loc().clone(),
                    actual: r_typ.clone(),
                    expected: "int".to_string(),
                })
            }
        };
        self.exprs.insert(id, typ);
        Ok(())
    }

    fn bin_op_sub(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;
        let l_typ = &self.exprs[&lhs.id()];
        let r_typ = &self.exprs[&rhs.id()];
        let typ = match (l_typ, r_typ) {
            (Type::Int(_), Type::Int(_)) => Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
            (Type::Ptr(l), Type::Int(_)) => Type::Ptr(l.clone()),
            (Type::Ptr(l), Type::Ptr(r)) => Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
            (Type::Int(_), Type::Ptr(_)) => {
                return Err(StaticAnalysisError::UnexpectedType {
                    loc: rhs.loc().clone(),
                    actual: r_typ.clone(),
                    expected: "int".to_string(),
                })
            }
        };
        self.exprs.insert(id, typ);
        Ok(())
    }

    fn bin_op_mul(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;
        let l_typ = &self.exprs[&lhs.id()];
        let r_typ = &self.exprs[&rhs.id()];
        let typ = match (l_typ, r_typ) {
            (Type::Int(_), Type::Int(_)) => Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
            (Type::Int(_), rhs) => {
                return Err(StaticAnalysisError::UnexpectedType {
                    loc: rhs.loc().clone(),
                    actual: r_typ.clone(),
                    expected: "int".to_string(),
                })
            }
            (lhs, _) => {
                return Err(StaticAnalysisError::UnexpectedType {
                    loc: lhs.loc().clone(),
                    actual: l_typ.clone(),
                    expected: "int".to_string(),
                })
            }
        };
        self.exprs.insert(id, typ);
        Ok(())
    }

    fn bin_op_div(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;
        let l_typ = &self.exprs[&lhs.id()];
        let r_typ = &self.exprs[&rhs.id()];
        let typ = match (l_typ, r_typ) {
            (Type::Int(_), Type::Int(_)) => Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
            (Type::Int(_), rhs) => {
                return Err(StaticAnalysisError::UnexpectedType {
                    loc: rhs.loc().clone(),
                    actual: r_typ.clone(),
                    expected: "int".to_string(),
                })
            }
            (lhs, _) => {
                return Err(StaticAnalysisError::UnexpectedType {
                    loc: lhs.loc().clone(),
                    actual: l_typ.clone(),
                    expected: "int".to_string(),
                })
            }
        };
        self.exprs.insert(id, typ);
        Ok(())
    }

    fn bin_op_eq(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;

        self.exprs.insert(
            id,
            Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
        );
        Ok(())
    }

    fn bin_op_ne(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;

        self.exprs.insert(
            id,
            Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
        );
        Ok(())
    }

    fn bin_op_lt(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;

        self.exprs.insert(
            id,
            Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
        );
        Ok(())
    }

    fn bin_op_le(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;

        self.exprs.insert(
            id,
            Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
        );
        Ok(())
    }

    fn bin_op_gt(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;

        self.exprs.insert(
            id,
            Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
        );
        Ok(())
    }

    fn bin_op_ge(&mut self, id: usize, lhs: &Expr, rhs: &Expr) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;

        self.exprs.insert(
            id,
            Type::Int(TypeInt {
                int_loc: Loc::dummy(),
            }),
        );
        Ok(())
    }

    fn bin_op_assign(
        &mut self,
        id: usize,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<(), StaticAnalysisError> {
        self.expr(lhs)?;
        self.expr(rhs)?;

        let r_typ = &self.exprs[&rhs.id()];
        self.exprs.insert(id, r_typ.clone());
        Ok(())
    }

    fn expr_neg(&mut self, x: &ExprNeg) -> Result<(), StaticAnalysisError> {
        self.expr(x.expr.as_ref())?;

        let typ = &self.exprs[&x.expr.as_ref().id()];
        self.exprs.insert(x.id, typ.clone());
        Ok(())
    }

    fn expr_lvalue(&mut self, x: &ExprLValue) -> Result<(), StaticAnalysisError> {
        use ExprLValue::*;
        match x {
            Var(x) => self.lvalue_var(x),
            Deref(x) => self.lvalue_deref(x),
        }
    }

    fn expr_call(&mut self, x: &ExprCall) -> Result<(), StaticAnalysisError> {
        for arg in &x.args {
            self.expr(arg)?;
        }
        let typ = self.funcs[&x.ident].typ.clone();
        self.exprs.insert(x.id, typ);
        Ok(())
    }

    fn expr_addr(&mut self, x: &ExprAddr) -> Result<(), StaticAnalysisError> {
        self.expr(x.expr.as_ref())?;
        let typ = self.exprs[&x.expr.as_ref().id()].clone();
        self.exprs
            .insert(x.id, Type::Ptr(TypePtr { typ: Box::new(typ) }));
        Ok(())
    }

    fn lvalue_var(&mut self, x: &LValueVar) -> Result<(), StaticAnalysisError> {
        let typ = self.locals[&x.ident].clone();
        self.exprs.insert(x.id, typ);
        Ok(())
    }

    fn lvalue_deref(&mut self, x: &LValueDeref) -> Result<(), StaticAnalysisError> {
        self.expr(x.expr.as_ref())?;
        let typ = &self.exprs[&x.expr.as_ref().id()];
        let typ = match typ {
            Type::Ptr(TypePtr { typ }) => typ.as_ref().clone(),
            _ => {
                return Err(StaticAnalysisError::UnexpectedType {
                    loc: x.expr.loc().clone(),
                    actual: typ.clone(),
                    expected: "pointer".to_string(),
                })
            }
        };
        self.exprs.insert(x.id, typ);
        Ok(())
    }
}

pub fn static_analysis(program: &Program) -> Result<StaticAnalysisResult, StaticAnalysisError> {
    let mut analysis = ProgramAnalysis::new();
    analysis.program(program)?;

    Ok(StaticAnalysisResult {
        exprs: analysis.exprs,
    })
}
