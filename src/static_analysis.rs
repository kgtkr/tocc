use std::collections::HashMap;

use crate::clang::{Decl, DeclFunc, FuncSig, Program, Stmt, Type};
use thiserror::Error;
#[derive(Debug)]
pub struct StaticAnalysisResult {
    pub exprs: HashMap<usize, Type>,
}

#[derive(Error, Debug, Clone)]
pub enum StaticAnalysisError {}

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

        if let Some(body) = &func.body {}

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
