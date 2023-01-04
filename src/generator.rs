use crate::{
    ast::{Decl, DeclPayload, Expr, ExprPayload, Program, Stmt, StmtPayload},
    buf::Buf,
};

#[derive(Debug)]
pub struct Generator {
    output: Buf,
}

impl Generator {
    pub fn new() -> Generator {
        Generator { output: Buf::new() }
    }

    fn expr(&mut self, expr: Expr) {
        use ExprPayload::*;
        match expr.payload {
            IntLit(x) => {
                self.output.append(format!("mov rax, {}\n", x));
            }
        }
    }

    fn stmt(&mut self, stmt: Stmt) {
        use StmtPayload::*;
        match stmt.payload {
            Expr(expr) => {
                self.expr(expr);
            }
            Return(expr) => {
                self.expr(expr);
                self.output.append("leave\n");
                self.output.append("ret\n");
            }
            Compound(stmts) => {
                for stmt in stmts {
                    self.stmt(stmt);
                }
            }
        }
    }

    fn decl(&mut self, decl: Decl) {
        use DeclPayload::*;
        match decl.payload {
            Func { name, body } => {
                self.output.append(format!("{}:\n", name));
                self.output.append("push rbp\n");
                self.output.append("mov rbp, rsp\n");
                self.output.append(format!("sub rsp, {}\n", 0));
                for stmt in body {
                    self.stmt(stmt);
                }
            }
        }
    }

    fn program(&mut self, program: Program) {
        self.output.append(".intel_syntax noprefix\n");
        self.output.append(".globl main\n");
        for decl in program.decls {
            self.decl(decl);
        }
    }

    pub fn generate(&mut self, program: Program) -> Vec<u8> {
        self.program(program);
        self.output.to_vec()
    }
}
