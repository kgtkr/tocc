use crate::ast::{Decl, DeclPayload, Expr, ExprPayload, Program, Stmt, StmtPayload};

#[derive(Debug)]
pub struct Generator {
    output: String,
}

impl Generator {
    pub fn new() -> Generator {
        Generator {
            output: String::new(),
        }
    }

    fn expr(&mut self, expr: Expr) {
        use ExprPayload::*;
        match expr.payload {
            IntLit(x) => {
                self.output.push_str(&format!("mov rax, {}\n", x));
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
                self.output.push_str("leave\n");
                self.output.push_str("ret\n");
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
                self.output.push_str(&format!("{}:\n", name));
                self.output.push_str("push rbp\n");
                self.output.push_str("mov rbp, rsp\n");
                self.output.push_str(&format!("sub rsp, {}\n", 0));
                for stmt in body {
                    self.stmt(stmt);
                }
            }
        }
    }

    fn program(&mut self, program: Program) {
        self.output.push_str(".intel_syntax noprefix\n");
        self.output.push_str(".globl main\n");
        for decl in program.decls {
            self.decl(decl);
        }
    }

    pub fn generate(&mut self, program: Program) -> String {
        self.program(program);
        self.output.clone()
    }
}
