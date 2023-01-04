use crate::{
    ast::{
        Decl, DeclFunc, DeclPayload, Expr, ExprIntLit, ExprPayload, Program, Stmt, StmtCompound,
        StmtExpr, StmtPayload, StmtReturn,
    },
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

    pub fn generate(&mut self, program: Program) -> Vec<u8> {
        self.program(program);
        self.output.to_vec()
    }

    fn program(&mut self, program: Program) {
        self.output.append(".intel_syntax noprefix\n");
        self.output.append(".globl main\n");
        for decl in program.decls {
            self.decl(decl);
        }
    }

    fn decl(&mut self, decl: Decl) {
        use DeclPayload::*;
        match decl.payload {
            Func(x) => {
                self.decl_func(x);
            }
        }
    }

    fn decl_func(&mut self, x: DeclFunc) {
        self.output.append(format!("{}:\n", x.name));
        self.output.append("push rbp\n");
        self.output.append("mov rbp, rsp\n");
        self.output.append(format!("sub rsp, {}\n", 0));
        self.stmt_compound(x.body);
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
        }
    }

    fn stmt_expr(&mut self, x: StmtExpr) {
        self.expr(x.expr);
    }

    fn stmt_return(&mut self, x: StmtReturn) {
        self.expr(x.expr);
        self.output.append("leave\n");
        self.output.append("ret\n");
    }

    fn stmt_compound(&mut self, x: StmtCompound) {
        for stmt in x.stmts {
            self.stmt(stmt);
        }
    }

    fn expr(&mut self, expr: Expr) {
        use ExprPayload::*;
        match expr.payload {
            IntLit(x) => self.expr_int_lit(x),
        }
    }

    fn expr_int_lit(&mut self, x: ExprIntLit) {
        self.output.append(format!("mov rax, {}\n", x.value));
    }
}
