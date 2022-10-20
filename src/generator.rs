use crate::ast::Expr;

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

    pub fn expr(&mut self, expr: Expr) {
        use crate::ast::ExprPayload::*;
        match expr.payload {
            IntLit(x) => {
                self.output.push_str(&format!("push {}\n", x));
            }
        }
    }
}
