use crate::buf::Buf;
use crate::tac::{self, Decl, DeclFunc, DeclPayload, Instr, InstrPayload, InstrReturn, Program};

#[derive(Debug)]
struct Generator {
    output: Buf,
}

impl Generator {
    fn new() -> Generator {
        Generator { output: Buf::new() }
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
        let locals_len = x.locals_count * 8;
        let locals_pad = if locals_len % 16 == 0 {
            0
        } else {
            16 - locals_len % 16
        };
        self.output
            .append(format!("sub rsp, {}\n", locals_len + locals_pad));
        for instr in x.instrs {
            self.instr(instr);
        }
    }

    fn instr(&mut self, instr: Instr) {
        use InstrPayload::*;
        match instr.payload {
            Return(x) => self.instr_return(x),
            IntConst(x) => self.instr_int_const(x),
        }
    }

    fn instr_return(&mut self, x: InstrReturn) {
        self.output
            .append(format!("mov rax, QWORD PTR [rbp-{}]\n", x.src * 8 + 8));
        self.output.append("leave\n");
        self.output.append("ret\n");
    }

    fn instr_int_const(&mut self, x: tac::InstrIntConst) {
        self.output.append(format!(
            "mov QWORD PTR [rbp-{}], {}\n",
            x.dst * 8 + 8,
            x.value
        ));
    }
}

pub fn generate(program: Program) -> Vec<u8> {
    let mut gen = Generator::new();
    gen.program(program);
    gen.output.to_vec()
}
