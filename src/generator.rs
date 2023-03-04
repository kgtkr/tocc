use crate::buf::Buf;
use crate::tac::{self, Decl, DeclFunc, DeclPayload, Instr, InstrPayload, InstrReturn, Program};
use crate::Bit;

#[derive(Debug, Copy, Clone)]
enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Register {
    fn for_bit(self, bit: Bit) -> &'static str {
        use Bit::*;
        use Register::*;
        match self {
            Rax => match bit {
                Bit8 => "al",
                Bit16 => "ax",
                Bit32 => "eax",
                Bit64 => "rax",
            },
            Rbx => match bit {
                Bit8 => "bl",
                Bit16 => "bx",
                Bit32 => "ebx",
                Bit64 => "rbx",
            },
            Rcx => match bit {
                Bit8 => "cl",
                Bit16 => "cx",
                Bit32 => "ecx",
                Bit64 => "rcx",
            },
            Rdx => match bit {
                Bit8 => "dl",
                Bit16 => "dx",
                Bit32 => "edx",
                Bit64 => "rdx",
            },
            Rsi => match bit {
                Bit8 => "sil",
                Bit16 => "si",
                Bit32 => "esi",
                Bit64 => "rsi",
            },
            Rdi => match bit {
                Bit8 => "dil",
                Bit16 => "di",
                Bit32 => "edi",
                Bit64 => "rdi",
            },
            Rbp => match bit {
                Bit8 => "bpl",
                Bit16 => "bp",
                Bit32 => "ebp",
                Bit64 => "rbp",
            },
            Rsp => match bit {
                Bit8 => "spl",
                Bit16 => "sp",
                Bit32 => "esp",
                Bit64 => "rsp",
            },
            R8 => match bit {
                Bit8 => "r8b",
                Bit16 => "r8w",
                Bit32 => "r8d",
                Bit64 => "r8",
            },
            R9 => match bit {
                Bit8 => "r9b",
                Bit16 => "r9w",
                Bit32 => "r9d",
                Bit64 => "r9",
            },
            R10 => match bit {
                Bit8 => "r10b",
                Bit16 => "r10w",
                Bit32 => "r10d",
                Bit64 => "r10",
            },
            R11 => match bit {
                Bit8 => "r11b",
                Bit16 => "r11w",
                Bit32 => "r11d",
                Bit64 => "r11",
            },
            R12 => match bit {
                Bit8 => "r12b",
                Bit16 => "r12w",
                Bit32 => "r12d",
                Bit64 => "r12",
            },
            R13 => match bit {
                Bit8 => "r13b",
                Bit16 => "r13w",
                Bit32 => "r13d",
                Bit64 => "r13",
            },
            R14 => match bit {
                Bit8 => "r14b",
                Bit16 => "r14w",
                Bit32 => "r14d",
                Bit64 => "r14",
            },
            R15 => match bit {
                Bit8 => "r15b",
                Bit16 => "r15w",
                Bit32 => "r15d",
                Bit64 => "r15",
            },
        }
    }
}

#[derive(Debug)]
struct Generator {
    buf: Buf,
}

impl Generator {
    fn new() -> Generator {
        Generator { buf: Buf::new() }
    }

    fn program(&mut self, program: Program) {
        self.buf.append(".intel_syntax noprefix\n");
        self.buf.append(".globl main\n");
        for decl in program.decls {
            self.decl(decl);
        }
        self.buf
            .append(".section .note.GNU-stack, \"\", @progbits\n");
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
        self.buf.append(FuncGenerator::gen(x));
    }
}

#[derive(Debug)]
pub struct FuncGenerator {
    buf: Buf,
    local_offsets: Vec<usize>,
    locals: Vec<tac::Local>,
    func_name: String,
}

impl FuncGenerator {
    fn gen(func: DeclFunc) -> Buf {
        let local_offsets = func
            .locals
            .iter()
            .scan(8 /* rbpの分 */, |acc, local| {
                let offset = *acc;
                *acc += local.bit.to_size();
                Some(offset)
            })
            .collect::<Vec<_>>();

        let mut gen = FuncGenerator {
            buf: Buf::new(),
            local_offsets,
            locals: func.locals.clone(),
            func_name: func.name.clone(),
        };
        gen.decl_func(func);
        gen.buf
    }

    fn decl_func(&mut self, func: DeclFunc) {
        let locals_size = func
            .locals
            .iter()
            .map(|local| local.bit.to_size())
            .sum::<usize>();

        self.buf.append(format!("{}:\n", func.name));
        self.buf.append("push rbp\n");
        self.buf.append("mov rbp, rsp\n");
        let stack_size = locals_size + 8 /* rbpの分 */;
        let locals_pad = if stack_size % 16 == 0 {
            0
        } else {
            16 - stack_size % 16
        };
        self.buf
            .append(format!("sub rsp, {}\n", locals_size + locals_pad));
        for instr in func.instrs {
            self.instr(instr);
        }
    }

    fn local(&self, local: usize) -> String {
        format!(
            "{} PTR [rbp-{}]",
            match self.locals[local].bit {
                Bit::Bit8 => "BYTE",
                Bit::Bit16 => "WORD",
                Bit::Bit32 => "DWORD",
                Bit::Bit64 => "QWORD",
            },
            self.local_offsets[local]
        )
    }

    fn instr(&mut self, instr: Instr) {
        use InstrPayload::*;
        match instr.payload {
            Return(x) => self.instr_return(x),
            IntConst(x) => self.instr_int_const(x),
            Add(x) => self.instr_add(x),
            Sub(x) => self.instr_sub(x),
            Mul(x) => self.instr_mul(x),
            Div(x) => self.instr_div(x),
            Neg(x) => self.instr_neg(x),
            Eq(x) => self.instr_eq(x),
            Ne(x) => self.instr_ne(x),
            Lt(x) => self.instr_lt(x),
            Le(x) => self.instr_le(x),
            LocalAddr(x) => self.instr_local_addr(x),
            Deref(x) => self.instr_deref(x),
            AssignIndirect(x) => self.instr_assign_indirect(x),
            Label(x) => self.instr_label(x),
            Jump(x) => self.instr_jump(x),
            JumpIf(x) => self.instr_jump_if(x),
            JumpIfNot(x) => self.instr_jump_if_not(x),
            Call(x) => self.instr_call(x),
        }
    }

    fn instr_return(&mut self, x: InstrReturn) {
        self.buf.append(format!("mov eax, {}\n", self.local(x.src)));
        self.buf.append("leave\n");
        self.buf.append("ret\n");
    }

    fn instr_int_const(&mut self, x: tac::InstrIntConst) {
        self.buf
            .append(format!("mov {}, {}\n", self.local(x.dst), x.value));
    }

    fn instr_add(&mut self, x: tac::InstrAdd) {
        self.buf.append(format!("mov eax, {}\n", self.local(x.lhs)));
        self.buf.append(format!("mov edi, {}\n", self.local(x.rhs)));
        self.buf.append("add rax, rdi\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_sub(&mut self, x: tac::InstrSub) {
        self.buf.append(format!("mov eax, {}\n", self.local(x.lhs)));
        self.buf.append(format!("mov edi, {}\n", self.local(x.rhs)));
        self.buf.append("sub rax, rdi\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_mul(&mut self, x: tac::InstrMul) {
        self.buf.append(format!("mov eax, {}\n", self.local(x.lhs)));
        self.buf.append(format!("mov edi, {}\n", self.local(x.rhs)));
        self.buf.append("imul rax, rdi\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_div(&mut self, x: tac::InstrDiv) {
        self.buf.append(format!("mov eax, {}\n", self.local(x.lhs)));
        self.buf.append(format!("mov edi, {}\n", self.local(x.rhs)));
        self.buf.append("cqo\n");
        self.buf.append("idiv edi\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_neg(&mut self, x: tac::InstrNeg) {
        self.buf.append(format!("mov eax, {}\n", self.local(x.src)));
        self.buf.append("neg eax\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_eq(&mut self, x: tac::InstrEq) {
        self.buf.append(format!("mov edi, {}\n", self.local(x.rhs)));
        self.buf.append(format!("mov eax, {}\n", self.local(x.lhs)));
        self.buf.append("cmp eax, edi\n");
        self.buf.append("sete al\n");
        self.buf.append("movzx eax, al\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_ne(&mut self, x: tac::InstrNe) {
        self.buf.append(format!("mov edi, {}\n", self.local(x.rhs)));
        self.buf.append(format!("mov eax, {}\n", self.local(x.lhs)));
        self.buf.append("cmp eax, edi\n");
        self.buf.append("setne al\n");
        self.buf.append("movzx eax, al\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_lt(&mut self, x: tac::InstrLt) {
        self.buf.append(format!("mov edi, {}\n", self.local(x.rhs)));
        self.buf.append(format!("mov eax, {}\n", self.local(x.lhs)));
        self.buf.append("cmp eax, edi\n");
        self.buf.append("setl al\n");
        self.buf.append("movzx eax, al\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_le(&mut self, x: tac::InstrLe) {
        self.buf.append(format!("mov edi, {}\n", self.local(x.rhs)));
        self.buf.append(format!("mov eax, {}\n", self.local(x.lhs)));
        self.buf.append("cmp eax, edi\n");
        self.buf.append("setle al\n");
        self.buf.append("movzx eax, al\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_local_addr(&mut self, x: tac::InstrLocalAddr) {
        self.buf.append(format!("lea rax, {}\n", self.local(x.src)));
        self.buf.append(format!("mov {}, rax\n", self.local(x.dst)));
    }

    fn instr_deref(&mut self, x: tac::InstrDeref) {
        self.buf.append(format!("mov rax, {}\n", self.local(x.src)));
        self.buf.append("mov eax, [rax]\n");
        self.buf.append(format!("mov {}, eax\n", self.local(x.dst)));
    }

    fn instr_assign_indirect(&mut self, x: tac::InstrAssignIndirect) {
        self.buf.append(format!("mov rax, {}\n", self.local(x.dst)));
        self.buf.append(format!("mov edi, {}\n", self.local(x.src)));
        self.buf.append("mov DWORD PTR [rax], edi\n");
    }

    fn instr_label(&mut self, x: tac::InstrLabel) {
        self.buf
            .append(format!(".L.{}.{}:\n", self.func_name, x.label));
    }

    fn instr_jump(&mut self, x: tac::InstrJump) {
        self.buf
            .append(format!("jmp .L.{}.{}\n", self.func_name, x.label));
    }

    fn instr_jump_if(&mut self, x: tac::InstrJumpIf) {
        self.buf
            .append(format!("mov eax, {}\n", self.local(x.cond)));
        self.buf.append(format!("cmp eax, 0\n"));
        self.buf
            .append(format!("jne .L.{}.{}\n", self.func_name, x.label));
    }

    fn instr_jump_if_not(&mut self, x: tac::InstrJumpIfNot) {
        self.buf
            .append(format!("mov eax, {}\n", self.local(x.cond)));
        self.buf.append(format!("cmp eax, 0\n"));
        self.buf
            .append(format!("je .L.{}.{}\n", self.func_name, x.label));
    }

    fn instr_call(&mut self, x: tac::InstrCall) {
        if let Some(arg) = x.args.get(0) {
            self.buf.append(format!("mov rdi, {}\n", self.local(*arg)));
        }

        if let Some(arg) = x.args.get(1) {
            self.buf.append(format!("mov rsi, {}\n", self.local(*arg)));
        }

        if let Some(arg) = x.args.get(2) {
            self.buf.append(format!("mov rdx, {}\n", self.local(*arg)));
        }

        if let Some(arg) = x.args.get(3) {
            self.buf.append(format!("mov rcx, {}\n", self.local(*arg)));
        }

        if let Some(arg) = x.args.get(4) {
            self.buf.append(format!("mov r8, {}\n", self.local(*arg)));
        }

        if let Some(arg) = x.args.get(5) {
            self.buf.append(format!("mov r9, {}\n", self.local(*arg)));
        }

        let extra_args_size= if x.args.len() > 6 {
            let extra_args_size = x
                .args
                .iter()
                .skip(6)
                .map(|&_arg| 
                    // TODO:
                    4
                )
                .sum::<usize>();
            let extra_args_size = extra_args_size + (extra_args_size % 16);
            self.buf.append(format!("sub rsp, {}\n", extra_args_size));
            for (i, arg) in x.args.iter().enumerate().skip(6) {
                self.buf.append(format!("mov rax, {}\n", self.local(*arg)));
                self.buf.append(format!("mov [rsp + {}], rax\n", i * 4));
            }
            extra_args_size
        } else {
            0
        };

        self.buf.append(format!("call {}\n", x.name));
        self.buf.append(format!("add rsp, {}\n", extra_args_size));
        self.buf.append(format!("mov {}, rax\n", self.local(x.dst)));
    }
}

pub fn generate(program: Program) -> Vec<u8> {
    let mut gen = Generator::new();
    gen.program(program);
    gen.buf.to_vec()
}
