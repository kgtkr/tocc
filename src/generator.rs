use crate::buf::Buf;
use crate::tac::{self, Decl, DeclFunc, Instr, Program};
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

fn bit_to_word(bit: Bit) -> &'static str {
    use Bit::*;
    match bit {
        Bit8 => "BYTE",
        Bit16 => "WORD",
        Bit32 => "DWORD",
        Bit64 => "QWORD",
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
        self.buf += ".intel_syntax noprefix\n";
        self.buf += ".globl main\n";
        for decl in program.decls {
            self.decl(decl);
        }
        self.buf += ".section .note.GNU-stack, \"\", @progbits\n";
    }

    fn decl(&mut self, decl: Decl) {
        use Decl::*;
        match decl {
            Func(x) => {
                self.decl_func(x);
            }
        }
    }

    fn decl_func(&mut self, x: DeclFunc) {
        self.buf += FuncGenerator::gen(x);
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
            .scan(0, |acc, local| {
                *acc += local.typ.to_bit().to_size();
                Some(*acc)
            })
            .collect::<Vec<_>>();

        let mut gen = FuncGenerator {
            buf: Buf::new(),
            local_offsets,
            locals: func.locals.clone(),
            func_name: func.ident.clone(),
        };
        gen.decl_func(func);
        gen.buf
    }

    fn decl_func(&mut self, func: DeclFunc) {
        let locals_size = func
            .locals
            .iter()
            .map(|local| local.typ.to_bit().to_size())
            .sum::<usize>();

        self.buf += format!("{}:\n", func.ident);
        self.buf += "push rbp\n";
        self.buf += "mov rbp, rsp\n";
        let stack_size = locals_size + 8 /* rbpの分 */;
        let locals_pad = if stack_size % 16 == 0 {
            0
        } else {
            16 - stack_size % 16
        };
        self.buf += format!("sub rsp, {}\n", locals_size + locals_pad);
        if func.args_count >= 1 {
            self.buf += format!("mov {}, edi\n", self.local(0));
        }
        if func.args_count >= 2 {
            self.buf += format!("mov {}, esi\n", self.local(1));
        }
        if func.args_count >= 3 {
            self.buf += format!("mov {}, edx\n", self.local(2));
        }
        if func.args_count >= 4 {
            self.buf += format!("mov {}, ecx\n", self.local(3));
        }
        if func.args_count >= 5 {
            self.buf += format!("mov {}, r8d\n", self.local(4));
        }
        if func.args_count >= 6 {
            self.buf += format!("mov {}, r9d\n", self.local(5));
        }
        if func.args_count >= 7 {
            for i in 0..(func.args_count - 6) {
                self.buf += format!("mov eax, DWORD PTR [rbp+{}]\n", 4 * i + 16);
                self.buf += format!("mov {}, eax\n", self.local(6 + i),);
            }
        }
        for bb in func.bbs {
            self.bb(bb);
        }
    }

    fn local(&self, local: usize) -> String {
        format!(
            "{} PTR [rbp-{}]",
            match self.locals[local].typ.to_bit() {
                Bit::Bit8 => "BYTE",
                Bit::Bit16 => "WORD",
                Bit::Bit32 => "DWORD",
                Bit::Bit64 => "QWORD",
            },
            self.local_offsets[local]
        )
    }

    fn bb(&mut self, bb: tac::BB) {
        self.buf += format!(".BB.{}.{}:\n", self.func_name, usize::from(bb.id));
        for instr in bb.instrs {
            self.instr(instr);
        }
    }

    fn instr(&mut self, instr: Instr) {
        use Instr::*;
        match instr {
            IntConst(x) => self.instr_int_const(x),
            BinOp(x) => self.instr_bin_op(x),
            UnOp(x) => self.instr_un_op(x),
            AssignIndirect(x) => self.instr_assign_indirect(x),
            Call(x) => self.instr_call(x),
            AssignLocal(x) => self.instr_assign_local(x),
            Nop => {}
            Term(x) => self.instr_term(x),
        }
    }

    fn instr_int_const(&mut self, x: tac::InstrIntConst) {
        self.buf += format!("mov {}, {}\n", self.local(x.dst), x.value);
    }

    fn instr_bin_op(&mut self, x: tac::InstrBinOp) {
        use tac::BinOp::*;
        match x.op {
            Add => self.bin_op_add(x.lhs, x.rhs, x.dst),
            Sub => self.bin_op_sub(x.lhs, x.rhs, x.dst),
            Mul => self.bin_op_mul(x.lhs, x.rhs, x.dst),
            Div => self.bin_op_div(x.lhs, x.rhs, x.dst),
            Eq => self.bin_op_eq(x.lhs, x.rhs, x.dst),
            Ne => self.bin_op_ne(x.lhs, x.rhs, x.dst),
            Lt => self.bin_op_lt(x.lhs, x.rhs, x.dst),
            Le => self.bin_op_le(x.lhs, x.rhs, x.dst),
        }
    }

    fn bin_op_add(&mut self, lhs: usize, rhs: usize, dst: usize) {
        self.buf += format!("mov eax, {}\n", self.local(lhs));
        self.buf += format!("mov edi, {}\n", self.local(rhs));
        self.buf += "add rax, rdi\n";
        self.buf += format!("mov {}, eax\n", self.local(dst));
    }

    fn bin_op_sub(&mut self, lhs: usize, rhs: usize, dst: usize) {
        self.buf += format!("mov eax, {}\n", self.local(lhs));
        self.buf += format!("mov edi, {}\n", self.local(rhs));
        self.buf += "sub rax, rdi\n";
        self.buf += format!("mov {}, eax\n", self.local(dst));
    }

    fn bin_op_mul(&mut self, lhs: usize, rhs: usize, dst: usize) {
        self.buf += format!("mov eax, {}\n", self.local(lhs));
        self.buf += format!("mov edi, {}\n", self.local(rhs));
        self.buf += "imul rax, rdi\n";
        self.buf += format!("mov {}, eax\n", self.local(dst));
    }

    fn bin_op_div(&mut self, lhs: usize, rhs: usize, dst: usize) {
        self.buf += format!("mov eax, {}\n", self.local(lhs));
        self.buf += format!("mov edi, {}\n", self.local(rhs));
        self.buf += "cqo\n";
        self.buf += "idiv edi\n";
        self.buf += format!("mov {}, eax\n", self.local(dst));
    }

    fn bin_op_eq(&mut self, lhs: usize, rhs: usize, dst: usize) {
        self.buf += format!("mov edi, {}\n", self.local(rhs));
        self.buf += format!("mov eax, {}\n", self.local(lhs));
        self.buf += "cmp eax, edi\n";
        self.buf += "sete al\n";
        self.buf += "movzx eax, al\n";
        self.buf += format!("mov {}, eax\n", self.local(dst));
    }

    fn bin_op_ne(&mut self, lhs: usize, rhs: usize, dst: usize) {
        self.buf += format!("mov edi, {}\n", self.local(rhs));
        self.buf += format!("mov eax, {}\n", self.local(lhs));
        self.buf += "cmp eax, edi\n";
        self.buf += "setne al\n";
        self.buf += "movzx eax, al\n";
        self.buf += format!("mov {}, eax\n", self.local(dst));
    }

    fn bin_op_lt(&mut self, lhs: usize, rhs: usize, dst: usize) {
        self.buf += format!("mov edi, {}\n", self.local(rhs));
        self.buf += format!("mov eax, {}\n", self.local(lhs));
        self.buf += "cmp eax, edi\n";
        self.buf += "setl al\n";
        self.buf += "movzx eax, al\n";
        self.buf += format!("mov {}, eax\n", self.local(dst));
    }

    fn bin_op_le(&mut self, lhs: usize, rhs: usize, dst: usize) {
        self.buf += format!("mov edi, {}\n", self.local(rhs));
        self.buf += format!("mov eax, {}\n", self.local(lhs));
        self.buf += "cmp eax, edi\n";
        self.buf += "setle al\n";
        self.buf += "movzx eax, al\n";
        self.buf += format!("mov {}, eax\n", self.local(dst));
    }

    fn instr_un_op(&mut self, x: tac::InstrUnOp) {
        use tac::UnOp::*;
        match x.op {
            Neg => self.un_op_neg(x.src, x.dst),
            LocalAddr => self.un_op_local_addr(x.src, x.dst),
            Deref => self.un_op_deref(x.src, x.dst),
        }
    }

    fn un_op_neg(&mut self, src: usize, dst: usize) {
        self.buf += format!("mov eax, {}\n", self.local(src));
        self.buf += "neg eax\n";
        self.buf += format!("mov {}, eax\n", self.local(dst));
    }

    fn un_op_local_addr(&mut self, src: usize, dst: usize) {
        self.buf += format!("lea rax, {}\n", self.local(src));
        self.buf += format!("mov {}, rax\n", self.local(dst));
    }

    fn un_op_deref(&mut self, src: usize, dst: usize) {
        let dst_type = self.locals[dst].typ.clone();
        let ax = Register::Rax.for_bit(dst_type.to_bit());
        self.buf += format!("mov rax, {}\n", self.local(src));
        self.buf += format!("mov {ax}, [rax]\n");
        self.buf += format!("mov {}, {ax}\n", self.local(dst));
    }

    fn instr_assign_indirect(&mut self, x: tac::InstrAssignIndirect) {
        let src_bit = self.locals[x.src].typ.to_bit();
        let di = Register::Rdi.for_bit(src_bit);
        let src_word = bit_to_word(src_bit);
        self.buf += format!("mov rax, {}\n", self.local(x.dst_ref));
        self.buf += format!("mov {di}, {}\n", self.local(x.src));
        self.buf += format!("mov {src_word} PTR [rax], {di}\n");
    }

    fn instr_call(&mut self, x: tac::InstrCall) {
        if let Some(arg) = x.args.get(0) {
            self.buf += format!("mov edi, {}\n", self.local(*arg));
        }

        if let Some(arg) = x.args.get(1) {
            self.buf += format!("mov esi, {}\n", self.local(*arg));
        }

        if let Some(arg) = x.args.get(2) {
            self.buf += format!("mov edx, {}\n", self.local(*arg));
        }

        if let Some(arg) = x.args.get(3) {
            self.buf += format!("mov ecx, {}\n", self.local(*arg));
        }

        if let Some(arg) = x.args.get(4) {
            self.buf += format!("mov r8d, {}\n", self.local(*arg));
        }

        if let Some(arg) = x.args.get(5) {
            self.buf += format!("mov r9d, {}\n", self.local(*arg));
        }

        let extra_args_size = if x.args.len() > 6 {
            let extra_args_size = x
                .args
                .iter()
                .skip(6)
                .map(|&_arg|
                    // TODO:
                    4)
                .sum::<usize>();
            let extra_args_size = extra_args_size + (extra_args_size % 16);
            self.buf += format!("sub rsp, {}\n", extra_args_size);
            for (i, arg) in x.args.iter().enumerate().skip(6) {
                self.buf += format!("mov eax, {}\n", self.local(*arg));
                self.buf += format!("mov DWORD PTR [rsp + {}], eax\n", (i - 6) * 4);
            }
            extra_args_size
        } else {
            0
        };

        self.buf += format!("call {}\n", x.ident);
        self.buf += format!("add rsp, {}\n", extra_args_size);
        self.buf += format!("mov {}, eax\n", self.local(x.dst));
    }

    fn instr_assign_local(&mut self, x: tac::InstrAssignLocal) {
        let ax = Register::Rax.for_bit(self.locals[x.src].typ.to_bit());
        self.buf += format!("mov {ax}, {}\n", self.local(x.src));
        self.buf += format!("mov {}, {ax}\n", self.local(x.dst));
    }

    fn instr_term(&mut self, x: tac::InstrTerm) {
        match x {
            tac::InstrTerm::Jump { id } => {
                // TODO: idが次のBBなら省略
                self.buf += format!("jmp .BB.{}.{}\n", self.func_name, usize::from(id));
            }
            tac::InstrTerm::JumpIf {
                cond,
                then_id,
                else_id,
            } => {
                self.buf += format!("mov eax, {}\n", self.local(cond));
                self.buf += "cmp eax, 0\n";
                self.buf += format!("je .BB.{}.{}\n", self.func_name, usize::from(else_id));
                // TODO: then_idが次のBBなら省略
                self.buf += format!("jmp .BB.{}.{}\n", self.func_name, usize::from(then_id));
            }
            tac::InstrTerm::Return { src } => {
                self.buf += format!("mov eax, {}\n", self.local(src));
                self.buf += "leave\n";
                self.buf += "ret\n";
            }
        }
    }
}

pub fn generate(program: Program) -> Vec<u8> {
    let mut gen = Generator::new();
    gen.program(program);
    gen.buf.to_vec()
}
