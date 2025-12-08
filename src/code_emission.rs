use crate::code_gen::{AsmProgram, Instruction, Operand, Register};
use std::fmt;

// We use intel syntax for x86_64
impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::AX => write!(f, "ax"),
            Register::EAX => write!(f, "eax"),
            Register::RAX => write!(f, "rax"),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::MOV { src, dst } => {
                write!(f, "\tmov {}, {}\n", dst, src)
            }
            Instruction::Ret => write!(f, "\tret\n"),
            Instruction::Label(name) => write!(f, "_{}:\n", name),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Imm(val) => write!(f, "{}", val),
            Operand::Reg(reg) => write!(f, "{}", reg),
        }
    }
}

impl fmt::Display for AsmProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "\t.intel_syntax noprefix")?;
        writeln!(f, "\t.globl _main")?;

        for instr in &self.instructions {
            write!(f, "{}", instr)?;
        }
        Ok(())
    }
}
