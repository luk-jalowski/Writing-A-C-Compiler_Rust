use core::panic;

use crate::code_gen::{
    AsmAst, AsmBinaryOp, AsmConditional, AsmFunction, AsmInstruction, AsmOperand, AsmRegister,
    AsmUnaryOp,
};

pub struct CodeEmission {}

impl CodeEmission {
    pub fn new() -> Self {
        CodeEmission {}
    }

    pub fn generate_code(&mut self, asm_ast: &AsmAst) -> Vec<String> {
        let mut instructions = Vec::new();

        match asm_ast {
            AsmAst::AsmProgram(functions) => {
                instructions.push("\t.intel_syntax noprefix\n".to_string());
                for func in functions {
                    instructions.push(format!("\t.globl _{}\n", func.name));
                }

                for func in functions {
                    self.emit_asm_function(func, &mut instructions);
                }
            }
        }

        instructions
    }

    pub fn emit_asm_function(&mut self, asm_function: &AsmFunction, asm_code: &mut Vec<String>) {
        asm_code.push(format!("_{}:\n", asm_function.name));

        //         for instr in &self.asm_instructions {
        //             write!(f, "{}", instr)?;
        //         }
        self.emit_asm_instructions(&asm_function.body, asm_code);
    }

    pub fn emit_asm_instructions(
        &mut self,
        asm_instructions: &Vec<AsmInstruction>,
        asm_code: &mut Vec<String>,
    ) {
        asm_code.push(format!("\tpush rbp\n"));
        asm_code.push(format!("\tmov rbp, rsp\n"));

        for instruction in asm_instructions {
            match instruction {
                AsmInstruction::AllocateStack(offset) => {
                    asm_code.push(format!("\tsub rsp, {}\n", offset.abs()));
                }
                AsmInstruction::DeallocateStack(offset) => {
                    asm_code.push(format!("\tadd rsp, {}\n", offset.abs()));
                }
                AsmInstruction::Mov { src, dst } => {
                    asm_code.push(format!(
                        "\tmov {}, {}\n",
                        self.asm_operand_to_string(dst),
                        self.asm_operand_to_string(src)
                    ));
                }
                AsmInstruction::Ret => {
                    asm_code.push(format!("\tmov rsp, rbp\n"));
                    asm_code.push(format!("\tpop rbp\n"));
                    asm_code.push(format!("\tret\n"));
                }
                AsmInstruction::Unary { op, operand } => {
                    asm_code.push(format!(
                        "\t{} {}\n",
                        self.asm_unary_operator_to_string(op),
                        self.asm_operand_to_string(operand)
                    ));
                }
                AsmInstruction::Binary { op, left, right } => {
                    asm_code.push(format!(
                        "\t{} {}, {}\n",
                        self.asm_binary_operator_to_string(op),
                        self.asm_operand_to_string(left),
                        self.asm_operand_to_string(right)
                    ));
                }
                AsmInstruction::Idiv(operand) => {
                    asm_code.push(format!("\tidiv {}\n", self.asm_operand_to_string(operand)));
                }
                AsmInstruction::Cdq => {
                    asm_code.push(format!("\tcdq\n"));
                }
                AsmInstruction::Cmp { left, right } => {
                    asm_code.push(format!(
                        "\tcmp {}, {}\n",
                        self.asm_operand_to_string(left),
                        self.asm_operand_to_string(right)
                    ));
                }
                AsmInstruction::Label(label) => {
                    asm_code.push(format!(".{}:\n", label.clone()));
                }
                AsmInstruction::Jmp(label) => {
                    asm_code.push(format!("jmp .{}\n", label.clone()));
                }
                AsmInstruction::JmpCC { condition, target } => {
                    let cc = self.asm_conditional_to_string(condition);
                    asm_code.push(format!("j{} .{}\n", cc.clone(), target.clone()));
                }
                AsmInstruction::SetCC { condition, operand } => {
                    let cc = self.asm_conditional_to_string(condition);
                    let operand_str = match operand {
                        AsmOperand::Stack(val) => {
                            if *val < 0 {
                                format!("BYTE PTR [rbp - {}]", val.abs())
                            } else {
                                format!("BYTE PTR [rbp + {}]", val)
                            }
                        }
                        AsmOperand::Reg(reg) => self.asm_reg_to_string(reg, 1),
                        _ => panic!("Invalid operand for SetCC: {:?}", operand),
                    };
                    asm_code.push(format!("set{} {}\n", cc.clone(), operand_str));
                }
                // This is targeting 64b systems, so needs to be 8 byte aligned no matter the registers used
                AsmInstruction::Pop(operand) => match operand {
                    AsmOperand::Reg(reg) => {
                        asm_code.push(format!("\tpop {}\n", self.asm_reg_to_string(reg, 8)));
                    }
                    _ => asm_code.push(format!("\tpop {}\n", self.asm_operand_to_string(operand))),
                },
                AsmInstruction::Push(operand) => match operand {
                    AsmOperand::Reg(reg) => {
                        asm_code.push(format!("\tpush {}\n", self.asm_reg_to_string(reg, 8)));
                    }
                    _ => asm_code.push(format!("\tpush {}\n", self.asm_operand_to_string(operand))),
                },
                AsmInstruction::Call(label) => {
                    asm_code.push(format!("\tcall _{}\n", label));
                }
            }
        }
    }

    pub fn asm_operand_to_string(&mut self, asm_operand: &AsmOperand) -> String {
        match asm_operand {
            AsmOperand::Imm(val) => val.to_string(),
            AsmOperand::Reg(reg) => self.asm_reg_to_string(reg, 4),
            AsmOperand::Stack(val) => {
                if *val < 0 {
                    format!("DWORD PTR [rbp - {}]", val.abs())
                } else {
                    format!("DWORD PTR [rbp + {}]", val)
                }
            }
        }
    }
    pub fn asm_unary_operator_to_string(&mut self, asm_op: &AsmUnaryOp) -> String {
        match asm_op {
            AsmUnaryOp::Neg => "neg".to_string(),
            AsmUnaryOp::Not => "not".to_string(),
        }
    }
    pub fn asm_binary_operator_to_string(&mut self, asm_op: &AsmBinaryOp) -> String {
        match asm_op {
            AsmBinaryOp::Add => "add".to_string(),
            AsmBinaryOp::Mult => "imul".to_string(),
            AsmBinaryOp::Sub => "sub".to_string(),
        }
    }

    pub fn asm_reg_to_string(&mut self, asm_reg: &AsmRegister, byte_size: usize) -> String {
        if byte_size == 8 {
            match asm_reg {
                AsmRegister::RAX => "rax".to_string(),
                AsmRegister::RCX => "rcx".to_string(),
                AsmRegister::RDX => "rdx".to_string(),
                AsmRegister::RDI => "rdi".to_string(),
                AsmRegister::RSI => "rsi".to_string(),
                AsmRegister::R8 => "r8".to_string(),
                AsmRegister::R9 => "r9".to_string(),
                AsmRegister::R10 => "r10".to_string(),
                AsmRegister::R11 => "r11".to_string(),
            }
        } else if byte_size == 4 {
            match asm_reg {
                AsmRegister::RAX => "eax".to_string(),
                AsmRegister::RCX => "ecx".to_string(),
                AsmRegister::RDX => "edx".to_string(),
                AsmRegister::RDI => "edi".to_string(),
                AsmRegister::RSI => "esi".to_string(),
                AsmRegister::R8 => "r8d".to_string(),
                AsmRegister::R9 => "r9d".to_string(),
                AsmRegister::R10 => "r10d".to_string(),
                AsmRegister::R11 => "r11d".to_string(),
            }
        } else {
            match asm_reg {
                AsmRegister::RAX => "ax".to_string(),
                AsmRegister::RCX => "cx".to_string(),
                AsmRegister::RDX => "dx".to_string(),
                AsmRegister::RDI => "di".to_string(),
                AsmRegister::RSI => "si".to_string(),
                AsmRegister::R8 => "r8b".to_string(),
                AsmRegister::R9 => "r9b".to_string(),
                AsmRegister::R10 => "r10b".to_string(),
                AsmRegister::R11 => "r11b".to_string(),
            }
        }
    }

    pub fn asm_conditional_to_string(&mut self, asm_cc: &AsmConditional) -> String {
        match asm_cc {
            AsmConditional::Equal => "e".to_string(),
            AsmConditional::NotEqual => "ne".to_string(),
            AsmConditional::Less => "l".to_string(),
            AsmConditional::LessOrEqual => "le".to_string(),
            AsmConditional::Greater => "g".to_string(),
            AsmConditional::GreaterOrEqual => "ge".to_string(),
        }
    }
}
