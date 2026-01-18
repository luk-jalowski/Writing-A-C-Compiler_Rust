use core::panic;

use crate::{
    code_gen::{
        AsmAst, AsmBinaryOp, AsmConditional, AsmFunction, AsmInstruction, AsmOperand, AsmRegister,
        AsmTopLevel, AsmUnaryOp, AssemblyType,
    },
    type_validation::StaticInit,
};

pub struct CodeEmission {}

impl Default for CodeEmission {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeEmission {
    pub fn new() -> Self {
        CodeEmission {}
    }

    pub fn generate_code(&mut self, asm_ast: &AsmAst) -> Vec<String> {
        let mut instructions = Vec::new();

        match asm_ast {
            AsmAst::AsmProgram(top_level) => {
                instructions.push("\t.intel_syntax noprefix\n".to_string());

                for item in top_level {
                    if let AsmTopLevel::Function(func) = item {
                        if func.global {
                            instructions.push(format!("\t.globl _{}\n", func.name));
                        }
                        self.emit_asm_function(func, &mut instructions);
                    }
                }

                instructions.push("\t.data\n".to_string());
                for item in top_level {
                    if let AsmTopLevel::StaticVariable {
                        name,
                        init,
                        global,
                        alignment,
                    } = item
                    {
                        if *global {
                            instructions.push(format!("\t.globl _{}\n", name));
                        }

                        let init_val = match init {
                            StaticInit::IntInit(val) => *val as i64,
                            StaticInit::UIntInit(val) => *val as i64,
                            StaticInit::LongInit(val) => *val,
                            StaticInit::ULongInit(val) => *val as i64,
                            StaticInit::Double(val) => val.to_bits() as i64,
                        };

                        if init_val == 0 && !matches!(init, StaticInit::Double(_)) {
                            instructions.push("\t.bss\n".to_string());
                            instructions.push(format!("\t.balign {}\n", alignment)); // MACOS or Linux directive
                            instructions.push(format!("_{}:\n", name));
                            match init {
                                StaticInit::IntInit(_) | StaticInit::UIntInit(_) => {
                                    instructions.push("\t.zero 4\n".to_string())
                                }
                                StaticInit::LongInit(_) | StaticInit::ULongInit(_) => {
                                    instructions.push("\t.zero 8\n".to_string())
                                }
                                StaticInit::Double(_) => unreachable!(),
                            }
                        } else {
                            instructions.push("\t.data\n".to_string());
                            instructions.push(format!("\t.balign {}\n", alignment));
                            instructions.push(format!("_{}:\n", name));
                            match init {
                                StaticInit::IntInit(val) => {
                                    instructions.push(format!("\t.long {}\n", val))
                                }
                                StaticInit::UIntInit(val) => {
                                    instructions.push(format!("\t.long {}\n", val))
                                }
                                StaticInit::LongInit(val) => {
                                    instructions.push(format!("\t.quad {}\n", val))
                                }
                                StaticInit::ULongInit(val) => {
                                    instructions.push(format!("\t.quad {}\n", val))
                                }
                                StaticInit::Double(val) => {
                                    instructions.push(format!("\t.quad {}\n", val.to_bits()))
                                }
                            }
                        }
                    } else if let AsmTopLevel::StaticConstant {
                        name,
                        init,
                        alignment,
                    } = item
                    {
                        instructions.push("\t.data\n".to_string());
                        instructions.push(format!("\t.balign {}\n", alignment));
                        instructions.push(format!("_{}:\n", name));
                        match init {
                            StaticInit::Double(val) => {
                                if name == "negative_zero" {
                                    instructions.push(format!("\t.quad {}\n", val.to_bits()));
                                    instructions.push("\t.quad 0\n".to_string());
                                } else {
                                    instructions.push(format!("\t.quad {}\n", val.to_bits()));
                                }
                            }
                            _ => {
                                let init_val = match init {
                                    StaticInit::IntInit(val) => *val as i64,
                                    StaticInit::UIntInit(val) => *val as i64,
                                    StaticInit::LongInit(val) => *val,
                                    StaticInit::ULongInit(val) => *val as i64,
                                    StaticInit::Double(val) => val.to_bits() as i64,
                                };
                                instructions.push(format!("\t.quad {}\n", init_val));
                            }
                        }
                    }
                }
            }
        }

        instructions
    }

    fn emit_asm_function(&mut self, asm_function: &AsmFunction, asm_code: &mut Vec<String>) {
        asm_code.push(format!("_{}:\n", asm_function.name));

        self.emit_asm_instructions(&asm_function.body, asm_code);
    }

    fn emit_asm_instructions(
        &mut self,
        asm_instructions: &Vec<AsmInstruction>,
        asm_code: &mut Vec<String>,
    ) {
        asm_code.push("\tpush rbp\n".to_string());
        asm_code.push("\tmov rbp, rsp\n".to_string());

        for instruction in asm_instructions {
            match instruction {
                AsmInstruction::AllocateStack(offset) => {
                    asm_code.push(format!("\tsub rsp, {}\n", offset.abs()));
                }
                AsmInstruction::DeallocateStack(offset) => {
                    asm_code.push(format!("\tadd rsp, {}\n", offset.abs()));
                }
                AsmInstruction::Mov { src, dst, asm_type } => {
                    if *asm_type == AssemblyType::Double {
                        asm_code.push(format!(
                            "\tmovsd {}, {}\n",
                            self.asm_operand_to_string(dst, asm_type),
                            self.asm_operand_to_string(src, asm_type)
                        ));
                    } else {
                        asm_code.push(format!(
                            "\tmov {}, {}\n",
                            self.asm_operand_to_string(dst, asm_type),
                            self.asm_operand_to_string(src, asm_type)
                        ));
                    }
                }
                AsmInstruction::Movsx { src, dst } => {
                    asm_code.push(format!(
                        "\tmovsxd {}, {}\n",
                        self.asm_operand_to_string(dst, &AssemblyType::Qword),
                        self.asm_operand_to_string(src, &AssemblyType::Dword)
                    ));
                }
                AsmInstruction::MovZeroExtend { src, dst } => {
                    asm_code.push(format!(
                        "\tmovzx {}, {}\n",
                        self.asm_operand_to_string(dst, &AssemblyType::Qword),
                        self.asm_operand_to_string(src, &AssemblyType::Dword)
                    ));
                }
                AsmInstruction::Ret => {
                    asm_code.push("\tmov rsp, rbp\n".to_string());
                    asm_code.push("\tpop rbp\n".to_string());
                    asm_code.push("\tret\n".to_string());
                }
                AsmInstruction::Unary {
                    op,
                    operand,
                    asm_type,
                } => {
                    asm_code.push(format!(
                        "\t{} {}\n",
                        self.asm_unary_operator_to_string(op),
                        self.asm_operand_to_string(operand, asm_type)
                    ));
                }
                AsmInstruction::Binary {
                    op,
                    left,
                    right,
                    asm_type,
                } => {
                    if *op == AsmBinaryOp::Xor && *asm_type == AssemblyType::Double {
                        let right_str = if let AsmOperand::Data(name) = right {
                            format!("XMMWORD PTR [rip + _{}]", name)
                        } else {
                            self.asm_operand_to_string(right, asm_type)
                        };
                        asm_code.push(format!(
                            "\txorpd {}, {}\n",
                            self.asm_operand_to_string(left, asm_type),
                            right_str
                        ));
                    } else {
                        asm_code.push(format!(
                            "\t{} {}, {}\n",
                            self.asm_binary_operator_to_string(op, asm_type),
                            self.asm_operand_to_string(left, asm_type),
                            self.asm_operand_to_string(right, asm_type)
                        ));
                    }
                }
                AsmInstruction::Idiv { operand, asm_type } => {
                    asm_code.push(format!(
                        "\tidiv {}\n",
                        self.asm_operand_to_string(operand, asm_type)
                    ));
                }
                AsmInstruction::Div { operand, asm_type } => {
                    asm_code.push(format!(
                        "\tdiv {}\n",
                        self.asm_operand_to_string(operand, asm_type)
                    ));
                }
                AsmInstruction::Cdq { asm_type } => match asm_type {
                    AssemblyType::Dword => asm_code.push("\tcdq\n".to_string()),
                    AssemblyType::Qword | AssemblyType::Double => {
                        asm_code.push("\tcqo\n".to_string())
                    }
                },
                AsmInstruction::Cmp {
                    left,
                    right,
                    asm_type,
                } => {
                    if *asm_type == AssemblyType::Double {
                        asm_code.push(format!(
                            "\tcomisd {}, {}\n",
                            self.asm_operand_to_string(left, asm_type),
                            self.asm_operand_to_string(right, asm_type)
                        ));
                    } else {
                        asm_code.push(format!(
                            "\tcmp {}, {}\n",
                            self.asm_operand_to_string(left, asm_type),
                            self.asm_operand_to_string(right, asm_type)
                        ));
                    }
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
                        AsmOperand::Data(name) => format!("BYTE PTR [rip + _{}]", name),
                        _ => panic!("Invalid operand for SetCC: {:?}", operand),
                    };
                    asm_code.push(format!("set{} {}\n", cc.clone(), operand_str));
                }
                // This is targeting 64b systems, so needs to be 8 byte aligned no matter the registers used
                AsmInstruction::Pop(operand) => match operand {
                    AsmOperand::Reg(reg) => {
                        asm_code.push(format!("\tpop {}\n", self.asm_reg_to_string(reg, 8)));
                    }
                    _ => asm_code.push(format!(
                        "\tpop {}\n",
                        self.asm_operand_to_string(operand, &AssemblyType::Qword)
                    )),
                },
                AsmInstruction::Push(operand) => match operand {
                    AsmOperand::Reg(reg) => {
                        asm_code.push(format!("\tpush {}\n", self.asm_reg_to_string(reg, 8)));
                    }
                    _ => asm_code.push(format!(
                        "\tpush {}\n",
                        self.asm_operand_to_string(operand, &AssemblyType::Qword)
                    )),
                },
                AsmInstruction::Call(label) => {
                    asm_code.push(format!("\tcall _{}\n", label));
                }
                AsmInstruction::Cvtsi2sd { src, dst, asm_type } => {
                    asm_code.push(format!(
                        "\tcvtsi2sd {}, {}\n",
                        self.asm_operand_to_string(dst, &AssemblyType::Double),
                        self.asm_operand_to_string(src, asm_type)
                    ));
                }
                AsmInstruction::Cvttsd2si { src, dst, asm_type } => {
                    asm_code.push(format!(
                        "\tcvttsd2si {}, {}\n",
                        self.asm_operand_to_string(dst, asm_type),
                        self.asm_operand_to_string(src, &AssemblyType::Double)
                    ));
                }
            }
        }
    }

    fn asm_operand_to_string(
        &mut self,
        asm_operand: &AsmOperand,
        asm_type: &AssemblyType,
    ) -> String {
        let ptr_size = match asm_type {
            AssemblyType::Dword => "DWORD PTR",
            AssemblyType::Qword | AssemblyType::Double => "QWORD PTR",
        };
        let byte_size = match asm_type {
            AssemblyType::Dword => 4,
            AssemblyType::Qword => 8,
            AssemblyType::Double => 8,
        };
        match asm_operand {
            AsmOperand::Imm(val) => val.to_string(),
            AsmOperand::Reg(reg) => self.asm_reg_to_string(reg, byte_size),
            AsmOperand::Stack(val) => {
                if *val < 0 {
                    format!("{} [rbp - {}]", ptr_size, val.abs())
                } else {
                    format!("{} [rbp + {}]", ptr_size, val)
                }
            }
            AsmOperand::Data(name) => format!("{} [rip + _{}]", ptr_size, name),
        }
    }
    fn asm_unary_operator_to_string(&mut self, asm_op: &AsmUnaryOp) -> String {
        match asm_op {
            AsmUnaryOp::Neg => "neg".to_string(),
            AsmUnaryOp::Not => "not".to_string(),
            AsmUnaryOp::Shr => "shr".to_string(),
        }
    }

    fn asm_binary_operator_to_string(
        &mut self,
        asm_op: &AsmBinaryOp,
        asm_type: &AssemblyType,
    ) -> String {
        if *asm_type == AssemblyType::Double {
            match asm_op {
                AsmBinaryOp::Add => "addsd".to_string(),
                AsmBinaryOp::Sub => "subsd".to_string(),
                AsmBinaryOp::Mult => "mulsd".to_string(),
                AsmBinaryOp::Xor => "xorpd".to_string(),
                AsmBinaryOp::DivDouble => "divsd".to_string(),
                _ => panic!("Unsupported binary operation on double"),
            }
        } else {
            // Integer types
            match asm_op {
                AsmBinaryOp::Add => "add".to_string(),
                AsmBinaryOp::Sub => "sub".to_string(),
                AsmBinaryOp::Mult => "imul".to_string(),
                AsmBinaryOp::DivDouble => "idiv".to_string(),
                AsmBinaryOp::Or => "or".to_string(),
                AsmBinaryOp::Xor => "xor".to_string(),
                AsmBinaryOp::And => "and".to_string(),
            }
        }
    }

    fn asm_reg_to_string(&mut self, asm_reg: &AsmRegister, byte_size: usize) -> String {
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
                AsmRegister::RSP => "rsp".to_string(),
                AsmRegister::XMM0 => "xmm0".to_string(),
                AsmRegister::XMM1 => "xmm1".to_string(),
                AsmRegister::XMM2 => "xmm2".to_string(),
                AsmRegister::XMM3 => "xmm3".to_string(),
                AsmRegister::XMM4 => "xmm4".to_string(),
                AsmRegister::XMM5 => "xmm5".to_string(),
                AsmRegister::XMM6 => "xmm6".to_string(),
                AsmRegister::XMM7 => "xmm7".to_string(),
                AsmRegister::XMM14 => "xmm14".to_string(),
                AsmRegister::XMM15 => "xmm15".to_string(),
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
                AsmRegister::RSP => "esp".to_string(),
                AsmRegister::XMM0 => "xmm0".to_string(),
                AsmRegister::XMM1 => "xmm1".to_string(),
                AsmRegister::XMM2 => "xmm2".to_string(),
                AsmRegister::XMM3 => "xmm3".to_string(),
                AsmRegister::XMM4 => "xmm4".to_string(),
                AsmRegister::XMM5 => "xmm5".to_string(),
                AsmRegister::XMM6 => "xmm6".to_string(),
                AsmRegister::XMM7 => "xmm7".to_string(),
                AsmRegister::XMM14 => "xmm14".to_string(),
                AsmRegister::XMM15 => "xmm15".to_string(),
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
                AsmRegister::RSP => "spl".to_string(),
                AsmRegister::XMM0 => "xmm0".to_string(),
                AsmRegister::XMM1 => "xmm1".to_string(),
                AsmRegister::XMM2 => "xmm2".to_string(),
                AsmRegister::XMM3 => "xmm3".to_string(),
                AsmRegister::XMM4 => "xmm4".to_string(),
                AsmRegister::XMM5 => "xmm5".to_string(),
                AsmRegister::XMM6 => "xmm6".to_string(),
                AsmRegister::XMM7 => "xmm7".to_string(),
                AsmRegister::XMM14 => "xmm14".to_string(),
                AsmRegister::XMM15 => "xmm15".to_string(),
            }
        }
    }

    fn asm_conditional_to_string(&mut self, asm_cc: &AsmConditional) -> String {
        match asm_cc {
            AsmConditional::Equal => "e".to_string(),
            AsmConditional::NotEqual => "ne".to_string(),
            AsmConditional::Less => "l".to_string(),
            AsmConditional::LessOrEqual => "le".to_string(),
            AsmConditional::Greater => "g".to_string(),
            AsmConditional::GreaterOrEqual => "ge".to_string(),
            AsmConditional::Above => "a".to_string(),
            AsmConditional::AboveOrEqual => "ae".to_string(),
            AsmConditional::Below => "b".to_string(),
            AsmConditional::BelowOrEqual => "be".to_string(),
        }
    }
}
