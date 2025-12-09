use crate::code_gen::{AsmAst, AsmFunction, AsmInstruction, AsmOperand, AsmRegister, AsmUnaryOp};

pub struct CodeEmission {}

impl CodeEmission {
    pub fn new() -> Self {
        CodeEmission {}
    }

    pub fn generate_code(&mut self, asm_ast: &AsmAst) -> Vec<String> {
        let mut instructions = Vec::new();

        match asm_ast {
            AsmAst::AsmProgram(func) => self.emit_asm_function(func, &mut instructions),
        }

        instructions
    }

    pub fn emit_asm_function(&mut self, asm_function: &AsmFunction, asm_code: &mut Vec<String>) {
        asm_code.push("\t.intel_syntax noprefix\n".to_string());
        asm_code.push(format!("\t.globl _{}\n", asm_function.name));
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
                AsmInstruction::Unary { unary_op, operand } => {
                    asm_code.push(format!(
                        "\t{} {}\n",
                        self.asm_unary_operator_to_string(unary_op),
                        self.asm_operand_to_string(operand)
                    ));
                }
            }
        }
    }

    pub fn asm_operand_to_string(&mut self, asm_operand: &AsmOperand) -> String {
        match asm_operand {
            AsmOperand::Imm(val) => val.to_string(),
            AsmOperand::Reg(reg) => self.asm_reg_to_string(reg),
            AsmOperand::Stack(val) => {
                format!("DWORD PTR [rbp - {}]", val.abs())
            }
        }
    }
    pub fn asm_unary_operator_to_string(&mut self, asm_op: &AsmUnaryOp) -> String {
        match asm_op {
            AsmUnaryOp::Neg => "neg".to_string(),
            AsmUnaryOp::Not => "not".to_string(),
        }
    }

    pub fn asm_reg_to_string(&mut self, asm_reg: &AsmRegister) -> String {
        match asm_reg {
            AsmRegister::EAX => "eax".to_string(),
            AsmRegister::R10d => "r10d".to_string(),
        }
    }
}
