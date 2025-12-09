use std::collections::HashMap;

use crate::{
    tac::{TacAst, TacFunction, TacInstruction, TacOperand, TacUnaryOp},
};

#[derive(Debug)]
pub enum AsmAst {
    AsmProgram(AsmFunction),
}

#[derive(Debug)]
pub struct AsmFunction {
    pub name: String,
    pub body: Vec<AsmInstruction>,
}

#[derive(Debug)]
pub enum AsmInstruction {
    Mov {
        src: AsmOperand,
        dst: AsmOperand,
    },
    Unary {
        unary_op: AsmUnaryOp,
        operand: AsmOperand,
    },
    AllocateStack(i32),
    Ret,
}

#[derive(Debug, Clone)]
pub enum AsmOperand {
    Reg(AsmRegister),
    Imm(i32),
    Stack(i32),
}

#[derive(Debug)]
pub enum AsmUnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum AsmRegister {
    EAX,
    R10d,
}

#[derive(Debug)]
pub struct AsmProgram {
    pub asm_program: AsmFunction,
    stack_map: HashMap<String, i32>,
    stack_index: i32,
}

impl AsmProgram {
    pub fn new() -> Self {
        AsmProgram {
            asm_program: AsmFunction {
                name: "".to_string(),
                body: Vec::new(),
            },
            stack_map: HashMap::new(),
            stack_index: 0,
        }
    }

    pub fn generate_assembly(&mut self, tac: TacAst) -> AsmAst {
        match tac {
            TacAst::TacProgram(function) => AsmAst::AsmProgram(self.parse_tac_function(function)),
        }
    }

    fn parse_tac_function(&mut self, function: TacFunction) -> AsmFunction {
        let mut asm_instructions: Vec<AsmInstruction> = Vec::new();

        self.parse_tac_instructions(&function.body, &mut asm_instructions);
        asm_instructions.insert(0, AsmInstruction::AllocateStack(self.stack_index));

        AsmFunction {
            name: function.name,
            body: asm_instructions,
        }
    }

    fn parse_tac_instructions(
        &mut self,
        tac_instructions: &Vec<TacInstruction>,
        asm_instructions: &mut Vec<AsmInstruction>,
    ) {
        for tac_instruction in tac_instructions {
            match tac_instruction {
                TacInstruction::Ret(val) => {
                    asm_instructions.push(AsmInstruction::Mov {
                        src: self.to_asm_operand(&val),
                        dst: AsmOperand::Reg(AsmRegister::EAX),
                    });
                    asm_instructions.push(AsmInstruction::Ret);
                }
                TacInstruction::Unary { op, src, dst } => {
                    let asm_src = self.to_asm_operand(src);
                    let asm_dst = self.to_asm_operand(dst);
                    self.emit_mov(asm_instructions, asm_src, asm_dst);
                    asm_instructions.push(AsmInstruction::Unary {
                        unary_op: self.to_asm_unary_operator(op),
                        operand: self.to_asm_operand(dst),
                    });
                }
            }
        }
    }

    //Avoids stack to stack mov
    fn emit_mov(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
    ) {
        if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (&dst, &src) {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10d);
            asm_instructions.push(AsmInstruction::Mov {
                src: src,
                dst: tmp_reg.clone(),
            });
            asm_instructions.push(AsmInstruction::Mov {
                src: tmp_reg,
                dst: dst,
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Mov { src: src, dst: dst });
        }
    }

    fn to_asm_operand(&mut self, tac_operand: &TacOperand) -> AsmOperand {
        match tac_operand {
            TacOperand::Const(val) => AsmOperand::Imm(*val),
            TacOperand::Var(val) => AsmOperand::Stack(self.get_var_offset(val)),
        }
    }
    fn to_asm_unary_operator(&mut self, tac_op: &TacUnaryOp) -> AsmUnaryOp {
        match tac_op {
            TacUnaryOp::Complement => AsmUnaryOp::Not,
            TacUnaryOp::Negate => AsmUnaryOp::Neg,
        }
    }

    // Return stack offset of tmp variable
    // Assign it one if seen for the first time
    fn get_var_offset(&mut self, name: &str) -> i32 {
        *self.stack_map.entry(name.to_string()).or_insert_with(|| {
            self.stack_index -= 4; // currently only 32 bit
            self.stack_index
        })
    }
}
