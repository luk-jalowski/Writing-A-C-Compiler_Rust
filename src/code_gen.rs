use std::collections::HashMap;

use crate::tac::{TacAst, TacBinaryOp, TacFunction, TacInstruction, TacOperand, TacUnaryOp};

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
        op: AsmUnaryOp,
        operand: AsmOperand,
    },
    Binary {
        op: AsmBinaryOp,
        left: AsmOperand,
        right: AsmOperand,
    },
    Idiv(AsmOperand),
    Cdq,
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

#[derive(Debug)]
pub enum AsmBinaryOp {
    Add,
    Sub,
    Mult,
}

#[derive(Debug, Clone)]
pub enum AsmRegister {
    EAX,
    EDX,
    R10d,
    R11d,
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
                        op: self.to_asm_unary_operator(op),
                        operand: self.to_asm_operand(dst),
                    });
                }
                TacInstruction::Binary {
                    op,
                    src1,
                    src2,
                    dst,
                } => {
                    let asm_src1 = self.to_asm_operand(src1);
                    let asm_src2 = self.to_asm_operand(src2);
                    let asm_dst = self.to_asm_operand(dst);
                    match op {
                        TacBinaryOp::Divide => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                AsmOperand::Reg(AsmRegister::EAX),
                            );
                            asm_instructions.push(AsmInstruction::Cdq);
                            self.emit_idiv(asm_instructions, asm_src2);
                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Reg(AsmRegister::EAX),
                                asm_dst,
                            );
                        }
                        TacBinaryOp::Remainder => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                AsmOperand::Reg(AsmRegister::EAX),
                            );
                            asm_instructions.push(AsmInstruction::Cdq);
                            self.emit_idiv(asm_instructions, asm_src2);
                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Reg(AsmRegister::EDX),
                                asm_dst,
                            );
                        }
                        TacBinaryOp::Add => {
                            self.emit_mov(asm_instructions, asm_src1, asm_dst.clone());
                            self.emit_add(asm_instructions, asm_src2, asm_dst)
                        }

                        TacBinaryOp::Subtract => {
                            self.emit_mov(asm_instructions, asm_src1, asm_dst.clone());
                            self.emit_sub(asm_instructions, asm_src2, asm_dst)
                        }

                        TacBinaryOp::Multiply => {
                            self.emit_mov(asm_instructions, asm_src1, asm_dst.clone());
                            self.emit_imul(asm_instructions, asm_src2, asm_dst)
                        }
                        _ => {
                            let asm_binary_op = self.to_asm_binary_operator(op);
                            self.emit_mov(asm_instructions, asm_src1, asm_dst.clone());
                            asm_instructions.push(AsmInstruction::Binary {
                                op: asm_binary_op,
                                left: asm_dst,
                                right: asm_src2,
                            });
                        }
                    }
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

    // Avoid memory to memory operations
    fn emit_idiv(&mut self, asm_instructions: &mut Vec<AsmInstruction>, operand: AsmOperand) {
        if let AsmOperand::Imm(_) = &operand {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10d);
            asm_instructions.push(AsmInstruction::Mov {
                src: operand,
                dst: tmp_reg.clone(),
            });
            asm_instructions.push(AsmInstruction::Idiv(tmp_reg));
        } else {
            asm_instructions.push(AsmInstruction::Idiv(operand))
        }
    }

    // Avoid memory to memory operations
    // add dst, src
    fn emit_add(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
    ) {
        if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (&dst, &src) {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10d);
            asm_instructions.push(AsmInstruction::Mov {
                src: src.clone(),
                dst: tmp_reg.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Add,
                left: dst,
                right: tmp_reg,
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Add,
                left: dst,
                right: src,
            });
        }
    }

    // Avoid memory to memory operations
    // sub dst, src
    fn emit_sub(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
    ) {
        if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (&dst, &src) {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10d);
            asm_instructions.push(AsmInstruction::Mov {
                src: src.clone(),
                dst: tmp_reg.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Sub,
                left: dst,
                right: tmp_reg,
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Sub,
                left: dst,
                right: src,
            });
        }
    }

    // Avoid memory to memory operations
    // imul dst, src
    fn emit_imul(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
    ) {
        if let AsmOperand::Stack(_) = &dst {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R11d);
            asm_instructions.push(AsmInstruction::Mov {
                src: dst.clone(),
                dst: tmp_reg.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Mult,
                left: tmp_reg.clone(),
                right: src,
            });
            asm_instructions.push(AsmInstruction::Mov {
                src: tmp_reg.clone(),
                dst: dst.clone(),
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Mult,
                left: dst,
                right: src,
            });
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
    fn to_asm_binary_operator(&mut self, tac_op: &TacBinaryOp) -> AsmBinaryOp {
        match tac_op {
            TacBinaryOp::Add => AsmBinaryOp::Add,
            TacBinaryOp::Subtract => AsmBinaryOp::Sub,
            TacBinaryOp::Multiply => AsmBinaryOp::Mult,
            _ => {
                panic!(
                    "Unexpected operation in to_asm_binary_operator: {:?}",
                    tac_op
                );
            }
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
