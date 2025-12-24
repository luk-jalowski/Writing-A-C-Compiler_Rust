use std::collections::HashMap;

use crate::tac::{TacAst, TacBinaryOp, TacFunction, TacInstruction, TacOperand, TacUnaryOp};

#[derive(Debug)]
pub enum AsmAst {
    AsmProgram(Vec<AsmFunction>),
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
    Cmp {
        left: AsmOperand,
        right: AsmOperand,
    },
    Idiv(AsmOperand),
    Cdq,
    Jmp(String),
    JmpCC {
        condition: AsmConditional,
        target: String,
    },
    SetCC {
        condition: AsmConditional,
        operand: AsmOperand,
    },
    Label(String),
    AllocateStack(i32),
    DeallocateStack(i32),
    Ret,
    Push(AsmOperand),
    Pop(AsmOperand),
    Call(String),
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
    RAX,
    RCX,
    RDX,
    RDI,
    RSI,
    R8,
    R9,
    R10,
    R11,
}

#[derive(Debug)]
pub enum AsmConditional {
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
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
            TacAst::TacProgram(func_declarations) => {
                let mut functions = Vec::new();

                for func_decl in func_declarations {
                    functions.push(self.parse_tac_function(func_decl));
                }

                AsmAst::AsmProgram(functions)
            }
        }
    }

    fn parse_tac_function(&mut self, function: TacFunction) -> AsmFunction {
        self.stack_index = 0;
        self.stack_map.clear();
        let mut asm_instructions: Vec<AsmInstruction> = Vec::new();

        let arg_registers = [
            AsmRegister::RDI,
            AsmRegister::RSI,
            AsmRegister::RDX,
            AsmRegister::RCX,
            AsmRegister::R8,
            AsmRegister::R9,
        ];

        for (i, param) in function.params.iter().enumerate() {
            if i < 6 {
                let offset = self.get_var_offset(param);
                self.emit_mov(
                    &mut asm_instructions,
                    AsmOperand::Reg(arg_registers[i].clone()),
                    AsmOperand::Stack(offset),
                );
            } else {
                // 7th arg is at rbp + 16, 8th at rbp + 24, etc.
                let offset = 16 + (i as i32 - 6) * 8;
                self.stack_map.insert(param.clone(), offset);
            }
        }

        self.parse_tac_instructions(&function.body, &mut asm_instructions);
        let stack_size = if self.stack_index % 16 != 0 {
            self.stack_index - (16 - (self.stack_index.abs() % 16))
        } else {
            self.stack_index
        };
        asm_instructions.insert(0, AsmInstruction::AllocateStack(stack_size));

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
                        dst: AsmOperand::Reg(AsmRegister::RAX),
                    });
                    asm_instructions.push(AsmInstruction::Ret);
                }
                TacInstruction::Unary { op, src, dst } => {
                    let asm_src = self.to_asm_operand(src);
                    let asm_dst = self.to_asm_operand(dst);

                    if *op == TacUnaryOp::Not {
                        self.emit_cmp(asm_instructions, asm_src, AsmOperand::Imm(0));
                        self.emit_mov(asm_instructions, AsmOperand::Imm(0), asm_dst.clone());
                        asm_instructions.push(AsmInstruction::SetCC {
                            condition: AsmConditional::Equal,
                            operand: asm_dst,
                        });
                        continue;
                    }

                    let asm_op = match op {
                        TacUnaryOp::Complement => AsmUnaryOp::Not,
                        TacUnaryOp::Negate => AsmUnaryOp::Neg,
                        _ => {
                            panic!("Unsupported unary op {:?}", op);
                        }
                    };
                    self.emit_mov(asm_instructions, asm_src, asm_dst);
                    asm_instructions.push(AsmInstruction::Unary {
                        op: asm_op,
                        operand: self.to_asm_operand(dst),
                    });
                }
                TacInstruction::JumpIfZero { condition, target } => {
                    let operand = self.to_asm_operand(condition);
                    self.emit_cmp(asm_instructions, operand, AsmOperand::Imm(0));
                    asm_instructions.push(AsmInstruction::JmpCC {
                        condition: AsmConditional::Equal,
                        target: target.clone(),
                    });
                }
                TacInstruction::JumpIfNotZero { condition, target } => {
                    let operand = self.to_asm_operand(condition);
                    self.emit_cmp(asm_instructions, operand, AsmOperand::Imm(0));
                    asm_instructions.push(AsmInstruction::JmpCC {
                        condition: AsmConditional::NotEqual,
                        target: target.clone(),
                    });
                }
                TacInstruction::Jump { target } => {
                    asm_instructions.push(AsmInstruction::Jmp(target.clone()));
                }
                TacInstruction::Label(name) => {
                    asm_instructions.push(AsmInstruction::Label(name.clone()));
                }
                TacInstruction::Copy { src, dst } => {
                    let asm_src = self.to_asm_operand(src);
                    let asm_dst = self.to_asm_operand(dst);
                    self.emit_mov(asm_instructions, asm_src, asm_dst);
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
                                AsmOperand::Reg(AsmRegister::RAX),
                            );
                            asm_instructions.push(AsmInstruction::Cdq);
                            self.emit_idiv(asm_instructions, asm_src2);
                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Reg(AsmRegister::RAX),
                                asm_dst,
                            );
                        }
                        TacBinaryOp::Remainder => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                AsmOperand::Reg(AsmRegister::RAX),
                            );
                            asm_instructions.push(AsmInstruction::Cdq);
                            self.emit_idiv(asm_instructions, asm_src2);
                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Reg(AsmRegister::RDX),
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
                        TacBinaryOp::EqualTo => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2);

                            self.emit_mov(asm_instructions, AsmOperand::Imm(0), asm_dst.clone());
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::Equal,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::NotEqualTo => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2);

                            self.emit_mov(asm_instructions, AsmOperand::Imm(0), asm_dst.clone());
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::NotEqual,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::LessThan => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2);

                            self.emit_mov(asm_instructions, AsmOperand::Imm(0), asm_dst.clone());
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::Less,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::GreaterThan => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2);

                            self.emit_mov(asm_instructions, AsmOperand::Imm(0), asm_dst.clone());
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::Greater,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::LessOrEqual => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2);

                            self.emit_mov(asm_instructions, AsmOperand::Imm(0), asm_dst.clone());
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::LessOrEqual,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::GreaterOrEqual => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2);

                            self.emit_mov(asm_instructions, AsmOperand::Imm(0), asm_dst.clone());
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::GreaterOrEqual,
                                operand: asm_dst,
                            });
                        }
                    }
                }
                TacInstruction::FuncCall { name, args, dst } => {
                    let arg_registers = [
                        AsmRegister::RDI,
                        AsmRegister::RSI,
                        AsmRegister::RDX,
                        AsmRegister::RCX,
                        AsmRegister::R8,
                        AsmRegister::R9,
                    ];
                    let stack_padding = if args.len() > 6 && args.len() % 2 == 1 {
                        8
                    } else {
                        0
                    };

                    if stack_padding != 0 {
                        asm_instructions.push(AsmInstruction::AllocateStack(stack_padding));
                    }
                    // First 6 args go to registers
                    for (index, arg) in args.iter().take(6).enumerate() {
                        let asm_op = self.to_asm_operand(arg);

                        self.emit_mov(
                            asm_instructions,
                            asm_op,
                            AsmOperand::Reg(arg_registers[index].clone()),
                        );
                    }

                    // Arguments are placed on stack in reverse order
                    for arg in args.iter().skip(6).rev() {
                        let asm_op = self.to_asm_operand(arg);
                        self.emit_mov(asm_instructions, asm_op, AsmOperand::Reg(AsmRegister::RAX));
                        asm_instructions
                            .push(AsmInstruction::Push(AsmOperand::Reg(AsmRegister::RAX)));
                    }

                    asm_instructions.push(AsmInstruction::Call(name.clone()));

                    if args.len() > 6 {
                        let stack_args_size = (args.len() - 6) as i32 * 8;
                        asm_instructions.push(AsmInstruction::DeallocateStack(
                            stack_args_size + stack_padding,
                        ));
                    } else if stack_padding != 0 {
                        asm_instructions.push(AsmInstruction::DeallocateStack(stack_padding));
                    }

                    let dst_op = self.to_asm_operand(dst);
                    self.emit_mov(asm_instructions, AsmOperand::Reg(AsmRegister::RAX), dst_op);
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
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
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
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
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
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
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
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
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
            let tmp_reg = AsmOperand::Reg(AsmRegister::R11);
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

    fn emit_cmp(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        left: AsmOperand,
        right: AsmOperand,
    ) {
        if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (&left, &right) {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
            asm_instructions.push(AsmInstruction::Mov {
                src: right.clone(),
                dst: tmp_reg.clone(),
            });
            asm_instructions.push(AsmInstruction::Cmp {
                left,
                right: tmp_reg.clone(),
            });
        } else if let AsmOperand::Imm(_) = &left {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R11);
            asm_instructions.push(AsmInstruction::Mov {
                src: left.clone(),
                dst: tmp_reg.clone(),
            });
            asm_instructions.push(AsmInstruction::Cmp {
                left: tmp_reg.clone(),
                right,
            });
        } else {
            asm_instructions.push(AsmInstruction::Cmp { left, right });
        }
    }

    fn to_asm_operand(&mut self, tac_operand: &TacOperand) -> AsmOperand {
        match tac_operand {
            TacOperand::Const(val) => AsmOperand::Imm(*val),
            TacOperand::Var(val) => AsmOperand::Stack(self.get_var_offset(val)),
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
