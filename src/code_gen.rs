use std::collections::HashMap;

use crate::ast::{Const, Type};
use crate::tac::{
    TacAst, TacBinaryOp, TacFunction, TacInstruction, TacOperand, TacTopLevel, TacUnaryOp,
};
use crate::type_validation::StaticInit;

#[derive(Debug)]
pub enum AsmAst {
    AsmProgram(Vec<AsmTopLevel>),
}

#[derive(Debug)]
pub enum AsmTopLevel {
    Function(AsmFunction),
    StaticVariable {
        name: String,
        init: StaticInit,
        global: bool,
        alignment: u32,
    },
}

#[derive(Debug)]
pub struct AsmFunction {
    pub name: String,
    pub body: Vec<AsmInstruction>,
    pub global: bool,
}

#[derive(Debug)]
pub enum AsmInstruction {
    Mov {
        src: AsmOperand,
        dst: AsmOperand,
        size: AssemblyType,
    },
    Movsx {
        src: AsmOperand,
        dst: AsmOperand,
    },
    MovZeroExtend {
        src: AsmOperand,
        dst: AsmOperand,
    },
    Unary {
        op: AsmUnaryOp,
        operand: AsmOperand,
        size: AssemblyType,
    },
    Binary {
        op: AsmBinaryOp,
        left: AsmOperand,
        right: AsmOperand,
        size: AssemblyType,
    },
    Cmp {
        left: AsmOperand,
        right: AsmOperand,
        size: AssemblyType,
    },
    Idiv {
        operand: AsmOperand,
        size: AssemblyType,
    },
    Div {
        operand: AsmOperand,
        size: AssemblyType,
    },
    Cdq {
        size: AssemblyType,
    },
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
    Imm(i64),
    Stack(i32),
    Data(String),
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
    RSP,
}

#[derive(Debug)]
pub enum AsmConditional {
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    Above,
    AboveOrEqual,
    Below,
    BelowOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssemblyType {
    Dword, // 16b
    Qword, // 32b
}

#[derive(Debug)]
pub struct AsmProgram {
    pub asm_program: AsmFunction,
    stack_map: HashMap<String, i32>,
    stack_index: i32,
}

impl Default for AsmProgram {
    fn default() -> Self {
        Self::new()
    }
}

impl AsmProgram {
    pub fn new() -> Self {
        AsmProgram {
            asm_program: AsmFunction {
                name: "".to_string(),
                body: Vec::new(),
                global: true,
            },
            stack_map: HashMap::new(),
            stack_index: 0,
        }
    }

    pub fn generate_assembly(&mut self, tac: TacAst) -> AsmAst {
        match tac {
            TacAst::TacProgram(func_declarations) => {
                let mut top_level = Vec::<AsmTopLevel>::new();

                for decl in func_declarations {
                    match decl {
                        TacTopLevel::Function(function) => {
                            top_level
                                .push(AsmTopLevel::Function(self.parse_tac_function(function)));
                        }
                        TacTopLevel::StaticVariable {
                            name,
                            init,
                            global,
                            var_type,
                        } => {
                            let alignment: u32 = match var_type {
                                Type::Long | Type::ULong => 8,
                                _ => 4,
                            };
                            top_level.push(AsmTopLevel::StaticVariable {
                                name,
                                init,
                                global,
                                alignment,
                            });
                        }
                    }
                }

                AsmAst::AsmProgram(top_level)
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

        for (i, (param, param_type)) in function
            .params
            .iter()
            .zip(function.param_types.iter())
            .enumerate()
        {
            if i < 6 {
                let size = match self.get_type_size(param_type) {
                    AssemblyType::Dword => 4,
                    AssemblyType::Qword => 8,
                };
                let offset = self.get_var_offset(param, size);
                self.emit_mov(
                    &mut asm_instructions,
                    AsmOperand::Reg(arg_registers[i].clone()),
                    AsmOperand::Stack(offset),
                    self.get_type_size(param_type),
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
            global: function.global,
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
                    let size = self.get_operand_size(val);
                    asm_instructions.push(AsmInstruction::Mov {
                        src: self.to_asm_operand(val),
                        dst: AsmOperand::Reg(AsmRegister::RAX),
                        size,
                    });
                    asm_instructions.push(AsmInstruction::Ret);
                }
                TacInstruction::Unary { op, src, dst } => {
                    let asm_src = self.to_asm_operand(src);
                    let asm_dst = self.to_asm_operand(dst);
                    let size = self.get_operand_size(dst);

                    if *op == TacUnaryOp::Not {
                        let src_size = self.get_operand_size(src);
                        self.emit_cmp(asm_instructions, asm_src, AsmOperand::Imm(0), src_size);
                        self.emit_mov(
                            asm_instructions,
                            AsmOperand::Imm(0),
                            asm_dst.clone(),
                            size.clone(),
                        );
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
                    self.emit_mov(asm_instructions, asm_src, asm_dst, size.clone());
                    asm_instructions.push(AsmInstruction::Unary {
                        op: asm_op,
                        operand: self.to_asm_operand(dst),
                        size,
                    });
                }
                TacInstruction::JumpIfZero { condition, target } => {
                    let operand = self.to_asm_operand(condition);
                    let size = self.get_operand_size(condition);
                    self.emit_cmp(asm_instructions, operand, AsmOperand::Imm(0), size);
                    asm_instructions.push(AsmInstruction::JmpCC {
                        condition: AsmConditional::Equal,
                        target: target.clone(),
                    });
                }
                TacInstruction::JumpIfNotZero { condition, target } => {
                    let operand = self.to_asm_operand(condition);
                    let size = self.get_operand_size(condition);
                    self.emit_cmp(asm_instructions, operand, AsmOperand::Imm(0), size);
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
                    let size = self.get_operand_size(dst);
                    self.emit_mov(asm_instructions, asm_src, asm_dst, size);
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
                    let size = self.get_operand_size(dst);
                    let op_size = self.get_operand_size(src1);
                    match op {
                        TacBinaryOp::Divide => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                AsmOperand::Reg(AsmRegister::RAX),
                                size.clone(),
                            );
                            if self.is_signed(dst) {
                                asm_instructions.push(AsmInstruction::Cdq { size: size.clone() });
                            } else {
                                self.emit_mov(
                                    asm_instructions,
                                    AsmOperand::Imm(0),
                                    AsmOperand::Reg(AsmRegister::RDX),
                                    size.clone(),
                                );
                            }
                            self.emit_div(
                                asm_instructions,
                                asm_src2,
                                size.clone(),
                                self.is_signed(dst),
                            );
                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Reg(AsmRegister::RAX),
                                asm_dst,
                                size,
                            );
                        }
                        TacBinaryOp::Remainder => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                AsmOperand::Reg(AsmRegister::RAX),
                                size.clone(),
                            );
                            if self.is_signed(dst) {
                                asm_instructions.push(AsmInstruction::Cdq { size: size.clone() });
                            } else {
                                self.emit_mov(
                                    asm_instructions,
                                    AsmOperand::Imm(0),
                                    AsmOperand::Reg(AsmRegister::RDX),
                                    size.clone(),
                                );
                            }
                            self.emit_div(
                                asm_instructions,
                                asm_src2,
                                size.clone(),
                                self.is_signed(dst),
                            );
                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Reg(AsmRegister::RDX),
                                asm_dst,
                                size,
                            );
                        }
                        TacBinaryOp::Add => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                asm_dst.clone(),
                                size.clone(),
                            );
                            self.emit_add(asm_instructions, asm_src2, asm_dst, size)
                        }

                        TacBinaryOp::Subtract => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                asm_dst.clone(),
                                size.clone(),
                            );
                            self.emit_sub(asm_instructions, asm_src2, asm_dst, size)
                        }

                        TacBinaryOp::Multiply => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                asm_dst.clone(),
                                size.clone(),
                            );
                            self.emit_imul(asm_instructions, asm_src2, asm_dst, size)
                        }
                        TacBinaryOp::EqualTo => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2, op_size);

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                size,
                            );
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::Equal,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::NotEqualTo => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2, op_size);

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                size,
                            );
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::NotEqual,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::LessThan => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2, op_size);

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                size,
                            );
                            let condition = if self.is_signed(src1) {
                                AsmConditional::Less
                            } else {
                                AsmConditional::Below
                            };
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::GreaterThan => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2, op_size);

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                size,
                            );
                            let condition = if self.is_signed(src1) {
                                AsmConditional::Greater
                            } else {
                                AsmConditional::Above
                            };
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::LessOrEqual => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2, op_size);

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                size,
                            );
                            let condition = if self.is_signed(src1) {
                                AsmConditional::LessOrEqual
                            } else {
                                AsmConditional::BelowOrEqual
                            };
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::GreaterOrEqual => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2, op_size);

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                size,
                            );
                            let condition = if self.is_signed(src1) {
                                AsmConditional::GreaterOrEqual
                            } else {
                                AsmConditional::AboveOrEqual
                            };
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition,
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
                        let size = self.get_operand_size(arg);

                        self.emit_mov(
                            asm_instructions,
                            asm_op,
                            AsmOperand::Reg(arg_registers[index].clone()),
                            size,
                        );
                    }

                    // Arguments are placed on stack in reverse order
                    for arg in args.iter().skip(6).rev() {
                        let asm_op = self.to_asm_operand(arg);
                        let size = self.get_operand_size(arg);
                        if size == AssemblyType::Dword {
                            self.emit_movsx(
                                asm_instructions,
                                asm_op,
                                AsmOperand::Reg(AsmRegister::RAX),
                            );
                        } else {
                            self.emit_mov(
                                asm_instructions,
                                asm_op,
                                AsmOperand::Reg(AsmRegister::RAX),
                                size,
                            );
                        }
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
                    let size = self.get_operand_size(dst);
                    self.emit_mov(
                        asm_instructions,
                        AsmOperand::Reg(AsmRegister::RAX),
                        dst_op,
                        size,
                    );
                }
                TacInstruction::SignExtend { src, dst } => {
                    let src_op = self.to_asm_operand(src);
                    let dst_op = self.to_asm_operand(dst);
                    self.emit_movsx(asm_instructions, src_op, dst_op);
                }
                TacInstruction::Truncate { src, dst } => {
                    // Truncate is just a move of the lower 32 bits
                    let src_op = self.to_asm_operand(src);
                    let dst_op = self.to_asm_operand(dst);
                    self.emit_mov(asm_instructions, src_op, dst_op, AssemblyType::Dword);
                }
                TacInstruction::ZeroExtend { src, dst } => {
                    let src_op = self.to_asm_operand(src);
                    let dst_op = self.to_asm_operand(dst);
                    self.emit_movzx(asm_instructions, src_op, dst_op);
                }
            }
        }
    }

    fn emit_mov(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
        size: AssemblyType,
    ) {
        let src = if let AsmOperand::Imm(val) = src {
            if size == AssemblyType::Dword {
                AsmOperand::Imm(val as i32 as i64)
            } else {
                AsmOperand::Imm(val)
            }
        } else {
            src
        };

        if self.is_memory(&dst) && self.is_memory(&src) {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
            asm_instructions.push(AsmInstruction::Mov {
                src,
                dst: tmp_reg.clone(),
                size: size.clone(),
            });
            asm_instructions.push(AsmInstruction::Mov {
                src: tmp_reg,
                dst,
                size,
            });
        } else {
            // Check if we are moving a large immediate to memory
            // x86-64 does not allow move of 64b Imm to Memory directly
            let requires_tmp_reg = if let AsmOperand::Imm(val) = &src {
                if self.is_memory(&dst) && size == AssemblyType::Qword {
                    // Check if value fits in 32-bit signed integer
                    *val > i32::MAX as i64 || *val < i32::MIN as i64
                } else {
                    false
                }
            } else {
                false
            };

            if requires_tmp_reg {
                let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
                asm_instructions.push(AsmInstruction::Mov {
                    src,
                    dst: tmp_reg.clone(),
                    size: size.clone(),
                });
                asm_instructions.push(AsmInstruction::Mov {
                    src: tmp_reg,
                    dst,
                    size,
                });
            } else {
                // Direct move is fine
                asm_instructions.push(AsmInstruction::Mov { src, dst, size });
            }
        }
    }

    fn emit_movsx(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
    ) {
        if matches!(src, AsmOperand::Imm(_)) {
            self.emit_mov(asm_instructions, src, dst, AssemblyType::Qword);
        } else if self.is_memory(&dst) {
            let tmp_reg = AsmOperand::Reg(AsmRegister::R11);
            asm_instructions.push(AsmInstruction::Movsx {
                src,
                dst: tmp_reg.clone(),
            });
            self.emit_mov(asm_instructions, tmp_reg, dst, AssemblyType::Qword);
        } else {
            asm_instructions.push(AsmInstruction::Movsx { src, dst });
        }
    }
    fn emit_movzx(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
    ) {
        if matches!(dst, AsmOperand::Reg(_)) {
            self.emit_mov(asm_instructions, src, dst, AssemblyType::Dword);
        } else {
            self.emit_mov(
                asm_instructions,
                src,
                AsmOperand::Reg(AsmRegister::R11),
                AssemblyType::Dword,
            );
            self.emit_mov(
                asm_instructions,
                AsmOperand::Reg(AsmRegister::R11),
                dst,
                AssemblyType::Qword,
            );
        }
    }

    fn emit_div(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        operand: AsmOperand,
        size: AssemblyType,
        signed: bool,
    ) {
        if let AsmOperand::Imm(_) = &operand {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
            asm_instructions.push(AsmInstruction::Mov {
                src: operand,
                dst: tmp_reg.clone(),
                size: size.clone(),
            });
            if signed {
                asm_instructions.push(AsmInstruction::Idiv {
                    operand: tmp_reg,
                    size,
                });
            } else {
                asm_instructions.push(AsmInstruction::Div {
                    operand: tmp_reg,
                    size,
                });
            }
        } else {
            if signed {
                asm_instructions.push(AsmInstruction::Idiv {
                    operand: operand,
                    size,
                })
            } else {
                asm_instructions.push(AsmInstruction::Div {
                    operand: operand,
                    size,
                });
            }
        }
    }

    // Avoid memory to memory operations
    // add dst, src
    fn emit_add(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
        size: AssemblyType,
    ) {
        let src_op = if let AsmOperand::Imm(val) = &src {
            if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                let tmp = AsmOperand::Reg(AsmRegister::R10);
                asm_instructions.push(AsmInstruction::Mov {
                    src: src.clone(),
                    dst: tmp.clone(),
                    size: size.clone(),
                });
                tmp
            } else {
                src.clone()
            }
        } else {
            src.clone()
        };
        if self.is_memory(&dst) && self.is_memory(&src_op) {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
            asm_instructions.push(AsmInstruction::Mov {
                src: src_op.clone(),
                dst: tmp_reg.clone(),
                size: size.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Add,
                left: dst,
                right: tmp_reg,
                size,
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Add,
                left: dst,
                right: src_op,
                size,
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
        size: AssemblyType,
    ) {
        let src_op = if let AsmOperand::Imm(val) = &src {
            if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                let tmp = AsmOperand::Reg(AsmRegister::R10);
                asm_instructions.push(AsmInstruction::Mov {
                    src: src.clone(),
                    dst: tmp.clone(),
                    size: size.clone(),
                });
                tmp
            } else {
                src.clone()
            }
        } else {
            src.clone()
        };
        if self.is_memory(&dst) && self.is_memory(&src_op) {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
            asm_instructions.push(AsmInstruction::Mov {
                src: src_op.clone(),
                dst: tmp_reg.clone(),
                size: size.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Sub,
                left: dst,
                right: tmp_reg,
                size,
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Sub,
                left: dst,
                right: src_op,
                size,
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
        size: AssemblyType,
    ) {
        let src_op = if let AsmOperand::Imm(val) = &src {
            if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                let tmp = AsmOperand::Reg(AsmRegister::R10);
                asm_instructions.push(AsmInstruction::Mov {
                    src: src.clone(),
                    dst: tmp.clone(),
                    size: size.clone(),
                });
                tmp
            } else {
                src.clone()
            }
        } else {
            src.clone()
        };

        if self.is_memory(&dst) {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R11);
            asm_instructions.push(AsmInstruction::Mov {
                src: dst.clone(),
                dst: tmp_reg.clone(),
                size: size.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Mult,
                left: tmp_reg.clone(),
                right: src_op,
                size: size.clone(),
            });
            asm_instructions.push(AsmInstruction::Mov {
                src: tmp_reg.clone(),
                dst: dst.clone(),
                size,
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Mult,
                left: dst,
                right: src_op,
                size,
            });
        }
    }

    fn emit_cmp(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        left: AsmOperand,
        right: AsmOperand,
        size: AssemblyType,
    ) {
        let right_op = if let AsmOperand::Imm(val) = &right {
            if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                let tmp = AsmOperand::Reg(AsmRegister::R10);
                asm_instructions.push(AsmInstruction::Mov {
                    src: right.clone(),
                    dst: tmp.clone(),
                    size: size.clone(),
                });
                tmp
            } else {
                right.clone()
            }
        } else {
            right.clone()
        };

        if self.is_memory(&left) && self.is_memory(&right_op) {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
            asm_instructions.push(AsmInstruction::Mov {
                src: right_op.clone(),
                dst: tmp_reg.clone(),
                size: size.clone(),
            });
            asm_instructions.push(AsmInstruction::Cmp {
                left,
                right: tmp_reg.clone(),
                size,
            });
        } else if let AsmOperand::Imm(_) = &left {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R11);
            asm_instructions.push(AsmInstruction::Mov {
                src: left.clone(),
                dst: tmp_reg.clone(),
                size: size.clone(),
            });
            asm_instructions.push(AsmInstruction::Cmp {
                left: tmp_reg.clone(),
                right: right_op,
                size,
            });
        } else {
            asm_instructions.push(AsmInstruction::Cmp {
                left,
                right: right_op,
                size,
            });
        }
    }

    fn to_asm_operand(&mut self, tac_operand: &TacOperand) -> AsmOperand {
        match tac_operand {
            TacOperand::Const(val) => {
                let num: i64 = match val {
                    Const::ConstInt(val) => *val as i64,
                    Const::ConstUInt(val) => *val as i64,
                    Const::ConstLong(val) => *val,
                    Const::ConstULong(val) => *val as i64,
                };
                AsmOperand::Imm(num)
            }
            TacOperand::Var {
                name,
                var_type,
                is_static,
            } => {
                if *is_static {
                    AsmOperand::Data(name.clone())
                } else {
                    let size = match self.get_type_size(var_type) {
                        AssemblyType::Dword => 4,
                        AssemblyType::Qword => 8,
                    };
                    AsmOperand::Stack(self.get_var_offset(name, size))
                }
            }
        }
    }

    fn get_operand_size(&self, operand: &TacOperand) -> AssemblyType {
        match operand {
            TacOperand::Var { var_type, .. } => self.get_type_size(var_type),
            TacOperand::Const(val) => match val {
                Const::ConstInt(_) => AssemblyType::Dword,
                Const::ConstUInt(_) => AssemblyType::Dword,
                Const::ConstLong(_) => AssemblyType::Qword,
                Const::ConstULong(_) => AssemblyType::Qword,
            },
        }
    }

    fn get_type_size(&self, var_type: &Type) -> AssemblyType {
        match var_type {
            Type::Int | Type::UInt => AssemblyType::Dword,
            Type::Long | Type::ULong => AssemblyType::Qword,
            _ => AssemblyType::Qword,
        }
    }

    // Return stack offset of tmp variable
    // Assign it one if seen for the first time
    fn get_var_offset(&mut self, name: &str, size: i32) -> i32 {
        *self.stack_map.entry(name.to_string()).or_insert_with(|| {
            self.stack_index -= size;
            self.stack_index
        })
    }

    fn is_memory(&self, op: &AsmOperand) -> bool {
        matches!(op, AsmOperand::Stack(_) | AsmOperand::Data(_))
    }

    fn is_signed(&self, op: &TacOperand) -> bool {
        match op {
            TacOperand::Var { var_type, .. } => match var_type {
                Type::Int | Type::Long => true,
                _ => false,
            },
            TacOperand::Const(c) => match c {
                Const::ConstInt(_) | Const::ConstLong(_) => true,
                _ => false,
            },
        }
    }
}
