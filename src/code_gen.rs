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

#[derive(Debug, Clone)]
pub enum AsmTopLevel {
    Function(AsmFunction),
    StaticVariable {
        name: String,
        init: StaticInit,
        global: bool,
        alignment: u32,
    },
    StaticConstant {
        name: String,
        init: StaticInit,
        alignment: u32,
    },
}

#[derive(Debug, Clone)]
pub struct AsmFunction {
    pub name: String,
    pub body: Vec<AsmInstruction>,
    pub global: bool,
}

#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Mov {
        src: AsmOperand,
        dst: AsmOperand,
        asm_type: AssemblyType,
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
        asm_type: AssemblyType,
    },
    Binary {
        op: AsmBinaryOp,
        left: AsmOperand,
        right: AsmOperand,
        asm_type: AssemblyType,
    },
    Cmp {
        left: AsmOperand,
        right: AsmOperand,
        asm_type: AssemblyType,
    },
    Idiv {
        operand: AsmOperand,
        asm_type: AssemblyType,
    },
    Div {
        operand: AsmOperand,
        asm_type: AssemblyType,
    },
    Cdq {
        asm_type: AssemblyType,
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
    Cvttsd2si {
        src: AsmOperand,
        dst: AsmOperand,
        asm_type: AssemblyType,
    },
    Cvtsi2sd {
        src: AsmOperand,
        dst: AsmOperand,
        asm_type: AssemblyType,
    },
}

#[derive(Debug, Clone)]
pub enum AsmOperand {
    Reg(AsmRegister),
    Imm(i64),
    Stack(i32),
    Data(String),
}

#[derive(Debug, Clone)]
pub enum AsmUnaryOp {
    Neg,
    Not,
    Shr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmBinaryOp {
    Add,
    Sub,
    Mult,
    DivDouble,
    And,
    Or,
    Xor,
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
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM14,
    XMM15,
}

#[derive(Debug, Clone)]
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
    Dword, // 32b
    Qword, // 64b
    Double,
}

#[derive(Debug)]
pub struct AsmProgram {
    pub asm_program: AsmFunction,
    stack_map: HashMap<String, i32>,
    stack_index: i32,
    top_level: Vec<AsmTopLevel>,
    label_counter: usize,
    data_label_counter: usize,
}

impl AsmProgram {
    pub fn new(label_count: usize) -> Self {
        AsmProgram {
            asm_program: AsmFunction {
                name: "".to_string(),
                body: Vec::new(),
                global: true,
            },
            stack_map: HashMap::new(),
            stack_index: 0,
            top_level: Vec::<AsmTopLevel>::new(),
            label_counter: label_count,
            data_label_counter: 0,
        }
    }

    fn new_label(&mut self) -> String {
        let i = self.label_counter;
        self.label_counter += 1;
        format!(".L{}", i)
    }

    fn new_data_label(&mut self) -> String {
        let i = self.data_label_counter;
        self.data_label_counter += 1;
        format!(".LC{}", i)
    }

    pub fn generate_assembly(&mut self, tac: TacAst) -> AsmAst {
        match tac {
            TacAst::TacProgram(func_declarations) => {
                for decl in func_declarations {
                    match decl {
                        TacTopLevel::Function(function) => {
                            let func = self.parse_tac_function(function);
                            self.top_level.push(AsmTopLevel::Function(func));
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
                            self.top_level.push(AsmTopLevel::StaticVariable {
                                name,
                                init,
                                global,
                                alignment,
                            });
                        }
                    }
                }

                // Really ugly work-around
                // This adds constants we need for double operations, mainly xorsd which need to be 16 byte aligned
                self.top_level.push(AsmTopLevel::StaticConstant {
                    name: "negative_zero".to_string(),
                    init: StaticInit::Double(-0.0),
                    alignment: 16,
                });
                self.top_level.push(AsmTopLevel::StaticConstant {
                    name: "upper_bound".to_string(),
                    init: StaticInit::Double(9223372036854775808.0),
                    alignment: 8,
                });
                AsmAst::AsmProgram(self.top_level.clone())
            }
        }
    }

    fn parse_tac_function(&mut self, function: TacFunction) -> AsmFunction {
        self.stack_index = 0;
        self.stack_map.clear();
        let mut asm_instructions: Vec<AsmInstruction> = Vec::new();

        let int_arg_registers = [
            AsmRegister::RDI,
            AsmRegister::RSI,
            AsmRegister::RDX,
            AsmRegister::RCX,
            AsmRegister::R8,
            AsmRegister::R9,
        ];
        let mut int_reg_idx = 0;

        let xmm_arg_registers = [
            AsmRegister::XMM0,
            AsmRegister::XMM1,
            AsmRegister::XMM2,
            AsmRegister::XMM3,
            AsmRegister::XMM4,
            AsmRegister::XMM5,
            AsmRegister::XMM6,
            AsmRegister::XMM7,
        ];
        let mut xmm_reg_idx = 0;

        let mut stack_arg_offset = 16;

        for (param, param_type) in function.params.iter().zip(function.param_types.iter()) {
            let asm_type = self.type_to_asm_type(param_type);
            if let Type::Double = param_type {
                if xmm_reg_idx < xmm_arg_registers.len() {
                    let reg = xmm_arg_registers[xmm_reg_idx].clone();
                    let size = self.get_type_size(param_type);
                    let offset = self.get_var_offset(param, size as i32);
                    self.emit_mov(
                        &mut asm_instructions,
                        AsmOperand::Reg(reg),
                        AsmOperand::Stack(offset),
                        asm_type,
                    );
                    xmm_reg_idx += 1;
                } else {
                    self.stack_map.insert(param.clone(), stack_arg_offset);
                    stack_arg_offset += 8;
                }
            } else {
                if int_reg_idx < int_arg_registers.len() {
                    let reg = int_arg_registers[int_reg_idx].clone();
                    let size = self.get_type_size(param_type);
                    let offset = self.get_var_offset(param, size as i32);
                    self.emit_mov(
                        &mut asm_instructions,
                        AsmOperand::Reg(reg),
                        AsmOperand::Stack(offset),
                        asm_type,
                    );
                    int_reg_idx += 1;
                } else {
                    self.stack_map.insert(param.clone(), stack_arg_offset);
                    stack_arg_offset += 8;
                }
            }
        }

        self.parse_tac_instructions(&function.body, &mut asm_instructions);
        let stack_size = if self.stack_index.abs() % 16 != 0 {
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
                    let asm_type = self.get_operand_asm_type(val);
                    if asm_type == AssemblyType::Double {
                        asm_instructions.push(AsmInstruction::Mov {
                            src: self.tac_to_asm_operand(val),
                            dst: AsmOperand::Reg(AsmRegister::XMM0),
                            asm_type,
                        });
                    } else {
                        asm_instructions.push(AsmInstruction::Mov {
                            src: self.tac_to_asm_operand(val),
                            dst: AsmOperand::Reg(AsmRegister::RAX),
                            asm_type,
                        });
                    }
                    asm_instructions.push(AsmInstruction::Ret);
                }
                TacInstruction::Unary { op, src, dst } => {
                    let asm_dst = self.tac_to_asm_operand(dst);
                    let asm_src = self.tac_to_asm_operand(src);
                    let dst_asm_type = self.get_operand_asm_type(dst);
                    let src_asm_type = self.get_operand_asm_type(src);

                    if *op == TacUnaryOp::Not {
                        if dst_asm_type == AssemblyType::Double
                            || src_asm_type == AssemblyType::Double
                        {
                            asm_instructions.push(AsmInstruction::Binary {
                                op: AsmBinaryOp::Xor,
                                left: AsmOperand::Reg(AsmRegister::XMM0),
                                right: AsmOperand::Reg(AsmRegister::XMM0),
                                asm_type: AssemblyType::Double,
                            });
                            self.emit_cmp(
                                asm_instructions,
                                asm_src,
                                AsmOperand::Reg(AsmRegister::XMM0),
                                AssemblyType::Double,
                            );
                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                dst_asm_type.clone(),
                            );
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::Equal,
                                operand: asm_dst,
                            });
                            continue;
                        } else {
                            self.emit_cmp(
                                asm_instructions,
                                asm_src,
                                AsmOperand::Imm(0),
                                src_asm_type,
                            );
                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                dst_asm_type.clone(),
                            );
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::Equal,
                                operand: asm_dst,
                            });
                            continue;
                        }
                    } else if *op == TacUnaryOp::Negate
                        && (dst_asm_type == AssemblyType::Double
                            || src_asm_type == AssemblyType::Double)
                    {
                        //xorpd needs to be 16 bytes aligned
                        self.emit_mov(
                            asm_instructions,
                            asm_src,
                            asm_dst.clone(),
                            dst_asm_type.clone(),
                        );
                        if self.is_memory(&asm_dst) {
                            let tmp_reg = AsmOperand::Reg(AsmRegister::XMM15);
                            self.emit_mov(
                                asm_instructions,
                                asm_dst.clone(),
                                tmp_reg.clone(),
                                dst_asm_type.clone(),
                            );
                            asm_instructions.push(AsmInstruction::Binary {
                                op: AsmBinaryOp::Xor,
                                left: tmp_reg.clone(),
                                right: AsmOperand::Data("negative_zero".to_string()),
                                asm_type: dst_asm_type.clone(),
                            });
                            self.emit_mov(asm_instructions, tmp_reg, asm_dst, dst_asm_type);
                        } else {
                            asm_instructions.push(AsmInstruction::Binary {
                                op: AsmBinaryOp::Xor,
                                left: asm_dst,
                                right: AsmOperand::Data("negative_zero".to_string()),
                                asm_type: dst_asm_type,
                            });
                        }
                        continue;
                    }

                    let asm_op = match op {
                        TacUnaryOp::Complement => AsmUnaryOp::Not,
                        TacUnaryOp::Negate => AsmUnaryOp::Neg,
                        _ => {
                            panic!("Unsupported unary op {:?}", op);
                        }
                    };
                    self.emit_mov(asm_instructions, asm_src, asm_dst, dst_asm_type.clone());
                    asm_instructions.push(AsmInstruction::Unary {
                        op: asm_op,
                        operand: self.tac_to_asm_operand(dst),
                        asm_type: dst_asm_type,
                    });
                }
                TacInstruction::JumpIfZero { condition, target } => {
                    let operand = self.tac_to_asm_operand(condition);
                    let asm_type = self.get_operand_asm_type(condition);
                    if asm_type == AssemblyType::Double {
                        asm_instructions.push(AsmInstruction::Binary {
                            op: AsmBinaryOp::Xor,
                            left: AsmOperand::Reg(AsmRegister::XMM0),
                            right: AsmOperand::Reg(AsmRegister::XMM0),
                            asm_type: AssemblyType::Double,
                        });
                        self.emit_cmp(
                            asm_instructions,
                            operand,
                            AsmOperand::Reg(AsmRegister::XMM0),
                            asm_type,
                        );
                    } else {
                        self.emit_cmp(asm_instructions, operand, AsmOperand::Imm(0), asm_type);
                    }
                    asm_instructions.push(AsmInstruction::JmpCC {
                        condition: AsmConditional::Equal,
                        target: target.clone(),
                    });
                }
                TacInstruction::JumpIfNotZero { condition, target } => {
                    let operand = self.tac_to_asm_operand(condition);
                    let asm_type = self.get_operand_asm_type(condition);
                    if asm_type == AssemblyType::Double {
                        asm_instructions.push(AsmInstruction::Binary {
                            op: AsmBinaryOp::Xor,
                            left: AsmOperand::Reg(AsmRegister::XMM0),
                            right: AsmOperand::Reg(AsmRegister::XMM0),
                            asm_type: AssemblyType::Double,
                        });
                        self.emit_cmp(
                            asm_instructions,
                            operand,
                            AsmOperand::Reg(AsmRegister::XMM0),
                            asm_type,
                        );
                    } else {
                        self.emit_cmp(asm_instructions, operand, AsmOperand::Imm(0), asm_type);
                    }
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
                    let asm_src = self.tac_to_asm_operand(src);
                    let asm_dst = self.tac_to_asm_operand(dst);
                    let asm_type = self.get_operand_asm_type(dst);
                    self.emit_mov(asm_instructions, asm_src, asm_dst, asm_type);
                }
                TacInstruction::Binary {
                    op,
                    src1,
                    src2,
                    dst,
                } => {
                    let asm_src1 = self.tac_to_asm_operand(src1);
                    let asm_src2 = self.tac_to_asm_operand(src2);
                    let asm_dst = self.tac_to_asm_operand(dst);
                    let dst_asm_type = self.get_operand_asm_type(dst);
                    let src_asm_type = self.get_operand_asm_type(src1);
                    match op {
                        TacBinaryOp::Divide => {
                            if src_asm_type == AssemblyType::Double {
                                self.emit_mov(
                                    asm_instructions,
                                    asm_src1,
                                    asm_dst.clone(),
                                    dst_asm_type.clone(),
                                );
                                if self.is_memory(&asm_dst) {
                                    let tmp_reg = AsmOperand::Reg(AsmRegister::XMM15);
                                    self.emit_mov(
                                        asm_instructions,
                                        asm_dst.clone(),
                                        tmp_reg.clone(),
                                        dst_asm_type.clone(),
                                    );

                                    asm_instructions.push(AsmInstruction::Binary {
                                        op: AsmBinaryOp::DivDouble,
                                        left: tmp_reg.clone(),
                                        right: asm_src2,
                                        asm_type: dst_asm_type.clone(),
                                    });
                                    self.emit_mov(asm_instructions, tmp_reg, asm_dst, dst_asm_type);
                                } else {
                                    asm_instructions.push(AsmInstruction::Binary {
                                        op: AsmBinaryOp::DivDouble,
                                        left: asm_src2,
                                        right: asm_dst,
                                        asm_type: dst_asm_type,
                                    });
                                }
                            } else {
                                self.emit_mov(
                                    asm_instructions,
                                    asm_src1,
                                    AsmOperand::Reg(AsmRegister::RAX),
                                    dst_asm_type.clone(),
                                );
                                if self.is_signed(dst) {
                                    asm_instructions.push(AsmInstruction::Cdq {
                                        asm_type: dst_asm_type.clone(),
                                    });
                                } else {
                                    self.emit_mov(
                                        asm_instructions,
                                        AsmOperand::Imm(0),
                                        AsmOperand::Reg(AsmRegister::RDX),
                                        dst_asm_type.clone(),
                                    );
                                }
                                self.emit_div(
                                    asm_instructions,
                                    asm_src2,
                                    dst_asm_type.clone(),
                                    self.is_signed(dst),
                                );
                                self.emit_mov(
                                    asm_instructions,
                                    AsmOperand::Reg(AsmRegister::RAX),
                                    asm_dst,
                                    dst_asm_type,
                                );
                            }
                        }
                        TacBinaryOp::Remainder => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                AsmOperand::Reg(AsmRegister::RAX),
                                dst_asm_type.clone(),
                            );
                            if self.is_signed(dst) {
                                asm_instructions.push(AsmInstruction::Cdq {
                                    asm_type: dst_asm_type.clone(),
                                });
                            } else {
                                self.emit_mov(
                                    asm_instructions,
                                    AsmOperand::Imm(0),
                                    AsmOperand::Reg(AsmRegister::RDX),
                                    dst_asm_type.clone(),
                                );
                            }
                            self.emit_div(
                                asm_instructions,
                                asm_src2,
                                dst_asm_type.clone(),
                                self.is_signed(dst),
                            );
                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Reg(AsmRegister::RDX),
                                asm_dst,
                                dst_asm_type,
                            );
                        }
                        TacBinaryOp::Add => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                asm_dst.clone(),
                                dst_asm_type.clone(),
                            );
                            self.emit_add(asm_instructions, asm_src2, asm_dst, dst_asm_type)
                        }

                        TacBinaryOp::Subtract => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                asm_dst.clone(),
                                dst_asm_type.clone(),
                            );
                            self.emit_sub(asm_instructions, asm_src2, asm_dst, dst_asm_type)
                        }

                        TacBinaryOp::Multiply => {
                            self.emit_mov(
                                asm_instructions,
                                asm_src1,
                                asm_dst.clone(),
                                dst_asm_type.clone(),
                            );
                            self.emit_mul(asm_instructions, asm_src2, asm_dst, dst_asm_type)
                        }
                        TacBinaryOp::EqualTo => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2, src_asm_type);

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                dst_asm_type,
                            );
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::Equal,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::NotEqualTo => {
                            self.emit_cmp(asm_instructions, asm_src1, asm_src2, src_asm_type);

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                dst_asm_type,
                            );
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition: AsmConditional::NotEqual,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::LessThan => {
                            self.emit_cmp(
                                asm_instructions,
                                asm_src1,
                                asm_src2,
                                src_asm_type.clone(),
                            );

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                dst_asm_type,
                            );
                            let condition = if (!self.is_signed(src1))
                                || src_asm_type == AssemblyType::Double
                            {
                                AsmConditional::Below
                            } else {
                                AsmConditional::Less
                            };
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::GreaterThan => {
                            self.emit_cmp(
                                asm_instructions,
                                asm_src1,
                                asm_src2,
                                src_asm_type.clone(),
                            );

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                dst_asm_type,
                            );
                            let condition = if (!self.is_signed(src1))
                                || src_asm_type == AssemblyType::Double
                            {
                                AsmConditional::Above
                            } else {
                                AsmConditional::Greater
                            };
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::LessOrEqual => {
                            self.emit_cmp(
                                asm_instructions,
                                asm_src1,
                                asm_src2,
                                src_asm_type.clone(),
                            );

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                dst_asm_type,
                            );
                            let condition = if (!self.is_signed(src1))
                                || src_asm_type == AssemblyType::Double
                            {
                                AsmConditional::BelowOrEqual
                            } else {
                                AsmConditional::LessOrEqual
                            };
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition,
                                operand: asm_dst,
                            });
                        }
                        TacBinaryOp::GreaterOrEqual => {
                            self.emit_cmp(
                                asm_instructions,
                                asm_src1,
                                asm_src2,
                                src_asm_type.clone(),
                            );

                            self.emit_mov(
                                asm_instructions,
                                AsmOperand::Imm(0),
                                asm_dst.clone(),
                                dst_asm_type,
                            );
                            let condition = if (!self.is_signed(src1))
                                || src_asm_type == AssemblyType::Double
                            {
                                AsmConditional::AboveOrEqual
                            } else {
                                AsmConditional::GreaterOrEqual
                            };
                            asm_instructions.push(AsmInstruction::SetCC {
                                condition,
                                operand: asm_dst,
                            });
                        }
                    }
                }
                TacInstruction::FuncCall { name, args, dst } => {
                    let int_arg_registers = [
                        AsmRegister::RDI,
                        AsmRegister::RSI,
                        AsmRegister::RDX,
                        AsmRegister::RCX,
                        AsmRegister::R8,
                        AsmRegister::R9,
                    ];
                    let xmm_arg_registers = [
                        AsmRegister::XMM0,
                        AsmRegister::XMM1,
                        AsmRegister::XMM2,
                        AsmRegister::XMM3,
                        AsmRegister::XMM4,
                        AsmRegister::XMM5,
                        AsmRegister::XMM6,
                        AsmRegister::XMM7,
                    ];
                    let mut int_reg_idx = 0;
                    let mut xmm_reg_idx = 0;

                    let mut reg_args = Vec::new();
                    let mut stack_args = Vec::new();

                    for arg in args.iter() {
                        let asm_op = self.tac_to_asm_operand(arg);
                        let arg_asm_type = self.get_operand_asm_type(arg);

                        if arg_asm_type == AssemblyType::Double {
                            if xmm_reg_idx < xmm_arg_registers.len() {
                                reg_args.push((
                                    asm_op,
                                    AsmOperand::Reg(xmm_arg_registers[xmm_reg_idx].clone()),
                                    arg_asm_type,
                                ));
                                xmm_reg_idx += 1;
                            } else {
                                stack_args.push((asm_op, arg_asm_type));
                            }
                        } else {
                            if int_reg_idx < int_arg_registers.len() {
                                reg_args.push((
                                    asm_op,
                                    AsmOperand::Reg(int_arg_registers[int_reg_idx].clone()),
                                    arg_asm_type,
                                ));
                                int_reg_idx += 1;
                            } else {
                                stack_args.push((asm_op, arg_asm_type));
                            }
                        }
                    }

                    // Align stack to 16-bytes
                    let padding = if (stack_args.len() % 2) != 0 { 8 } else { 0 };
                    if padding != 0 {
                        asm_instructions.push(AsmInstruction::AllocateStack(padding));
                    }

                    for (op, _) in stack_args.iter().rev() {
                        if let AsmOperand::Imm(val) = op {
                            if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                                let tmp = AsmOperand::Reg(AsmRegister::R11);
                                self.emit_mov(
                                    asm_instructions,
                                    op.clone(),
                                    tmp.clone(),
                                    AssemblyType::Qword,
                                );
                                asm_instructions.push(AsmInstruction::Push(tmp));
                                continue;
                            }
                        }
                        asm_instructions.push(AsmInstruction::Push(op.clone()));
                    }

                    for (src, dst, asm_type) in reg_args {
                        self.emit_mov(asm_instructions, src, dst, asm_type);
                    }

                    asm_instructions.push(AsmInstruction::Call(name.clone()));

                    let stack_cleanup = (stack_args.len() as i32 * 8) + padding;
                    if stack_cleanup > 0 {
                        asm_instructions.push(AsmInstruction::DeallocateStack(stack_cleanup));
                    }

                    let dst_op = self.tac_to_asm_operand(dst);
                    let dst_asm_type = self.get_operand_asm_type(dst);

                    let return_reg = if dst_asm_type == AssemblyType::Double {
                        AsmOperand::Reg(AsmRegister::XMM0)
                    } else {
                        AsmOperand::Reg(AsmRegister::RAX)
                    };

                    self.emit_mov(asm_instructions, return_reg, dst_op, dst_asm_type);
                }
                TacInstruction::SignExtend { src, dst } => {
                    let src_op = self.tac_to_asm_operand(src);
                    let dst_op = self.tac_to_asm_operand(dst);
                    self.emit_movsx(asm_instructions, src_op, dst_op);
                }
                TacInstruction::Truncate { src, dst } => {
                    // Truncate is just a move of the lower 32 bits
                    let src_op = self.tac_to_asm_operand(src);
                    let dst_op = self.tac_to_asm_operand(dst);
                    self.emit_mov(asm_instructions, src_op, dst_op, AssemblyType::Dword);
                }
                TacInstruction::ZeroExtend { src, dst } => {
                    let src_op = self.tac_to_asm_operand(src);
                    let dst_op = self.tac_to_asm_operand(dst);
                    self.emit_movzx(asm_instructions, src_op, dst_op);
                }

                TacInstruction::DoubleToInt { src, dst } => {
                    let src_op = self.tac_to_asm_operand(src);
                    let dst_op = self.tac_to_asm_operand(dst);
                    let asm_type = self.get_operand_asm_type(dst);
                    self.emit_cvttsd2si(asm_instructions, src_op, dst_op, asm_type);
                }
                TacInstruction::DoubleToUInt { src, dst, dst_type } => {
                    let src_op = self.tac_to_asm_operand(src);
                    let dst_op = self.tac_to_asm_operand(dst);

                    if *dst_type == Type::UInt {
                        let r = AsmOperand::Reg(AsmRegister::RAX);
                        self.emit_cvttsd2si(
                            asm_instructions,
                            src_op,
                            r.clone(),
                            AssemblyType::Qword,
                        );
                        self.emit_mov(asm_instructions, r, dst_op, AssemblyType::Dword);
                    } else {
                        let label1 = self.new_label();
                        let label2 = self.new_label();
                        let xmm_reg = AsmOperand::Reg(AsmRegister::XMM0);
                        let upper_bound = AsmOperand::Data("upper_bound".to_string());

                        self.emit_cmp(
                            asm_instructions,
                            src_op.clone(),
                            upper_bound.clone(),
                            AssemblyType::Double,
                        );
                        asm_instructions.push(AsmInstruction::JmpCC {
                            condition: AsmConditional::AboveOrEqual,
                            target: label1.clone(),
                        });
                        self.emit_cvttsd2si(
                            asm_instructions,
                            src_op.clone(),
                            dst_op.clone(),
                            AssemblyType::Qword,
                        );
                        asm_instructions.push(AsmInstruction::Jmp(label2.clone()));
                        asm_instructions.push(AsmInstruction::Label(label1));
                        self.emit_mov(
                            asm_instructions,
                            src_op.clone(),
                            xmm_reg.clone(),
                            AssemblyType::Double,
                        );
                        asm_instructions.push(AsmInstruction::Binary {
                            op: AsmBinaryOp::Sub,
                            left: xmm_reg.clone(),
                            right: upper_bound,
                            asm_type: AssemblyType::Double,
                        });
                        self.emit_cvttsd2si(
                            asm_instructions,
                            xmm_reg,
                            dst_op.clone(),
                            AssemblyType::Qword,
                        );
                        let r = AsmOperand::Reg(AsmRegister::R10);
                        self.emit_mov(
                            asm_instructions,
                            AsmOperand::Imm(0x8000000000000000u64 as i64),
                            r.clone(),
                            AssemblyType::Qword,
                        );
                        self.emit_add(asm_instructions, r, dst_op.clone(), AssemblyType::Qword);
                        asm_instructions.push(AsmInstruction::Label(label2));
                    }
                }
                TacInstruction::IntToDouble { src, dst } => {
                    let src_op = self.tac_to_asm_operand(src);
                    let dst_op = self.tac_to_asm_operand(dst);
                    let asm_type = self.get_operand_asm_type(src);

                    self.emit_cvtsi2sd(asm_instructions, src_op, dst_op, asm_type);
                }
                TacInstruction::UIntToDouble { src, dst, src_type } => {
                    let src_op = self.tac_to_asm_operand(src);
                    let dst_op = self.tac_to_asm_operand(dst);

                    if *src_type == Type::UInt {
                        let tmp = AsmOperand::Reg(AsmRegister::RAX);
                        self.emit_movzx(asm_instructions, src_op, tmp.clone());
                        self.emit_cvtsi2sd(asm_instructions, tmp, dst_op, AssemblyType::Qword);
                    } else {
                        let label1 = self.new_label();
                        let label2 = self.new_label();
                        let r1 = AsmOperand::Reg(AsmRegister::R10);
                        let r2 = AsmOperand::Reg(AsmRegister::R11);

                        self.emit_cmp(
                            asm_instructions,
                            src_op.clone(),
                            AsmOperand::Imm(0),
                            AssemblyType::Qword,
                        );
                        asm_instructions.push(AsmInstruction::JmpCC {
                            condition: AsmConditional::Less,
                            target: label1.clone(),
                        });
                        self.emit_cvtsi2sd(
                            asm_instructions,
                            src_op.clone(),
                            dst_op.clone(),
                            AssemblyType::Qword,
                        );
                        asm_instructions.push(AsmInstruction::Jmp(label2.clone()));
                        asm_instructions.push(AsmInstruction::Label(label1));
                        self.emit_mov(
                            asm_instructions,
                            src_op.clone(),
                            r1.clone(),
                            AssemblyType::Qword,
                        );
                        self.emit_mov(
                            asm_instructions,
                            r1.clone(),
                            r2.clone(),
                            AssemblyType::Qword,
                        );
                        asm_instructions.push(AsmInstruction::Unary {
                            op: AsmUnaryOp::Shr,
                            operand: r2.clone(),
                            asm_type: AssemblyType::Qword,
                        });
                        asm_instructions.push(AsmInstruction::Binary {
                            op: AsmBinaryOp::And,
                            left: r1.clone(),
                            right: AsmOperand::Imm(1),
                            asm_type: AssemblyType::Qword,
                        });
                        asm_instructions.push(AsmInstruction::Binary {
                            op: AsmBinaryOp::Or,
                            left: r2.clone(),
                            right: r1.clone(),
                            asm_type: AssemblyType::Qword,
                        });
                        self.emit_cvtsi2sd(
                            asm_instructions,
                            r2.clone(),
                            dst_op.clone(),
                            AssemblyType::Qword,
                        );
                        self.emit_add(
                            asm_instructions,
                            dst_op.clone(),
                            dst_op.clone(),
                            AssemblyType::Double,
                        );

                        asm_instructions.push(AsmInstruction::Label(label2));
                    }
                }
            }
        }
    }

    fn emit_mov(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
        asm_type: AssemblyType,
    ) {
        let src = if let AsmOperand::Imm(val) = src {
            if asm_type == AssemblyType::Dword {
                AsmOperand::Imm(val as i32 as i64)
            } else {
                AsmOperand::Imm(val)
            }
        } else {
            src
        };

        if self.is_memory(&dst) && self.is_memory(&src) {
            let tmp_reg = if asm_type == AssemblyType::Double {
                AsmOperand::Reg(AsmRegister::XMM15)
            } else {
                AsmOperand::Reg(AsmRegister::R10)
            };
            let mov_asm_type = if asm_type == AssemblyType::Double {
                AssemblyType::Double
            } else {
                asm_type.clone()
            };
            asm_instructions.push(AsmInstruction::Mov {
                src,
                dst: tmp_reg.clone(),
                asm_type: mov_asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Mov {
                src: tmp_reg,
                dst,
                asm_type: mov_asm_type,
            });
        } else {
            // Check if we are moving a large immediate to memory
            // x86-64 does not allow move of 64b Imm to Memory directly
            let requires_tmp_reg = if let AsmOperand::Imm(val) = &src {
                if self.is_memory(&dst) && asm_type == AssemblyType::Qword {
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
                    asm_type: asm_type.clone(),
                });
                asm_instructions.push(AsmInstruction::Mov {
                    src: tmp_reg,
                    dst,
                    asm_type,
                });
            } else {
                // Direct move is fine
                asm_instructions.push(AsmInstruction::Mov { src, dst, asm_type });
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
        asm_type: AssemblyType,
        signed: bool,
    ) {
        if let AsmOperand::Imm(_) = &operand {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = if asm_type == AssemblyType::Double {
                AsmOperand::Reg(AsmRegister::XMM15)
            } else {
                AsmOperand::Reg(AsmRegister::R10)
            };
            asm_instructions.push(AsmInstruction::Mov {
                src: operand,
                dst: tmp_reg.clone(),
                asm_type: asm_type.clone(),
            });
            if signed {
                asm_instructions.push(AsmInstruction::Idiv {
                    operand: tmp_reg,
                    asm_type,
                });
            } else {
                asm_instructions.push(AsmInstruction::Div {
                    operand: tmp_reg,
                    asm_type,
                });
            }
        } else if signed {
            asm_instructions.push(AsmInstruction::Idiv { operand, asm_type })
        } else {
            asm_instructions.push(AsmInstruction::Div { operand, asm_type });
        }
    }

    // Avoid memory to memory operations
    // add dst, src
    fn emit_add(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
        asm_type: AssemblyType,
    ) {
        if asm_type == AssemblyType::Double {
            self.emit_double_binary_op(asm_instructions, AsmBinaryOp::Add, src, dst, asm_type);
            return;
        }

        let src_op = if let AsmOperand::Imm(val) = &src {
            if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                let tmp = if asm_type == AssemblyType::Double {
                    AsmOperand::Reg(AsmRegister::XMM15)
                } else {
                    AsmOperand::Reg(AsmRegister::R10)
                };
                asm_instructions.push(AsmInstruction::Mov {
                    src: src.clone(),
                    dst: tmp.clone(),
                    asm_type: asm_type.clone(),
                });
                tmp
            } else {
                src.clone()
            }
        } else {
            src.clone()
        };
        if self.is_memory(&dst) && self.is_memory(&src_op) {
            let tmp_reg = if asm_type == AssemblyType::Double {
                AsmOperand::Reg(AsmRegister::XMM15)
            } else {
                AsmOperand::Reg(AsmRegister::R10)
            };
            asm_instructions.push(AsmInstruction::Mov {
                src: src_op.clone(),
                dst: tmp_reg.clone(),
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Add,
                left: dst,
                right: tmp_reg,
                asm_type,
            });
        } else if self.is_memory(&dst) {
            let tmp_reg = if asm_type == AssemblyType::Double {
                AsmOperand::Reg(AsmRegister::XMM15)
            } else {
                AsmOperand::Reg(AsmRegister::R11)
            };
            asm_instructions.push(AsmInstruction::Mov {
                src: dst.clone(),
                dst: tmp_reg.clone(),
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Add,
                left: tmp_reg.clone(),
                right: src_op,
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Mov {
                src: tmp_reg,
                dst,
                asm_type,
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Add,
                left: dst,
                right: src_op,
                asm_type,
            });
        }
    }

    fn emit_double_binary_op(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        op: AsmBinaryOp,
        src: AsmOperand,
        dst: AsmOperand,
        asm_type: AssemblyType,
    ) {
        if self.is_memory(&dst) {
            let tmp_reg = AsmOperand::Reg(AsmRegister::XMM15);
            self.emit_mov(
                asm_instructions,
                dst.clone(),
                tmp_reg.clone(),
                asm_type.clone(),
            );
            asm_instructions.push(AsmInstruction::Binary {
                op,
                left: tmp_reg.clone(),
                right: src,
                asm_type: asm_type.clone(),
            });
            self.emit_mov(asm_instructions, tmp_reg, dst, asm_type);
        } else {
            asm_instructions.push(AsmInstruction::Binary {
                op,
                left: dst,
                right: src,
                asm_type,
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
        asm_type: AssemblyType,
    ) {
        if asm_type == AssemblyType::Double {
            self.emit_double_binary_op(asm_instructions, AsmBinaryOp::Sub, src, dst, asm_type);
            return;
        }

        let src_op = if let AsmOperand::Imm(val) = &src {
            if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                let tmp = AsmOperand::Reg(AsmRegister::R10);
                asm_instructions.push(AsmInstruction::Mov {
                    src: src.clone(),
                    dst: tmp.clone(),
                    asm_type: asm_type.clone(),
                });
                tmp
            } else {
                src.clone()
            }
        } else {
            src.clone()
        };
        if self.is_memory(&dst) && self.is_memory(&src_op) {
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
            asm_instructions.push(AsmInstruction::Mov {
                src: src_op.clone(),
                dst: tmp_reg.clone(),
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Sub,
                left: dst,
                right: tmp_reg,
                asm_type,
            });
        } else if self.is_memory(&dst) {
            let tmp_reg = AsmOperand::Reg(AsmRegister::R11);
            asm_instructions.push(AsmInstruction::Mov {
                src: dst.clone(),
                dst: tmp_reg.clone(),
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Sub,
                left: tmp_reg.clone(),
                right: src_op,
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Mov {
                src: tmp_reg,
                dst,
                asm_type,
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Sub,
                left: dst,
                right: src_op,
                asm_type,
            });
        }
    }

    // Avoid memory to memory operations
    // imul dst, src
    fn emit_mul(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
        asm_type: AssemblyType,
    ) {
        if asm_type == AssemblyType::Double {
            self.emit_double_binary_op(asm_instructions, AsmBinaryOp::Mult, src, dst, asm_type);
            return;
        }

        let src_op = if let AsmOperand::Imm(val) = &src {
            if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                let tmp = AsmOperand::Reg(AsmRegister::R10);
                asm_instructions.push(AsmInstruction::Mov {
                    src: src.clone(),
                    dst: tmp.clone(),
                    asm_type: asm_type.clone(),
                });
                tmp
            } else {
                src.clone()
            }
        } else {
            src.clone()
        };

        if self.is_memory(&dst) {
            let tmp_reg = if asm_type == AssemblyType::Double {
                AsmOperand::Reg(AsmRegister::XMM15)
            } else {
                AsmOperand::Reg(AsmRegister::R11)
            };
            asm_instructions.push(AsmInstruction::Mov {
                src: dst.clone(),
                dst: tmp_reg.clone(),
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Mult,
                left: tmp_reg.clone(),
                right: src_op,
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Mov {
                src: tmp_reg,
                dst,
                asm_type,
            });
        } else {
            // Direct move is fine
            asm_instructions.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Mult,
                left: dst,
                right: src_op,
                asm_type,
            });
        }
    }

    fn emit_cvttsd2si(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
        asm_type: AssemblyType,
    ) {
        if !matches!(dst, AsmOperand::Reg(_)) {
            let tmp_reg = AsmOperand::Reg(AsmRegister::R11);
            asm_instructions.push(AsmInstruction::Cvttsd2si {
                src,
                dst: tmp_reg.clone(),
                asm_type: asm_type.clone(),
            });
            self.emit_mov(asm_instructions, tmp_reg, dst, asm_type);
        } else {
            asm_instructions.push(AsmInstruction::Cvttsd2si { src, dst, asm_type });
        }
    }

    fn emit_cvtsi2sd(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        src: AsmOperand,
        dst: AsmOperand,
        asm_type: AssemblyType,
    ) {
        let mut final_src = src;
        if matches!(final_src, AsmOperand::Imm(_)) {
            let tmp_reg = AsmOperand::Reg(AsmRegister::R10);
            self.emit_mov(
                asm_instructions,
                final_src,
                tmp_reg.clone(),
                asm_type.clone(),
            );
            final_src = tmp_reg;
        }

        if !matches!(dst, AsmOperand::Reg(_)) {
            let tmp_reg = AsmOperand::Reg(AsmRegister::XMM15);
            asm_instructions.push(AsmInstruction::Cvtsi2sd {
                src: final_src,
                dst: tmp_reg.clone(),
                asm_type: asm_type.clone(),
            });
            self.emit_mov(asm_instructions, tmp_reg, dst, AssemblyType::Double);
        } else {
            asm_instructions.push(AsmInstruction::Cvtsi2sd {
                src: final_src,
                dst,
                asm_type,
            });
        }
    }

    fn emit_cmp(
        &mut self,
        asm_instructions: &mut Vec<AsmInstruction>,
        left: AsmOperand,
        right: AsmOperand,
        asm_type: AssemblyType,
    ) {
        if asm_type == AssemblyType::Double {
            let mut final_left = left;
            if self.is_memory(&final_left) {
                let tmp_reg = AsmOperand::Reg(AsmRegister::XMM14);
                self.emit_mov(
                    asm_instructions,
                    final_left,
                    tmp_reg.clone(),
                    asm_type.clone(),
                );
                final_left = tmp_reg;
            }
            let mut final_right = right;
            if !matches!(final_right, AsmOperand::Reg(_)) {
                let tmp_reg = AsmOperand::Reg(AsmRegister::XMM15);
                self.emit_mov(
                    asm_instructions,
                    final_right,
                    tmp_reg.clone(),
                    asm_type.clone(),
                );
                final_right = tmp_reg;
            }
            asm_instructions.push(AsmInstruction::Cmp {
                left: final_left,
                right: final_right,
                asm_type,
            });
            return;
        }

        let right_op = if let AsmOperand::Imm(val) = &right {
            if *val > i32::MAX as i64 || *val < i32::MIN as i64 {
                let tmp = AsmOperand::Reg(AsmRegister::R10);
                asm_instructions.push(AsmInstruction::Mov {
                    src: right.clone(),
                    dst: tmp.clone(),
                    asm_type: asm_type.clone(),
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
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Cmp {
                left,
                right: tmp_reg.clone(),
                asm_type,
            });
        } else if let AsmOperand::Imm(_) = &left {
            // Use R10 as a temporary register for mem-to-mem moves
            let tmp_reg = AsmOperand::Reg(AsmRegister::R11);
            asm_instructions.push(AsmInstruction::Mov {
                src: left.clone(),
                dst: tmp_reg.clone(),
                asm_type: asm_type.clone(),
            });
            asm_instructions.push(AsmInstruction::Cmp {
                left: tmp_reg.clone(),
                right: right_op,
                asm_type,
            });
        } else {
            asm_instructions.push(AsmInstruction::Cmp {
                left,
                right: right_op,
                asm_type,
            });
        }
    }

    fn tac_to_asm_operand(&mut self, tac_operand: &TacOperand) -> AsmOperand {
        match tac_operand {
            TacOperand::Const(val) => match val {
                Const::ConstDouble(d) => {
                    let label = self.new_data_label();
                    self.top_level.push(AsmTopLevel::StaticConstant {
                        name: label.clone(),
                        init: StaticInit::Double(*d),
                        alignment: 8,
                    });
                    AsmOperand::Data(label)
                }
                _ => {
                    let num: i64 = match val {
                        Const::ConstInt(val) => *val as i64,
                        Const::ConstUInt(val) => *val as i64,
                        Const::ConstLong(val) => *val,
                        Const::ConstULong(val) => *val as i64,
                        Const::ConstDouble(_) => unreachable!(),
                    };
                    AsmOperand::Imm(num)
                }
            },
            TacOperand::Var {
                name,
                var_type,
                is_static,
            } => {
                if *is_static {
                    AsmOperand::Data(name.clone())
                } else {
                    let size = self.get_type_size(var_type);
                    AsmOperand::Stack(self.get_var_offset(name, size as i32))
                }
            }
        }
    }

    fn get_operand_asm_type(&self, operand: &TacOperand) -> AssemblyType {
        match operand {
            TacOperand::Var { var_type, .. } => self.type_to_asm_type(var_type),
            TacOperand::Const(val) => match val {
                Const::ConstInt(_) => AssemblyType::Dword,
                Const::ConstUInt(_) => AssemblyType::Dword,
                Const::ConstLong(_) => AssemblyType::Qword,
                Const::ConstULong(_) => AssemblyType::Qword,
                Const::ConstDouble(_) => AssemblyType::Double,
            },
        }
    }

    fn type_to_asm_type(&self, var_type: &Type) -> AssemblyType {
        match var_type {
            Type::Int | Type::UInt => AssemblyType::Dword,
            Type::Long | Type::ULong => AssemblyType::Qword,
            Type::Double => AssemblyType::Double,
            Type::FuncType { .. } => AssemblyType::Qword,
        }
    }

    fn get_type_size(&self, var_type: &Type) -> u32 {
        match var_type {
            Type::Int | Type::UInt => 4,
            Type::Long | Type::ULong => 8,
            _ => 8,
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
            TacOperand::Var { var_type, .. } => matches!(var_type, Type::Int | Type::Long),
            TacOperand::Const(c) => matches!(c, Const::ConstInt(_) | Const::ConstLong(_)),
        }
    }
}
