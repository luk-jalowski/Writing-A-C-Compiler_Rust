use core::panic;

use crate::parser::{
    AST, BinaryOperator, BlockItem, Declaration, Expression, Function, Statement, UnaryOperator,
};

#[derive(Debug)]
pub enum TacAst {
    TacProgram(TacFunction),
}
#[derive(Debug)]
pub struct TacFunction {
    pub name: String, // identifier
    pub body: Vec<TacInstruction>,
}

#[derive(Debug)]
pub enum TacInstruction {
    Unary {
        op: TacUnaryOp,
        src: TacOperand,
        dst: TacOperand,
    },
    Ret(TacOperand),
    Binary {
        op: TacBinaryOp,
        src1: TacOperand,
        src2: TacOperand,
        dst: TacOperand,
    },
    Copy {
        src: TacOperand,
        dst: TacOperand,
    },
    Jump {
        target: String,
    },
    JumpIfZero {
        condition: TacOperand,
        target: String,
    },
    JumpIfNotZero {
        condition: TacOperand,
        target: String,
    },
    Label(String),
}

#[derive(Debug, Clone)]
pub enum TacOperand {
    Var(String),
    Const(i32),
}

#[derive(Debug, PartialEq)]
pub enum TacUnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum TacBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    EqualTo,
    NotEqualTo,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug)]
pub struct TacProgram {
    var_counter: usize,
    label_counter: usize,
}

impl TacProgram {
    pub fn new(var_counter: usize, label_counter: usize) -> Self {
        TacProgram {
            var_counter,
            label_counter,
        }
    }

    fn new_temp_var(&mut self) -> TacOperand {
        let tmp = format!("tmp.{}", self.var_counter);
        self.var_counter += 1;
        TacOperand::Var(tmp.to_string())
    }

    fn new_label_(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    pub fn generate_tac(&mut self, ast: AST) -> TacAst {
        match ast {
            AST::Program(function) => TacAst::TacProgram(self.parse_function(function)),
        }
    }

    fn parse_function(&mut self, function: Function) -> TacFunction {
        let mut tac_instructions: Vec<TacInstruction> = Vec::new();

        for block_item in function.body {
            self.parse_block_item(block_item, &mut tac_instructions);
        }

        // Add 0 if there is no implicit return
        if !matches!(tac_instructions.last(), Some(TacInstruction::Ret(_))) {
            tac_instructions.push(TacInstruction::Ret(TacOperand::Const(0)));
        }

        TacFunction {
            name: function.name,
            body: tac_instructions,
        }
    }

    fn parse_block_item(
        &mut self,
        block_item: BlockItem,
        tac_instructions: &mut Vec<TacInstruction>,
    ) {
        match block_item {
            BlockItem::Statement(statement) => self.parse_statement(statement, tac_instructions),
            BlockItem::Declaration(declaration) => {
                self.parse_declaration(declaration, tac_instructions)
            }
        }
    }

    fn parse_declaration(
        &mut self,
        declaration: Declaration,
        tac_instructions: &mut Vec<TacInstruction>,
    ) {
        if let Some(expr) = declaration.init {
            let src = self.parse_expression(expr, tac_instructions);
            tac_instructions.push(TacInstruction::Copy {
                src,
                dst: TacOperand::Var(declaration.name),
            });
        }
    }

    fn parse_statement(
        &mut self,
        statement: Statement,
        tac_instructions: &mut Vec<TacInstruction>,
    ) {
        match statement {
            Statement::Return(expr) => {
                let result_operand = self.parse_expression(expr, tac_instructions);
                tac_instructions.push(TacInstruction::Ret(result_operand));
            }
            Statement::Expression(expr) => {
                self.parse_expression(expr, tac_instructions);
            }
            Statement::Null => {}
        }
    }

    fn parse_expression(
        &mut self,
        expression: Expression,
        tac_instructions: &mut Vec<TacInstruction>,
    ) -> TacOperand {
        match expression {
            Expression::Constant(val) => TacOperand::Const(val),
            Expression::UnaryExpr(op, inner_expr) => {
                let src = self.parse_expression(*inner_expr, tac_instructions);
                let dst = self.new_temp_var();
                let tac_op = match op {
                    UnaryOperator::Complement => TacUnaryOp::Complement,
                    UnaryOperator::Negate => TacUnaryOp::Negate,
                    UnaryOperator::Not => TacUnaryOp::Not,
                    _ => {
                        panic!("Unsupported operator in tac {:?}", op);
                    }
                };

                tac_instructions.push(TacInstruction::Unary {
                    op: tac_op,
                    src,
                    dst: dst.clone(),
                });
                dst
            }
            Expression::BinaryExp(left, op, right) => {
                // First handle short-circuting logical operators AND OR
                match op {
                    BinaryOperator::And => {
                        let false_label = self.new_label_();
                        let end_label = self.new_label_();
                        let dst = self.new_temp_var();

                        let src_left = self.parse_expression(*left, tac_instructions);

                        tac_instructions.push(TacInstruction::JumpIfZero {
                            condition: src_left,
                            target: false_label.clone(),
                        });
                        let src_right = self.parse_expression(*right, tac_instructions);
                        tac_instructions.push(TacInstruction::JumpIfZero {
                            condition: src_right,
                            target: false_label.clone(),
                        });
                        tac_instructions.push(TacInstruction::Copy {
                            src: TacOperand::Const(1),
                            dst: dst.clone(),
                        });
                        tac_instructions.push(TacInstruction::Jump {
                            target: end_label.clone(),
                        });

                        tac_instructions.push(TacInstruction::Label(false_label));

                        tac_instructions.push(TacInstruction::Copy {
                            src: TacOperand::Const(0),
                            dst: dst.clone(),
                        });
                        tac_instructions.push(TacInstruction::Label(end_label));

                        return dst;
                    }
                    BinaryOperator::Or => {
                        let true_label = self.new_label_();
                        let end_label = self.new_label_();
                        let dst = self.new_temp_var();

                        let src_left = self.parse_expression(*left, tac_instructions);

                        tac_instructions.push(TacInstruction::JumpIfNotZero {
                            condition: src_left,
                            target: true_label.clone(),
                        });
                        let src_right = self.parse_expression(*right, tac_instructions);
                        tac_instructions.push(TacInstruction::JumpIfNotZero {
                            condition: src_right,
                            target: true_label.clone(),
                        });
                        tac_instructions.push(TacInstruction::Copy {
                            src: TacOperand::Const(0),
                            dst: dst.clone(),
                        });
                        tac_instructions.push(TacInstruction::Jump {
                            target: end_label.clone(),
                        });

                        tac_instructions.push(TacInstruction::Label(true_label));

                        tac_instructions.push(TacInstruction::Copy {
                            src: TacOperand::Const(1),
                            dst: dst.clone(),
                        });
                        tac_instructions.push(TacInstruction::Label(end_label));

                        return dst;
                    }
                    _ => {}
                }
                let src_left = self.parse_expression(*left, tac_instructions);
                let src_right = self.parse_expression(*right, tac_instructions);
                let dst = self.new_temp_var();

                let tac_op = match op {
                    BinaryOperator::Addition => TacBinaryOp::Add,
                    BinaryOperator::Subtraction => TacBinaryOp::Subtract,
                    BinaryOperator::Multiplication => TacBinaryOp::Multiply,
                    BinaryOperator::Division => TacBinaryOp::Divide,
                    BinaryOperator::Modulo => TacBinaryOp::Remainder,
                    BinaryOperator::EqualTo => TacBinaryOp::EqualTo,
                    BinaryOperator::NotEqualTo => TacBinaryOp::NotEqualTo,
                    BinaryOperator::LessThan => TacBinaryOp::LessThan,
                    BinaryOperator::LessOrEqual => TacBinaryOp::LessOrEqual,
                    BinaryOperator::GreaterThan => TacBinaryOp::GreaterThan,
                    BinaryOperator::GreaterOrEqual => TacBinaryOp::GreaterOrEqual,
                    _ => {
                        panic!("Unsupported binary operator in TAC {:?}", op);
                    }
                };
                tac_instructions.push(TacInstruction::Binary {
                    op: tac_op,
                    src1: src_left,
                    src2: src_right,
                    dst: dst.clone(),
                });

                dst
            }
            Expression::Var(name) => TacOperand::Var(name),
            Expression::Assignment(left, right) => {
                let src_right = self.parse_expression(*right, tac_instructions);

                if let Expression::Var(name) = *left {
                    tac_instructions.push(TacInstruction::Copy {
                        src: src_right.clone(),
                        dst: TacOperand::Var(name),
                    });
                    src_right
                } else {
                    panic!("Expected lvalue for assignment");
                }
            }
        }
    }
}
