use core::panic;

use crate::parser::{AST, BinaryOperator, Expression, Function, Statement, UnaryOperator};

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
}

#[derive(Debug, Clone)]
pub enum TacOperand {
    Var(String),
    Const(i32),
}

#[derive(Debug)]
pub enum TacUnaryOp {
    Complement,
    Negate,
}

#[derive(Debug)]
pub enum TacBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

#[derive(Debug)]
pub struct TacProgram {
    // pub tac_instructions: Vec<TacInstruction>,
    var_counter: usize,
}

impl TacProgram {
    pub fn new() -> Self {
        TacProgram { var_counter: 0 }
    }

    fn new_temp_var(&mut self) -> TacOperand {
        let tmp = format!("tmp.{}", self.var_counter);
        self.var_counter += 1;
        TacOperand::Var(tmp.to_string())
    }

    pub fn generate_tac(&mut self, ast: AST) -> TacAst {
        match ast {
            AST::Program(function) => TacAst::TacProgram(self.parse_function(function)),
        }
    }

    fn parse_function(&mut self, function: Function) -> TacFunction {
        let mut tac_instructions: Vec<TacInstruction> = Vec::new();

        self.parse_statement(function.body, &mut tac_instructions);

        TacFunction {
            name: function.name,
            body: tac_instructions,
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
                };

                tac_instructions.push(TacInstruction::Unary {
                    op: tac_op,
                    src,
                    dst: dst.clone(),
                });
                dst
            }
            Expression::BinaryExp(left, op, right) => {
                let src1 = self.parse_expression(*left, tac_instructions);
                let src2 = self.parse_expression(*right, tac_instructions);
                let dst = self.new_temp_var();

                let tac_op = match op {
                    BinaryOperator::Addition => TacBinaryOp::Add,
                    BinaryOperator::Subtraction => TacBinaryOp::Subtract,
                    BinaryOperator::Multiplication => TacBinaryOp::Multiply,
                    BinaryOperator::Division => TacBinaryOp::Divide,
                    BinaryOperator::Modulo => TacBinaryOp::Remainder,
                };
                tac_instructions.push(TacInstruction::Binary {
                    op: tac_op,
                    src1,
                    src2,
                    dst: dst.clone(),
                });

                dst
            }
        }
    }
}
