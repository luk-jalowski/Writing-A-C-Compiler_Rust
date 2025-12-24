use core::panic;

use crate::ast::{
    AST, BinaryOperator, BlockItem, Declaration, Expression, ForInit, FunDecl, Statement,
    UnaryOperator, VarDecl,
};

#[derive(Debug)]
pub enum TacAst {
    TacProgram(Vec<TacFunction>),
}
#[derive(Debug)]
pub struct TacFunction {
    pub name: String,
    pub params: Vec<String>,
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

    FuncCall {
        name: String,
        args: Vec<TacOperand>,
        dst: TacOperand,
    },
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

    fn new_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    pub fn generate_tac(&mut self, ast: AST) -> TacAst {
        match ast {
            AST::Program(declarations) => {
                let mut functions = Vec::new();
                for decl in declarations {
                    if let Declaration::FunDecl(func) = decl {
                        if func.body.is_some() {
                            functions.push(self.parse_function(func));
                        }
                    }
                }
                TacAst::TacProgram(functions)
            }
        }
    }

    fn parse_function(&mut self, function: FunDecl) -> TacFunction {
        let mut tac_instructions: Vec<TacInstruction> = Vec::new();

        if let Some(body) = function.body {
            for block_item in body.block_items {
                self.parse_block_item(block_item, &mut tac_instructions);
            }
        };

        // Add 0 if there is no implicit return
        if !matches!(tac_instructions.last(), Some(TacInstruction::Ret(_))) {
            tac_instructions.push(TacInstruction::Ret(TacOperand::Const(0)));
        }

        TacFunction {
            name: function.name,
            params: function.params,
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
        match declaration {
            Declaration::VarDecl(var_decl) => {
                self.parse_var_declaration(var_decl, tac_instructions);
            }
            Declaration::FunDecl(func_decl) => {
                self.parse_function(func_decl);
            }
        }
    }

    fn parse_var_declaration(
        &mut self,
        var_decl: VarDecl,
        tac_instructions: &mut Vec<TacInstruction>,
    ) {
        if let Some(expr) = var_decl.init {
            let src = self.parse_expression(expr, tac_instructions);
            tac_instructions.push(TacInstruction::Copy {
                src,
                dst: TacOperand::Var(var_decl.name),
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
            Statement::If {
                exp,
                then,
                else_statement,
            } => {
                let cond_result = self.parse_expression(exp, tac_instructions);
                let end_label = self.new_label();
                // Handle if else
                if let Some(else_stmnt) = else_statement {
                    let else_label = self.new_label();

                    tac_instructions.push(TacInstruction::JumpIfZero {
                        condition: cond_result,
                        target: else_label.clone(),
                    });

                    self.parse_statement(*then, tac_instructions);
                    tac_instructions.push(TacInstruction::Jump {
                        target: end_label.clone(),
                    });
                    tac_instructions.push(TacInstruction::Label(else_label));
                    self.parse_statement(*else_stmnt, tac_instructions);
                    tac_instructions.push(TacInstruction::Label(end_label));
                } else {
                    // handle just if

                    tac_instructions.push(TacInstruction::JumpIfZero {
                        condition: cond_result,
                        target: end_label.clone(),
                    });

                    self.parse_statement(*then, tac_instructions);
                    tac_instructions.push(TacInstruction::Label(end_label));
                }
            }
            Statement::Compound(block) => {
                for block_item in block.block_items {
                    self.parse_block_item(block_item, tac_instructions);
                }
            }
            Statement::Break { label } => {
                tac_instructions.push(TacInstruction::Jump { target: label });
            }
            Statement::Continue { label } => {
                tac_instructions.push(TacInstruction::Jump { target: label });
            }
            Statement::DoWhile {
                body,
                condition,
                label,
            } => {
                let break_target = format!("break_{}", label);
                let continue_target = format!("continue_{}", label);

                tac_instructions.push(TacInstruction::Label(label.clone()));
                self.parse_statement(*body, tac_instructions);
                tac_instructions.push(TacInstruction::Label(continue_target));
                let result = self.parse_expression(condition, tac_instructions);
                tac_instructions.push(TacInstruction::JumpIfNotZero {
                    condition: result,
                    target: label,
                });
                tac_instructions.push(TacInstruction::Label(break_target));
            }
            Statement::For {
                init,
                condition,
                post,
                body,
                label,
            } => {
                let break_target = format!("break_{}", label);
                let continue_target = format!("continue_{}", label);

                self.parse_for_init(init, tac_instructions);
                tac_instructions.push(TacInstruction::Label(label.clone()));

                if let Some(for_cond) = condition {
                    let cond_result = self.parse_expression(*for_cond, tac_instructions);
                    tac_instructions.push(TacInstruction::JumpIfZero {
                        condition: cond_result,
                        target: break_target.clone(),
                    });
                }

                self.parse_statement(*body, tac_instructions);
                tac_instructions.push(TacInstruction::Label(continue_target));
                if let Some(for_post) = post {
                    self.parse_expression(*for_post, tac_instructions);
                }
                tac_instructions.push(TacInstruction::Jump { target: label });
                tac_instructions.push(TacInstruction::Label(break_target));
            }
            Statement::While {
                condition,
                body,
                label,
            } => {
                let break_target = format!("break_{}", label);
                let continue_target = format!("continue_{}", label);

                tac_instructions.push(TacInstruction::Label(continue_target.clone()));
                let result = self.parse_expression(condition, tac_instructions);
                tac_instructions.push(TacInstruction::JumpIfZero {
                    condition: result,
                    target: break_target.clone(),
                });
                self.parse_statement(*body, tac_instructions);
                tac_instructions.push(TacInstruction::Jump {
                    target: continue_target,
                });
                tac_instructions.push(TacInstruction::Label(break_target));
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
                    UnaryOperator::Not => TacUnaryOp::Not,
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
                        let false_label = self.new_label();
                        let end_label = self.new_label();
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
                        let true_label = self.new_label();
                        let end_label = self.new_label();
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
            Expression::Conditional {
                condition,
                exp1,
                exp2,
            } => {
                let e2 = self.new_label();
                let end_label = self.new_label();
                let result = self.new_temp_var();

                // <condition> ? <e1> : <e2>
                let src_cond = self.parse_expression(*condition, tac_instructions);
                tac_instructions.push(TacInstruction::JumpIfZero {
                    condition: src_cond,
                    target: e2.clone(),
                });

                //main body
                let src_exp1 = self.parse_expression(*exp1, tac_instructions);
                tac_instructions.push(TacInstruction::Copy {
                    src: src_exp1,
                    dst: result.clone(),
                });
                tac_instructions.push(TacInstruction::Jump {
                    target: end_label.clone(),
                });

                // else
                tac_instructions.push(TacInstruction::Label(e2));
                let src_exp2 = self.parse_expression(*exp2, tac_instructions);
                tac_instructions.push(TacInstruction::Copy {
                    src: src_exp2,
                    dst: result.clone(),
                });

                tac_instructions.push(TacInstruction::Label(end_label));
                result
            }
            Expression::FunctionCall { name, args } => {
                let mut call_args = Vec::new();
                let result = self.new_temp_var();
                for arg in args {
                    call_args.push(self.parse_expression(arg, tac_instructions));
                }
                tac_instructions.push(TacInstruction::FuncCall {
                    name,
                    args: call_args,
                    dst: result.clone(),
                });
                result
            }
        }
    }

    pub fn parse_for_init(
        &mut self,
        for_init: ForInit,
        tac_instructions: &mut Vec<TacInstruction>,
    ) {
        match for_init {
            ForInit::InitDeclaration(decl) => {
                self.parse_var_declaration(decl, tac_instructions);
            }
            ForInit::InitExpression(expr) => {
                if let Some(e) = expr {
                    self.parse_expression(*e, tac_instructions);
                }
            }
        }
    }
}
