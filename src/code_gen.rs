use crate::parser::{AST, Expression, Function, Statement};


#[derive(Debug)]
pub enum Instruction {
    MOV { src: Operand, dst: Operand },
    Ret,
    Label(String),
}

#[derive(Debug)]
pub enum Operand {
    Reg(Register),
    Imm(i32),
}

#[derive(Debug)]
pub enum Register {
    AX,
    EAX,
    RAX,
    // ...
}

#[derive(Debug)]
pub struct AsmProgram {
    pub instructions: Vec<Instruction>,
}

impl AsmProgram {
    pub fn new() -> Self {
        AsmProgram {
            instructions: Vec::new(),
        }
    }
    pub fn generate_code(&mut self, ast: AST) {
        match ast {
            AST::Program(function) => {
                self.parse_function(function);
            }
        }
    }

    fn parse_function(&mut self, function: Function) {
        self.instructions.push(Instruction::Label(function.name));

        self.parse_statement(function.body);
    }

    fn parse_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Return(expr) => {
                self.parse_expression(expr);
                self.instructions.push(Instruction::Ret);
            }
        }
    }

    fn parse_expression(&mut self, expression: Expression) {
        match expression {
            Expression::Constant(val) => {
                self.instructions.push(Instruction::MOV {
                    src: Operand::Imm(val),
                    dst: Operand::Reg(Register::EAX),
                });
            }
        }
    }
}
