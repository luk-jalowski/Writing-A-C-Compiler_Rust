use core::panic;
use std::collections::HashMap;

use crate::ast::{
    AST, Block, BlockItem, Declaration, Expression, ForInit, FunDecl, Statement, VarDecl,
};

#[derive(Debug, PartialEq)]
enum Type {
    Int,
    FunType { param_count: u32 },
}

pub struct TypeValidation {
    symbols: HashMap<String, (Type, bool)>,
}

impl TypeValidation {
    pub fn new() -> Self {
        TypeValidation {
            symbols: HashMap::new(),
        }
    }

    pub fn validate(&mut self, ast: &mut AST) {
        match ast {
            AST::Program(declarations) => {
                for decl in declarations {
                    self.typecheck_declaration(decl);
                }
            }
        }
    }

    fn typecheck_declaration(&mut self, declaration: &mut Declaration) {
        match declaration {
            Declaration::VarDecl(var_decl) => self.typecheck_var_declaration(var_decl),
            Declaration::FunDecl(func_decl) => self.typecheck_function_declaration(func_decl),
        }
    }

    fn typecheck_function_declaration(&mut self, function: &mut FunDecl) {
        let has_body = match function.body {
            Some(_) => true,
            None => false,
        };
        let mut is_defined = has_body;

        if let Some((prev_decl, prev_defined)) = self.symbols.get(&function.name) {
            let param_count = match prev_decl {
                Type::FunType { param_count } => *param_count,
                _ => {
                    panic!(
                        "typecheck_function_declaration Expected function type {:?}",
                        prev_decl
                    );
                }
            };
            if param_count != function.params.len() as u32 {
                panic!(
                    "typecheck_function_declaration Found duplicate function {} with different argument number {}, previous count {}",
                    function.name,
                    function.params.len(),
                    param_count
                );
            }
            if *prev_defined && has_body {
                panic!(
                    "typecheck_function_declaration Found duplicate function {} that has already been defined",
                    function.name,
                );
            }

            if *prev_defined {
                is_defined = true;
            }
        }

        self.symbols.insert(
            function.name.clone(),
            (
                Type::FunType {
                    param_count: function.params.len() as u32,
                },
                is_defined,
            ),
        );

        if has_body {
            for param in &mut function.params {
                self.symbols.insert(param.clone(), (Type::Int, true));
            }
            if let Some(block) = &mut function.body {
                self.typecheck_block(block);
            }
        }
    }

    fn typecheck_var_declaration(&mut self, var_decl: &mut VarDecl) {
        self.symbols
            .insert(var_decl.name.clone(), (Type::Int, true));

        if let Some(init) = &mut var_decl.init {
            self.typecheck_expression(init);
        };
    }

    fn typecheck_expression(&mut self, expr: &mut Expression) {
        match expr {
            Expression::FunctionCall { name, args } => {
                let (func_type, _) = self.symbols.get(name).unwrap();
                match *func_type {
                    Type::Int => {
                        panic!("typecheck_expression Variable used as function name!");
                    }
                    Type::FunType { param_count } => {
                        if param_count != args.len() as u32 {
                            panic!("typecheck_expression Incorrect argument count!");
                        }

                        for arg in args {
                            self.typecheck_expression(arg);
                        }
                    }
                }
            }
            Expression::Var(name) => {
                let (func_type, _) = self.symbols.get(name).unwrap();
                if *func_type != Type::Int {
                    panic!("typecheck_expression Function name used as variable!");
                }
            }
            Expression::UnaryExpr(_, expr) => self.typecheck_expression(expr),
            Expression::BinaryExp(left, _, right) => {
                self.typecheck_expression(left);
                self.typecheck_expression(right);
            }
            Expression::Assignment(left, right) => {
                self.typecheck_expression(left);
                self.typecheck_expression(right);
            }
            Expression::Conditional {
                condition,
                exp1,
                exp2,
            } => {
                self.typecheck_expression(condition);
                self.typecheck_expression(exp1);
                self.typecheck_expression(exp2);
            }
            _ => {}
        }
    }

    fn typecheck_block(&mut self, block: &mut Block) {
        for item in &mut block.block_items {
            self.typecheck_block_item(item);
        }
    }

    fn typecheck_block_item(&mut self, item: &mut BlockItem) {
        match item {
            BlockItem::Declaration(decl) => self.typecheck_declaration(decl),
            BlockItem::Statement(stmt) => self.typecheck_statement(stmt),
        }
    }

    fn typecheck_statement(&mut self, stmt: &mut Statement) {
        match stmt {
            Statement::Return(expr) => self.typecheck_expression(expr),
            Statement::Expression(expr) => self.typecheck_expression(expr),
            Statement::If {
                exp,
                then,
                else_statement,
            } => {
                self.typecheck_expression(exp);
                self.typecheck_statement(then);
                if let Some(else_stmt) = else_statement {
                    self.typecheck_statement(else_stmt);
                }
            }
            Statement::Compound(block) => self.typecheck_block(block),
            Statement::While {
                condition, body, ..
            } => {
                self.typecheck_expression(condition);
                self.typecheck_statement(body);
            }
            Statement::DoWhile {
                body, condition, ..
            } => {
                self.typecheck_statement(body);
                self.typecheck_expression(condition);
            }
            Statement::For {
                init,
                condition,
                post,
                body,
                ..
            } => {
                match init {
                    ForInit::InitDeclaration(decl) => self.typecheck_var_declaration(decl),
                    ForInit::InitExpression(Some(expr)) => self.typecheck_expression(expr),
                    _ => {}
                }
                if let Some(cond) = condition {
                    self.typecheck_expression(cond);
                }
                if let Some(p) = post {
                    self.typecheck_expression(p);
                }
                self.typecheck_statement(body);
            }
            _ => {}
        }
    }
}
