use std::collections::HashMap;

use crate::parser::{AST, BlockItem, Declaration, Expression, Function, Statement};

pub struct SemanticValidation {
    variable_map: HashMap<String, String>,
    pub var_counter: usize,
}
impl SemanticValidation {
    pub fn new() -> Self {
        SemanticValidation {
            variable_map: HashMap::new(),
            var_counter: 0,
        }
    }

    // Copied from TAC, move to some shared file?
    fn new_temp_var(&mut self) -> String {
        let tmp = format!("tmp.{}", self.var_counter);
        self.var_counter += 1;
        tmp.to_string()
    }

    pub fn validate(&mut self, ast: &mut AST) {
        match ast {
            AST::Program(function) => self.validate_function(function),
        }
    }

    fn validate_function(&mut self, function: &mut Function) {
        for block_item in &mut function.body {
            self.validate_block_item(block_item);
        }
    }

    fn validate_block_item(&mut self, block_item: &mut BlockItem) {
        match block_item {
            BlockItem::Declaration(declaration) => {
                self.resolve_declaration(declaration);
            }
            BlockItem::Statement(statement) => {
                self.resolve_statement(statement);
            }
        }
    }

    fn resolve_declaration(&mut self, declaration: &mut Declaration) {
        if self.variable_map.contains_key(&declaration.name) {
            panic!(
                "<resolve_declaration> Duplicate name found: {}",
                declaration.name
            );
        }
        let temp_name = self.new_temp_var();
        self.variable_map
            .insert(declaration.name.clone(), temp_name.clone());

        // Updates Declaration Node
        declaration.name = temp_name;

        if let Some(init) = &mut declaration.init {
            self.resolve_expression(init);
        }
    }

    fn resolve_expression(&mut self, expr: &mut Expression) {
        match expr {
            Expression::Var(name) => {
                if let Some(new_name) = self.variable_map.get(name) {
                    *name = new_name.clone();
                } else {
                    panic!(
                        "<resolve_expression> Undeclared variable name found: {}",
                        name
                    );
                }
            }
            Expression::Assignment(left, right) => {
                if !matches!(**left, Expression::Var(_)) {
                    panic!("Invalid lvalue");
                }
                self.resolve_expression(left);
                self.resolve_expression(right);
            }
            Expression::Constant(_) => {}
            Expression::UnaryExpr(_, inner) => self.resolve_expression(inner),
            Expression::BinaryExp(left, _, right) => {
                self.resolve_expression(left);
                self.resolve_expression(right);
            }
        }
    }

    fn resolve_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Expression(expr) => self.resolve_expression(expr),
            Statement::Return(expr) => self.resolve_expression(expr),
            Statement::Null => {}
        }
    }
}
