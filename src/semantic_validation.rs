use std::collections::HashMap;

use crate::ast::{
    AST, BlockItem, Declaration, Expression, ForInit, FunDecl, Statement, StorageClass,
    TypedExpression, VarDecl,
};

struct MapEntry {
    tmp_name: String,
    has_linkage: bool,
}

pub struct SemanticValidation {
    scopes: Vec<HashMap<String, MapEntry>>,
    loop_stack: Vec<(String, String)>, // (break_label, continue_label)
    pub var_counter: usize,
    pub label_counter: usize,
}
impl Default for SemanticValidation {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticValidation {
    pub fn new() -> Self {
        SemanticValidation {
            scopes: vec![HashMap::new()],
            loop_stack: Vec::new(),
            var_counter: 0,
            label_counter: 0,
        }
    }

    // Copied from TAC, move to some shared file?
    fn new_temp_var(&mut self) -> String {
        let tmp = format!("tmp.{}", self.var_counter);
        self.var_counter += 1;
        tmp.to_string()
    }

    fn new_loop_label(&mut self) -> String {
        let label = format!("Loop.{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    pub fn validate(&mut self, ast: &mut AST) {
        match ast {
            AST::Program(declarations) => {
                for decl in declarations {
                    self.resolve_declaration(decl);
                }
            }
        }
    }

    fn resolve_function_declaration(&mut self, function: &mut FunDecl) {
        if function.body.is_some() && self.scopes.len() > 1 {
            panic!("Nested function definitions are not permitted");
        }

        if self.scopes.len() > 1 && matches!(function.storage_class, Some(StorageClass::Static)) {
            panic!("Static function declaration in block scope is not allowed");
        }

        if let Some(entry) = self
            .scopes
            .last()
            .expect("Scope is empty")
            .get(&function.name)
            && entry.tmp_name != function.name
        {
            panic!(
                "<resolve_function_declaration> Duplicate function name found: {}",
                function.name
            );
        }
        self.scopes.last_mut().expect("Scope is empty").insert(
            function.name.clone(),
            MapEntry {
                tmp_name: function.name.clone(),
                has_linkage: true,
            },
        );
        self.scopes.push(HashMap::new());

        for param in &mut function.params {
            if self
                .scopes
                .last()
                .expect("Scope is empty")
                .contains_key(param)
            {
                panic!(
                    "<resolve_function_declaration> Duplicate parameter name found: {}",
                    param
                );
            }

            let temp_name = self.new_temp_var();
            self.scopes.last_mut().expect("Scope is empty").insert(
                param.clone(),
                MapEntry {
                    tmp_name: temp_name.clone(),
                    has_linkage: false,
                },
            );

            *param = temp_name;
        }

        if let Some(body) = &mut function.body {
            for block_item in &mut body.block_items {
                self.resolve_block_item(block_item);
            }
        };

        self.scopes.pop();
    }

    fn resolve_block_item(&mut self, block_item: &mut BlockItem) {
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
        match declaration {
            Declaration::VarDecl(var_decl) => self.resolve_var_declaration(var_decl),
            Declaration::FunDecl(func_decl) => self.resolve_function_declaration(func_decl),
        }
    }

    fn resolve_var_declaration(&mut self, var_decl: &mut VarDecl) {
        // let temp_name = self.new_temp_var();
        let is_file_scope = self.scopes.len() == 1;

        if is_file_scope {
            if let Some(entry) = self
                .scopes
                .last()
                .expect("Scope is empty")
                .get(&var_decl.name)
            {
                if entry.tmp_name != var_decl.name {
                    panic!(
                        "<resolve_declaration> Duplicate name found: {}",
                        var_decl.name
                    );
                }
            } else {
                self.scopes.last_mut().expect("Scope is empty").insert(
                    var_decl.name.clone(),
                    MapEntry {
                        tmp_name: var_decl.name.clone(),
                        has_linkage: true,
                    },
                );
            }
        } else {
            let is_extern = matches!(var_decl.storage_class, Some(StorageClass::Extern));
            if let Some(prev_entry) = self.scopes.last().unwrap().get(&var_decl.name)
                && !(prev_entry.has_linkage && is_extern)
            {
                panic!("Conflicting local declarations for {}", var_decl.name);
            }
            if is_extern {
                self.scopes.last_mut().unwrap().insert(
                    var_decl.name.clone(),
                    MapEntry {
                        tmp_name: var_decl.name.clone(),
                        has_linkage: true,
                    },
                );
            } else {
                let tmp_name = self.new_temp_var();
                self.scopes.last_mut().unwrap().insert(
                    var_decl.name.clone(),
                    MapEntry {
                        tmp_name: tmp_name.clone(),
                        has_linkage: false,
                    },
                );
                var_decl.name = tmp_name;
            }
        }

        if let Some(init) = &mut var_decl.init {
            self.resolve_expression(init);
        }
    }

    fn resolve_expression(&mut self, expr: &mut TypedExpression) {
        match &mut expr.expr {
            Expression::Var(name) => {
                // We need rev cause we need last occurence of given name (inner scope)
                match self.scopes.iter().rev().find_map(|scope| scope.get(name)) {
                    Some(entry) => {
                        *name = entry.tmp_name.clone();
                    }
                    _ => {
                        panic!(
                            "<resolve_expression> Undeclared variable name found: {}",
                            name
                        );
                    }
                };
            }
            Expression::Assignment(left, right) => {
                if !matches!(left.expr, Expression::Var(_)) {
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
            Expression::Conditional {
                condition,
                exp1,
                exp2,
            } => {
                self.resolve_expression(condition);
                self.resolve_expression(exp1);
                self.resolve_expression(exp2);
            }
            Expression::FunctionCall { name, args } => {
                if let Some(entry) = self.scopes.iter().rev().find_map(|scope| scope.get(name)) {
                    *name = entry.tmp_name.clone();
                }
                for arg in args {
                    self.resolve_expression(arg);
                }
            }
            Expression::Cast { target_type: _, exp } => {
                self.resolve_expression(exp);
            }
        }
    }

    fn resolve_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Expression(expr) => self.resolve_expression(expr),
            Statement::Return(expr) => self.resolve_expression(expr),
            Statement::Null => {}
            Statement::If {
                exp,
                then,
                else_statement,
            } => {
                self.resolve_expression(exp);
                self.resolve_statement(then);
                if let Some(else_stm) = else_statement {
                    self.resolve_statement(else_stm);
                }
            }
            Statement::Compound(block) => {
                self.scopes.push(HashMap::new());
                for block_item in &mut block.block_items {
                    self.resolve_block_item(block_item);
                }
                self.scopes.pop();
            }
            Statement::Break { label } => {
                let (break_target, _) = self
                    .loop_stack
                    .last()
                    .expect("Break statement outside of loop");
                *label = break_target.clone();
            }
            Statement::Continue { label } => {
                let (_, continue_target) = self
                    .loop_stack
                    .last()
                    .expect("Continue statement outside of loop");
                *label = continue_target.clone();
            }
            Statement::DoWhile {
                body,
                condition,
                label,
            } => {
                let loop_name = self.new_loop_label();
                *label = loop_name.clone(); // update dummy label
                let break_target = format!("break_{}", loop_name);
                let continue_target = format!("continue_{}", loop_name);

                self.loop_stack
                    .push((break_target.clone(), continue_target.clone()));

                self.scopes.push(HashMap::new());
                self.resolve_statement(body);
                self.resolve_expression(condition);
                self.scopes.pop();

                self.loop_stack.pop();
            }
            Statement::For {
                init,
                condition,
                post,
                body,
                label,
            } => {
                let loop_name = self.new_loop_label();
                *label = loop_name.clone(); // update dummy label
                let break_target = format!("break_{}", loop_name);
                let continue_target = format!("continue_{}", loop_name);

                self.loop_stack
                    .push((break_target.clone(), continue_target.clone()));

                self.scopes.push(HashMap::new());
                self.resolve_for_init(init);
                if let Some(cond_expr) = condition {
                    self.resolve_expression(cond_expr);
                }
                if let Some(post_expr) = post {
                    self.resolve_expression(post_expr);
                }
                self.resolve_statement(body);
                self.scopes.pop();

                self.loop_stack.pop();
            }
            Statement::While {
                condition,
                body,
                label,
            } => {
                let loop_name = self.new_loop_label();
                *label = loop_name.clone(); // update dummy label
                let break_target = format!("break_{}", loop_name);
                let continue_target = format!("continue_{}", loop_name);

                self.loop_stack
                    .push((break_target.clone(), continue_target.clone()));

                self.scopes.push(HashMap::new());
                self.resolve_expression(condition);
                self.resolve_statement(body);
                self.scopes.pop();

                self.loop_stack.pop();
            }
        }
    }

    fn resolve_for_init(&mut self, for_init: &mut ForInit) {
        match for_init {
            ForInit::InitDeclaration(var_decl) => {
                if var_decl.storage_class.is_some() {
                    panic!("Variable declared in for loop cannot have storage class");
                }
                self.resolve_var_declaration(var_decl);
            }
            ForInit::InitExpression(expr) => {
                if let Some(init_expr) = expr {
                    self.resolve_expression(init_expr);
                }
            }
        }
    }
}
