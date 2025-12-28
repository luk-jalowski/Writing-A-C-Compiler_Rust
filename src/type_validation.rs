use core::panic;
use std::collections::HashMap;

use crate::ast::{
    AST, Block, BlockItem, Declaration, Expression, ForInit, FunDecl, Statement, StorageClass,
    VarDecl,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    FunType { param_count: u32 },
}

#[derive(Debug)]
pub struct TypeValidation {
    pub symbols: HashMap<String, (Type, IdentifierAttribs)>,
    current_scope_is_global: bool,
}

#[derive(Debug, Clone)]
pub enum IdentifierAttribs {
    FunAttr { defined: bool, global: bool },
    StaticAttr { init: InitialValue, global: bool },
}

#[derive(Debug, Clone)]
pub enum InitialValue {
    Tentative,
    Initial { value: u32 },
    NoInitializer,
}

impl Default for TypeValidation {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeValidation {
    pub fn new() -> Self {
        TypeValidation {
            symbols: HashMap::new(),
            current_scope_is_global: true,
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
        let has_body = function.body.is_some();
        let mut is_defined = has_body;
        let mut is_global = function.storage_class != Some(StorageClass::Static);

        if let Some((prev_decl, prev_attr)) = self.symbols.get(&function.name) {
            let param_count = match prev_decl {
                Type::FunType { param_count } => *param_count,
                _ => {
                    panic!(
                        "typecheck_function_declaration Expected function type {:?}",
                        prev_decl
                    );
                }
            };
            match prev_attr {
                IdentifierAttribs::FunAttr { defined, global } => {
                    if param_count != function.params.len() as u32 {
                        panic!(
                            "typecheck_function_declaration Found duplicate function {} with different argument number {}, previous count {}",
                            function.name,
                            function.params.len(),
                            param_count
                        );
                    }
                    if *defined && has_body {
                        panic!(
                            "typecheck_function_declaration Found duplicate function {} that has already been defined",
                            function.name,
                        );
                    }

                    if *defined {
                        is_defined = true;
                    }

                    if *global && function.storage_class == Some(StorageClass::Static) {
                        panic!(
                            "Static function declaration follows non-static - function name: {}",
                            function.name
                        );
                    }
                    is_global = *global;
                }
                _ => {
                    panic!("Expected function attributes {:?}", prev_attr);
                }
            };
        }

        self.symbols.insert(
            function.name.clone(),
            (
                Type::FunType {
                    param_count: function.params.len() as u32,
                },
                IdentifierAttribs::FunAttr {
                    defined: is_defined,
                    global: is_global,
                },
            ),
        );

        if has_body {
            for param in &mut function.params {
                self.symbols.insert(
                    param.clone(),
                    (
                        Type::Int,
                        IdentifierAttribs::FunAttr {
                            defined: true,
                            global: is_global,
                        },
                    ),
                );
            }
            if let Some(block) = &mut function.body {
                self.current_scope_is_global = false;
                self.typecheck_block(block);
                self.current_scope_is_global = true;
            }
        }
    }

    fn typecheck_var_declaration(&mut self, var_decl: &mut VarDecl) {
        let is_global_scope = self.current_scope_is_global;
        let is_extern = var_decl.storage_class == Some(StorageClass::Extern);
        let is_static = var_decl.storage_class == Some(StorageClass::Static);

        // Determine initialization status
        let mut init_val = if let Some(init) = &var_decl.init {
            if is_global_scope || is_static {
                match init {
                    Expression::Constant(val) => InitialValue::Initial { value: *val as u32 },
                    _ => InitialValue::Initial { value: 0 },
                }
            } else {
                InitialValue::NoInitializer
            }
        } else if is_extern {
            InitialValue::NoInitializer
        } else if is_global_scope {
            InitialValue::Tentative
        } else if is_static {
            InitialValue::Initial { value: 0 }
        } else {
            InitialValue::NoInitializer
        };

        let mut has_external_linkage = if is_extern {
            true
        } else if is_global_scope {
            !is_static
        } else {
            false
        };

        // Check for conflicts with existing symbols
        if let Some((prev_type, prev_attr)) = self.symbols.get(&var_decl.name) {
            if *prev_type != Type::Int {
                panic!(
                    "Variable {} redeclared as different kind of symbol",
                    var_decl.name
                );
            }

            let prev_has_external_linkage = match prev_attr {
                IdentifierAttribs::FunAttr { global, .. } => *global,
                IdentifierAttribs::StaticAttr { global, .. } => *global,
            };

            if is_extern {
                if !prev_has_external_linkage {
                    has_external_linkage = false;
                }
            } else if is_global_scope
                && prev_has_external_linkage != has_external_linkage {
                    panic!("Conflicting linkage for {}", var_decl.name);
                }

            if is_global_scope
                && let IdentifierAttribs::StaticAttr {
                    init: prev_init, ..
                } = prev_attr
                {
                    if matches!(init_val, InitialValue::Initial { .. })
                        && matches!(prev_init, InitialValue::Initial { .. })
                    {
                        panic!("Redefinition of global variable {}", var_decl.name);
                    }

                    if matches!(prev_init, InitialValue::Initial { .. })
                        || matches!(init_val, InitialValue::NoInitializer)
                    {
                        return;
                    }
                }

            if let IdentifierAttribs::StaticAttr {
                init: prev_init, ..
            } = prev_attr
                && matches!(init_val, InitialValue::NoInitializer) {
                    init_val = prev_init.clone();
                }
        }

        self.symbols.insert(
            var_decl.name.clone(),
            (
                Type::Int,
                IdentifierAttribs::StaticAttr {
                    init: init_val,
                    global: has_external_linkage,
                },
            ),
        );

        if let Some(init) = &mut var_decl.init {
            if is_extern && !is_global_scope {
                panic!("Extern variable cannot have an initializer in block scope");
            }
            self.typecheck_expression(init);
            if (is_global_scope || is_static)
                && !self.is_constant_expression(init) {
                    panic!("Initializer element is not constant");
                }
        };

        if is_static && !is_global_scope {
            var_decl.init = None;
        }
    }

    fn is_constant_expression(&self, expr: &Expression) -> bool {
        match expr {
            Expression::Constant(_) => true,
            Expression::UnaryExpr(_, inner) => self.is_constant_expression(inner),
            Expression::BinaryExp(left, _, right) => {
                self.is_constant_expression(left) && self.is_constant_expression(right)
            }
            Expression::Conditional {
                condition,
                exp1,
                exp2,
            } => {
                self.is_constant_expression(condition)
                    && self.is_constant_expression(exp1)
                    && self.is_constant_expression(exp2)
            }
            _ => false,
        }
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
