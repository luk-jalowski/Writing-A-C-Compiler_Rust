use core::panic;
use std::collections::HashMap;

use crate::ast::{
    AST, BinaryOperator, Block, BlockItem, Const, Declaration, Expression, ForInit, FunDecl,
    Statement, StorageClass, Type, TypedExpression, UnaryOperator, VarDecl,
};

#[derive(Debug)]
pub struct TypeValidation {
    pub symbols: HashMap<String, (Type, IdentifierAttribs)>,
    current_scope_is_global: bool,
    current_func_ret_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum IdentifierAttribs {
    FunAttr { defined: bool, global: bool },
    StaticAttr { init: InitialValue, global: bool },
}

#[derive(Debug, Clone)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInit),
    NoInitializer,
}

#[derive(Debug, Clone)]
pub enum StaticInit {
    IntInit(i32),
    UIntInit(u32),
    LongInit(i64),
    ULongInit(u64),
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
            current_func_ret_type: None,
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
            let (prev_params, _) = match prev_decl {
                Type::FuncType { params, ret } => (params, ret),
                _ => {
                    panic!(
                        "typecheck_function_declaration Expected function type {:?}",
                        prev_decl
                    );
                }
            };
            match prev_attr {
                IdentifierAttribs::FunAttr { defined, global } => {
                    if prev_params.len() != function.params.len() {
                        panic!(
                            "typecheck_function_declaration Found duplicate function {} with different argument number {}, previous count {}",
                            function.name,
                            function.params.len(),
                            prev_params.len()
                        );
                    }
                    if prev_decl != &function.func_type {
                        panic!(
                            "typecheck_function_declaration Found duplicate function {} with different signature",
                            function.name
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
                    panic!(
                        "typecheck_function_declaration Expected function attributes {:?}",
                        prev_attr
                    );
                }
            };
        }

        self.symbols.insert(
            function.name.clone(),
            (
                function.func_type.clone(),
                IdentifierAttribs::FunAttr {
                    defined: is_defined,
                    global: is_global,
                },
            ),
        );

        if has_body {
            let param_types = match &function.func_type {
                Type::FuncType { params, .. } => params,
                _ => panic!(
                    "typecheck_function_declaration Function has non-function type {:?}",
                    function.func_type
                ),
            };

            for (param, param_type) in function.params.iter_mut().zip(param_types.iter()) {
                self.symbols.insert(
                    param.clone(),
                    (
                        param_type.clone(),
                        IdentifierAttribs::FunAttr {
                            defined: true,
                            global: is_global,
                        },
                    ),
                );
            }
            if let Some(block) = &mut function.body {
                if let Type::FuncType { ret, .. } = &function.func_type {
                    self.current_func_ret_type = Some(*ret.clone());
                }
                self.current_scope_is_global = false;
                self.typecheck_block(block);
                self.current_scope_is_global = true;
                self.current_func_ret_type = None;
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
                match &init.expr {
                    Expression::Constant(val) => match val {
                        Const::ConstInt(i) => match var_decl.var_type {
                            Type::Long => InitialValue::Initial(StaticInit::LongInit(*i as i64)),
                            _ => InitialValue::Initial(StaticInit::IntInit(*i)),
                        },
                        Const::ConstUInt(ui) => match var_decl.var_type {
                            //TODO fix this
                            Type::Long => InitialValue::Initial(StaticInit::ULongInit(*ui as u64)),
                            _ => InitialValue::Initial(StaticInit::UIntInit(*ui)),
                        },
                        Const::ConstLong(l) => match var_decl.var_type {
                            Type::Int => InitialValue::Initial(StaticInit::IntInit(*l as i32)),
                            _ => InitialValue::Initial(StaticInit::LongInit(*l)),
                        },
                        Const::ConstULong(ul) => match var_decl.var_type {
                            //TODO fix this
                            Type::Int => InitialValue::Initial(StaticInit::UIntInit(*ul as u32)),
                            _ => InitialValue::Initial(StaticInit::ULongInit(*ul)),
                        },
                    },
                    _ => InitialValue::Initial(StaticInit::IntInit(0)),
                }
            } else {
                InitialValue::NoInitializer
            }
        } else if is_extern {
            InitialValue::NoInitializer
        } else if is_global_scope {
            InitialValue::Tentative
        } else if is_static {
            InitialValue::Initial(StaticInit::IntInit(0))
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
            if *prev_type != var_decl.var_type {
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
            } else if is_global_scope && prev_has_external_linkage != has_external_linkage {
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
                && matches!(init_val, InitialValue::NoInitializer)
            {
                init_val = prev_init.clone();
            }
        }

        self.symbols.insert(
            var_decl.name.clone(),
            (
                var_decl.var_type.clone(),
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
            let init_type = init.etype.as_ref().unwrap();
            if init_type != &var_decl.var_type {
                let old_expr =
                    std::mem::replace(&mut init.expr, Expression::Constant(Const::ConstInt(0)));
                let old_typed_expr = TypedExpression {
                    expr: old_expr,
                    etype: Some(init_type.clone()),
                };
                init.expr = Expression::Cast {
                    target_type: var_decl.var_type.clone(),
                    exp: Box::new(old_typed_expr),
                };
                init.etype = Some(var_decl.var_type.clone());
            }
            if (is_global_scope || is_static) && !self.is_constant_expression(init) {
                panic!("Initializer element is not constant");
            }
        };

        if is_static && !is_global_scope {
            var_decl.init = None;
        }
    }

    fn is_constant_expression(&self, expr: &TypedExpression) -> bool {
        match &expr.expr {
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
            Expression::Cast { exp, .. } => self.is_constant_expression(exp),
            _ => false,
        }
    }

    fn typecheck_expression(&mut self, typed_expr: &mut TypedExpression) {
        match &mut typed_expr.expr {
            Expression::Constant(c) => {
                typed_expr.etype = Some(match c {
                    Const::ConstInt(_) => Type::Int,
                    Const::ConstUInt(_) => Type::UInt,
                    Const::ConstLong(_) => Type::Long,
                    Const::ConstULong(_) => Type::ULong,
                });
            }
            Expression::FunctionCall { name, args } => {
                let (func_type, _) = self.symbols.get(name).unwrap().clone();
                match func_type {
                    Type::FuncType { params, ret } => {
                        if params.len() != args.len() {
                            panic!("typecheck_expression Incorrect argument count!");
                        }

                        for (arg, param_type) in args.iter_mut().zip(params.iter()) {
                            self.typecheck_expression(arg);
                            let arg_type = arg.etype.as_ref().unwrap();
                            if arg_type != param_type {
                                let old_expr = std::mem::replace(
                                    &mut arg.expr,
                                    Expression::Constant(Const::ConstInt(0)),
                                );
                                let old_typed_expr = TypedExpression {
                                    expr: old_expr,
                                    etype: Some(arg_type.clone()),
                                };
                                arg.expr = Expression::Cast {
                                    target_type: param_type.clone(),
                                    exp: Box::new(old_typed_expr),
                                };
                                arg.etype = Some(param_type.clone());
                            }
                        }
                        typed_expr.etype = Some(*ret);
                    }
                    _ => panic!("typecheck_expression Variable used as function name!"),
                }
            }
            Expression::Var(name) => {
                let (var_type, _) = self.symbols.get(name).unwrap();

                if matches!(var_type, Type::FuncType { .. }) {
                    panic!(
                        "typecheck_expression Function name used as variable! {}",
                        name
                    );
                }
                typed_expr.etype = Some(var_type.clone());
            }
            Expression::UnaryExpr(op, expr) => {
                self.typecheck_expression(expr);
                let inner_type = expr.etype.as_ref().unwrap();
                match op {
                    UnaryOperator::Not => typed_expr.etype = Some(Type::Int),
                    _ => typed_expr.etype = Some(inner_type.clone()),
                }
            }
            Expression::BinaryExp(left, op, right) => {
                self.typecheck_expression(left);
                self.typecheck_expression(right);
                let left_type = left.etype.as_ref().unwrap();
                let right_type = right.etype.as_ref().unwrap();

                if matches!(op, BinaryOperator::And | BinaryOperator::Or) {
                    typed_expr.etype = Some(Type::Int);
                    return;
                }

                let common_type = self.get_common_type(left_type, right_type);

                if *left_type != common_type {
                    let old_expr =
                        std::mem::replace(&mut left.expr, Expression::Constant(Const::ConstInt(0)));
                    let old_typed_expr = TypedExpression {
                        expr: old_expr,
                        etype: Some(left_type.clone()),
                    };
                    left.expr = Expression::Cast {
                        target_type: common_type.clone(),
                        exp: Box::new(old_typed_expr),
                    };
                    left.etype = Some(common_type.clone());
                }

                if *right_type != common_type {
                    let old_expr = std::mem::replace(
                        &mut right.expr,
                        Expression::Constant(Const::ConstInt(0)),
                    );
                    let old_typed_expr = TypedExpression {
                        expr: old_expr,
                        etype: Some(right_type.clone()),
                    };
                    right.expr = Expression::Cast {
                        target_type: common_type.clone(),
                        exp: Box::new(old_typed_expr),
                    };
                    right.etype = Some(common_type.clone());
                }

                match op {
                    BinaryOperator::EqualTo
                    | BinaryOperator::NotEqualTo
                    | BinaryOperator::LessThan
                    | BinaryOperator::LessOrEqual
                    | BinaryOperator::GreaterThan
                    | BinaryOperator::GreaterOrEqual => {
                        typed_expr.etype = Some(Type::Int);
                    }
                    _ => {
                        typed_expr.etype = Some(common_type);
                    }
                }
            }
            Expression::Assignment(left, right) => {
                self.typecheck_expression(left);
                self.typecheck_expression(right);
                let left_type = left.etype.as_ref().unwrap();
                let right_type = right.etype.as_ref().unwrap();
                if left_type != right_type {
                    let old_expr = std::mem::replace(
                        &mut right.expr,
                        Expression::Constant(Const::ConstInt(0)),
                    );
                    let old_typed_expr = TypedExpression {
                        expr: old_expr,
                        etype: Some(right_type.clone()),
                    };
                    right.expr = Expression::Cast {
                        target_type: left_type.clone(),
                        exp: Box::new(old_typed_expr),
                    };
                    right.etype = Some(left_type.clone());
                }
                typed_expr.etype = left.etype.clone();
            }
            Expression::Conditional {
                condition,
                exp1,
                exp2,
            } => {
                self.typecheck_expression(condition);
                self.typecheck_expression(exp1);
                self.typecheck_expression(exp2);
                let t1 = exp1.etype.as_ref().unwrap();
                let t2 = exp2.etype.as_ref().unwrap();

                let common_type = self.get_common_type(t1, t2);

                if *t1 != common_type {
                    let old_expr =
                        std::mem::replace(&mut exp1.expr, Expression::Constant(Const::ConstInt(0)));
                    let old_typed_expr = TypedExpression {
                        expr: old_expr,
                        etype: Some(t1.clone()),
                    };
                    exp1.expr = Expression::Cast {
                        target_type: common_type.clone(),
                        exp: Box::new(old_typed_expr),
                    };
                    exp1.etype = Some(common_type.clone());
                }

                if *t2 != common_type {
                    let old_expr =
                        std::mem::replace(&mut exp2.expr, Expression::Constant(Const::ConstInt(0)));
                    let old_typed_expr = TypedExpression {
                        expr: old_expr,
                        etype: Some(t2.clone()),
                    };
                    exp2.expr = Expression::Cast {
                        target_type: common_type.clone(),
                        exp: Box::new(old_typed_expr),
                    };
                    exp2.etype = Some(common_type.clone());
                }
                typed_expr.etype = Some(common_type);
            }
            Expression::Cast { target_type, exp } => {
                self.typecheck_expression(exp);
                typed_expr.etype = Some(target_type.clone());
            }
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
            Statement::Return(expr) => {
                self.typecheck_expression(expr);
                if let Some(ret_type) = &self.current_func_ret_type {
                    let expr_type = expr.etype.as_ref().unwrap();
                    if expr_type != ret_type {
                        let old_expr = std::mem::replace(
                            &mut expr.expr,
                            Expression::Constant(Const::ConstInt(0)),
                        );
                        let old_typed_expr = TypedExpression {
                            expr: old_expr,
                            etype: Some(expr_type.clone()),
                        };
                        expr.expr = Expression::Cast {
                            target_type: ret_type.clone(),
                            exp: Box::new(old_typed_expr),
                        };
                        expr.etype = Some(ret_type.clone());
                    }
                }
            }
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

    fn get_common_type(&mut self, type1: &Type, type2: &Type) -> Type {
        if *type1 == *type2 {
            return type1.clone();
        }

        if self.get_type_size(type1) == self.get_type_size(type2) {
            if *type1 == Type::Int || *type1 == Type::Long {
                return type2.clone();
            } else {
                return type1.clone();
            }
        }
        if self.get_type_size(&type1) > self.get_type_size(&type2) {
            return type1.clone();
        } else {
            return type2.clone();
        }
    }

    fn get_type_size(&mut self, type1: &Type) -> u32 {
        let size = match type1 {
            Type::Int | Type::UInt => 4,
            Type::Long | Type::ULong => 8,
            _ => 4,
        };
        size
    }
}
