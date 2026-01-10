#[derive(Debug)]
pub enum AST {
    Program(Vec<Declaration>),
}

#[derive(Debug)]
pub struct FunDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Option<Block>,
    pub storage_class: Option<StorageClass>,
    pub func_type: Type,
}

#[derive(Debug)]
pub struct VarDecl {
    pub name: String,
    pub init: Option<TypedExpression>,
    pub storage_class: Option<StorageClass>,
    pub var_type: Type,
}

#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug)]
pub enum Statement {
    Return(TypedExpression),
    Expression(TypedExpression),
    If {
        exp: TypedExpression,
        then: Box<Statement>,
        else_statement: Option<Box<Statement>>,
    },
    Compound(Block),
    Break {
        label: String,
    },
    Continue {
        label: String,
    },
    While {
        condition: TypedExpression,
        body: Box<Statement>,
        label: String,
    },
    DoWhile {
        body: Box<Statement>,
        condition: TypedExpression,
        label: String,
    },
    For {
        init: ForInit,
        condition: Option<Box<TypedExpression>>,
        post: Option<Box<TypedExpression>>,
        body: Box<Statement>,
        label: String,
    },
    Null,
}

#[derive(Debug)]
pub enum ForInit {
    InitDeclaration(VarDecl),
    InitExpression(Option<Box<TypedExpression>>),
}

#[derive(Debug)]
pub enum Declaration {
    VarDecl(VarDecl),
    FunDecl(FunDecl),
}

#[derive(Debug)]
pub struct TypedExpression {
    pub expr: Expression,
    pub etype: Option<Type>,
}

#[derive(Debug)]
pub enum Expression {
    Constant(Const),
    Var(String),
    UnaryExpr(UnaryOperator, Box<TypedExpression>),
    BinaryExp(Box<TypedExpression>, BinaryOperator, Box<TypedExpression>),
    Assignment(Box<TypedExpression>, Box<TypedExpression>),
    Conditional {
        condition: Box<TypedExpression>,
        exp1: Box<TypedExpression>,
        exp2: Box<TypedExpression>,
    },
    FunctionCall {
        name: String,
        args: Vec<TypedExpression>,
    },
    Cast {
        target_type: Type,
        exp: Box<TypedExpression>,
    },
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Assignment,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    And,
    Or,
    EqualTo,
    NotEqualTo,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    QuestionMark,
    Colon,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,  // 32b
    Long, // 64 bit
    FuncType { params: Vec<Type>, ret: Box<Type> },
}

#[derive(Debug, Clone, Copy)]
pub enum Const {
    ConstInt(i32),
    ConstLong(i64),
}
