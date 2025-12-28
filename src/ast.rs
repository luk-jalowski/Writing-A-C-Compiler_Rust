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
}

#[derive(Debug)]
pub struct VarDecl {
    pub name: String,
    pub init: Option<Expression>,
    pub storage_class: Option<StorageClass>,
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
    Return(Expression),
    Expression(Expression),
    If {
        exp: Expression,
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
        condition: Expression,
        body: Box<Statement>,
        label: String,
    },
    DoWhile {
        body: Box<Statement>,
        condition: Expression,
        label: String,
    },
    For {
        init: ForInit,
        condition: Option<Box<Expression>>,
        post: Option<Box<Expression>>,
        body: Box<Statement>,
        label: String,
    },
    Null,
}

#[derive(Debug)]
pub enum ForInit {
    InitDeclaration(VarDecl),
    InitExpression(Option<Box<Expression>>),
}

#[derive(Debug)]
pub enum Declaration {
    VarDecl(VarDecl),
    FunDecl(FunDecl),
}

#[derive(Debug)]
pub enum Expression {
    Constant(i32),
    Var(String),
    UnaryExpr(UnaryOperator, Box<Expression>),
    BinaryExp(Box<Expression>, BinaryOperator, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    Conditional {
        condition: Box<Expression>,
        exp1: Box<Expression>,
        exp2: Box<Expression>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expression>,
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

#[derive(Debug, Clone, Copy)]
pub enum Types {
    Integer32,
}
