use core::panic;

use crate::lexer::{Token, TokenType};

#[derive(Debug)]
pub enum AST {
    Program(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: String, // identifier
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
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
    Null,
}

#[derive(Debug)]
pub struct Declaration {
    pub name: String,
    pub init: Option<Expression>,
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

pub struct Parser {
    pub tokens: Vec<Token>,
    current_positon: usize,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens,
            current_positon: 0,
        }
    }

    pub fn parse(&mut self) -> Result<AST, String> {
        self.parse_program()
    }
    pub fn parse_program(&mut self) -> Result<AST, String> {
        let func = self.parse_function();

        if (self.current_positon != self.tokens.len() - 1)
            || (self.tokens[self.current_positon].token_type != TokenType::EOF)
        {
            return Err(format!("Syntax Error: Unexpected token at the end"));
        }
        println!(
            "Current token is {}, total: {}",
            self.current_positon,
            self.tokens.len()
        );
        Ok(AST::Program(func))
    }

    pub fn parse_function(&mut self) -> Function {
        self.expect_token(TokenType::KeywordInt);

        let name = match self.consume_token() {
            Some(token) => match token.token_type {
                TokenType::Identifier(name) => name,
                _ => panic!("Expected function name"),
            },
            None => panic!("Unexpected EOF while parsing function name"),
        };

        self.expect_token(TokenType::OpenParen);
        self.expect_token(TokenType::KeywordVoid);
        self.expect_token(TokenType::CloseParen);

        let blocks = self.parse_block();

        Function { name, body: blocks }
    }

    pub fn parse_block(&mut self) -> Block {
        self.expect_token(TokenType::OpenBrace);

        let mut block_items: Vec<BlockItem> = Vec::new();

        while TokenType::CloseBrace != self.peek_token().unwrap().token_type {
            let block_item = self.parse_block_item();
            block_items.push(block_item);
        }
        self.consume_token();

        Block { block_items }
    }

    pub fn parse_block_item(&mut self) -> BlockItem {
        match self.peek_token().map(|t| t.token_type) {
            Some(TokenType::KeywordInt) => {
                self.consume_token();
                let name = match self.consume_token() {
                    Some(token) => match token.token_type {
                        TokenType::Identifier(name) => name,
                        _ => panic!("Expected identifier after int keyword!"),
                    },
                    None => panic!("Unexpected end of tokens!"),
                };

                let init =
                    if let Some(TokenType::Assignment) = self.peek_token().map(|t| t.token_type) {
                        self.consume_token();
                        Some(self.parse_expression(0))
                    } else {
                        None
                    };

                self.expect_token(TokenType::Semicolon);
                BlockItem::Declaration(Declaration { name, init })
            }
            _ => BlockItem::Statement(self.parse_statement()),
        }
    }

    pub fn parse_statement(&mut self) -> Statement {
        match self.peek_token().map(|t| t.token_type) {
            Some(TokenType::KeywordReturn) => {
                self.consume_token();
                let expr = self.parse_expression(0);
                self.expect_token(TokenType::Semicolon);
                Statement::Return(expr)
            }
            Some(TokenType::Semicolon) => {
                self.consume_token();
                Statement::Null
            }
            Some(TokenType::KeywordIf) => {
                self.consume_token();
                self.expect_token(TokenType::OpenParen);
                let if_condition = self.parse_expression(0);
                self.expect_token(TokenType::CloseParen);

                let if_statement = self.parse_statement();
                // Optional else
                let else_statement =
                    if let Some(TokenType::KeywordElse) = self.peek_token().map(|t| t.token_type) {
                        self.consume_token();
                        Some(Box::new(self.parse_statement()))
                    } else {
                        None
                    };

                Statement::If {
                    exp: if_condition,
                    then: Box::new(if_statement),
                    else_statement,
                }
            }
            Some(TokenType::OpenBrace) => {
                let block = self.parse_block();

                Statement::Compound(block)
            }
            _ => {
                let expr = self.parse_expression(0);
                self.expect_token(TokenType::Semicolon);
                Statement::Expression(expr)
            }
        }
    }

    pub fn parse_factor(&mut self) -> Expression {
        match self.consume_token() {
            Some(token) => match token.token_type {
                TokenType::Integer(val) => Expression::Constant(val),
                TokenType::Hyphen => {
                    Expression::UnaryExpr(UnaryOperator::Negate, Box::new(self.parse_factor()))
                }
                TokenType::Tilde => {
                    Expression::UnaryExpr(UnaryOperator::Complement, Box::new(self.parse_factor()))
                }
                TokenType::Not => {
                    Expression::UnaryExpr(UnaryOperator::Not, Box::new(self.parse_factor()))
                }
                TokenType::OpenParen => {
                    let inner_expr = self.parse_expression(0);
                    self.expect_token(TokenType::CloseParen);
                    inner_expr
                }
                TokenType::Identifier(name) => Expression::Var(name),

                _ => panic!("Expected factor got {:?}", token),
            },
            None => panic!("Unexpected EOF while parsing factor"),
        }
    }

    pub fn parse_expression(&mut self, min_prec: u32) -> Expression {
        let mut left = self.parse_factor();
        while let Some(next_token) = self.peek_token() {
            let precedence: u32 = match next_token.token_type {
                TokenType::Asterisk | TokenType::ForwardSlash | TokenType::Percent => 50,
                TokenType::Plus | TokenType::Hyphen => 45,
                TokenType::LessThan
                | TokenType::LessOrEqual
                | TokenType::GreaterThan
                | TokenType::GreaterOrEqual => 35,
                TokenType::EqualTo | TokenType::NotEqualTo => 30,
                TokenType::And => 10,
                TokenType::Or => 5,
                TokenType::QuestionMark => 3,
                TokenType::Assignment => 1,
                _ => break,
            };
            if precedence < min_prec {
                break;
            }
            self.consume_token();
            if next_token.token_type == TokenType::Assignment {
                let right = self.parse_expression(precedence);
                left = Expression::Assignment(Box::new(left), Box::new(right));
            } else if next_token.token_type == TokenType::QuestionMark {
                // Parse middle expression of ? <exp> : <exp> until :
                let middle = self.parse_expression(0);
                self.expect_token(TokenType::Colon);

                // right is after :
                let right = self.parse_expression(precedence);

                // left = Expression::Assignment(Box::new(left), Box::new(right));
                left = Expression::Conditional {
                    condition: Box::new(left),
                    exp1: Box::new(middle),
                    exp2: Box::new(right),
                };
            } else {
                let op: BinaryOperator = match next_token.token_type {
                    TokenType::Plus => BinaryOperator::Addition,
                    TokenType::Hyphen => BinaryOperator::Subtraction,
                    TokenType::Asterisk => BinaryOperator::Multiplication,
                    TokenType::ForwardSlash => BinaryOperator::Division,
                    TokenType::Percent => BinaryOperator::Modulo,
                    TokenType::And => BinaryOperator::And,
                    TokenType::Or => BinaryOperator::Or,
                    TokenType::EqualTo => BinaryOperator::EqualTo,
                    TokenType::NotEqualTo => BinaryOperator::NotEqualTo,
                    TokenType::LessThan => BinaryOperator::LessThan,
                    TokenType::LessOrEqual => BinaryOperator::LessOrEqual,
                    TokenType::GreaterThan => BinaryOperator::GreaterThan,
                    TokenType::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
                    _ => break,
                };

                let right = self.parse_expression(precedence + 1);
                left = Expression::BinaryExp(Box::new(left), op, Box::new(right));
            }
        }

        left
    }

    pub fn expect_token(&mut self, expected: TokenType) {
        match self.consume_token() {
            Some(token) => {
                if std::mem::discriminant(&token.token_type) != std::mem::discriminant(&expected) {
                    panic!("Expected {:?} but found {:?}", expected, token.token_type);
                }
            }
            None => panic!("Expected {:?}, but found EOF", expected),
        }
    }

    pub fn peek_token(&mut self) -> Option<Token> {
        if self.current_positon >= self.tokens.len() {
            return None;
        }

        let token = self.tokens[self.current_positon].clone();
        Some(token)
    }

    pub fn consume_token(&mut self) -> Option<Token> {
        if self.current_positon >= self.tokens.len() {
            return None;
        }
        let token = self.tokens[self.current_positon].clone();
        self.current_positon += 1;
        Some(token)
    }
}
