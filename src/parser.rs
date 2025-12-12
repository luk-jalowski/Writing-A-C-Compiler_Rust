use crate::lexer::{Token, TokenType};

#[derive(Debug)]
pub enum AST {
    Program(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: String, // identifier
    pub body: Statement,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Constant(i32),
    UnaryExpr(UnaryOperator, Box<Expression>),
    BinaryExp(Box<Expression>, BinaryOperator, Box<Expression>),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
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

        self.expect_token(TokenType::OpenBrace);

        let body = self.parse_statement();

        self.expect_token(TokenType::CloseBrace);

        Function { name, body }
    }

    pub fn parse_statement(&mut self) -> Statement {
        self.expect_token(TokenType::KeywordReturn);
        let expr = self.parse_expression(0);
        self.expect_token(TokenType::Semicolon);

        Statement::Return(expr)
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
                TokenType::OpenParen => {
                    let inner_expr = self.parse_expression(0);
                    self.expect_token(TokenType::CloseParen);
                    inner_expr
                }

                _ => panic!("Expected factor got {:?}", token),
            },
            None => panic!("Unexpected EOF while parsing factor"),
        }
    }

    pub fn parse_expression(&mut self, min_prec: u32) -> Expression {
        let mut left = self.parse_factor();
        while let Some(next_token) = self.peek_token() {
            let op = match next_token.token_type {
                TokenType::Plus => BinaryOperator::Addition,
                TokenType::Hyphen => BinaryOperator::Subtraction,
                TokenType::Asterisk => BinaryOperator::Multiplication,
                TokenType::ForwardSlash => BinaryOperator::Division,
                TokenType::Percent => BinaryOperator::Modulo,
                _ => break,
            };
            let precedence: u32 = match next_token.token_type {
                TokenType::Plus | TokenType::Hyphen => 45,
                TokenType::Asterisk | TokenType::ForwardSlash | TokenType::Percent => 50,
                _ => break,
            };
            if precedence < min_prec {
                break;
            }
            self.consume_token();
            let right = self.parse_expression(precedence + 1);
            left = Expression::BinaryExp(Box::new(left), op, Box::new(right));
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
