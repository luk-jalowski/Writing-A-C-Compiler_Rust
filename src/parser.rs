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
        self.expect(TokenType::KeywordInt);

        let name = match self.consume_token() {
            Some(token) => match token.token_type {
                TokenType::Identifier(name) => name,
                _ => panic!("Expected function name"),
            },
            None => panic!("Unexpected EOF while parsing function name"),
        };

        self.expect(TokenType::OpenParen);
        self.expect(TokenType::KeywordVoid);
        self.expect(TokenType::CloseParen);

        self.expect(TokenType::OpenBrace);

        let body = self.parse_statement();

        self.expect(TokenType::CloseBrace);

        Function { name, body }
    }

    pub fn parse_statement(&mut self) -> Statement {
        self.expect(TokenType::KeywordReturn);
        let expr = self.parse_expression();
        self.expect(TokenType::Semicolon);

        Statement::Return(expr)
    }

    pub fn parse_expression(&mut self) -> Expression {
        match self.consume_token() {
            Some(token) => match token.token_type {
                TokenType::Integer(val) => Expression::Constant(val),
                _ => panic!("Expected integer"),
            },
            None => panic!("Unexpected EOF while parsing expression"),
        }
    }

    pub fn expect(&mut self, expected: TokenType) {
        match self.consume_token() {
            Some(token) => {
                if std::mem::discriminant(&token.token_type) != std::mem::discriminant(&expected) {
                    panic!("Expected {:?} but found {:?}", expected, token.token_type);
                }
            }
            None => panic!("Expected {:?}, but found EOF", expected),
        }
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
