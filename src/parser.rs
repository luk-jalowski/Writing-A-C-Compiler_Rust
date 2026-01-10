use core::panic;

use crate::ast::*;
use crate::lexer::{Token, TokenType};

pub struct Parser {
    pub tokens: Vec<Token>,
    current_positon: usize,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current_positon: 0,
        }
    }

    pub fn parse(&mut self) -> Result<AST, String> {
        self.parse_program()
    }
    fn parse_program(&mut self) -> Result<AST, String> {
        let mut declarations = Vec::new();
        while let Some(token) = self.peek_token() {
            if token.token_type == TokenType::EOF {
                break;
            }
            declarations.push(self.parse_declaration());
        }
        Ok(AST::Program(declarations))
    }

    fn parse_block(&mut self) -> Block {
        self.expect_token(TokenType::OpenBrace);

        let mut block_items: Vec<BlockItem> = Vec::new();

        while TokenType::CloseBrace != self.peek_token().unwrap().token_type {
            let block_item = self.parse_block_item();
            block_items.push(block_item);
        }
        self.consume_token();

        Block { block_items }
    }

    fn parse_block_item(&mut self) -> BlockItem {
        match self.peek_token().map(|t| t.token_type) {
            Some(TokenType::KeywordInt)
            | Some(TokenType::KeywordLong)
            | Some(TokenType::KeywordStatic)
            | Some(TokenType::KeywordExtern) => BlockItem::Declaration(self.parse_declaration()),
            _ => BlockItem::Statement(self.parse_statement()),
        }
    }

    fn parse_declaration(&mut self) -> Declaration {
        // Type is always int32 for now, ignore for now
        let (decl_type, storage_class) = self.parse_type_and_storage_class();

        let name = match self.consume_token() {
            Some(token) => match token.token_type {
                TokenType::Identifier(name) => name,
                _ => panic!("Expected identifier after int keyword!"),
            },
            None => panic!("Unexpected end of tokens!"),
        };

        match self.peek_token().map(|t| t.token_type) {
            // Since we have = this is variable declaration
            Some(TokenType::Assignment) => {
                self.consume_token();
                let expr = self.parse_expression(0);
                self.expect_token(TokenType::Semicolon);

                Declaration::VarDecl(VarDecl {
                    name,
                    init: Some(expr),
                    storage_class,
                    var_type: decl_type,
                })
            }
            Some(TokenType::Semicolon) => {
                self.consume_token();
                Declaration::VarDecl(VarDecl {
                    name,
                    init: None,
                    storage_class,
                    var_type: decl_type,
                })
            }
            Some(TokenType::OpenParen) => {
                self.consume_token();
                let (params, param_types) = self.parse_param_list();
                self.expect_token(TokenType::CloseParen);

                let func_type = Type::FuncType {
                    params: param_types,
                    ret: Box::new(decl_type),
                };

                if let Some(TokenType::OpenBrace) = self.peek_token().map(|t| t.token_type) {
                    let body = self.parse_block();
                    Declaration::FunDecl(FunDecl {
                        name,
                        params,
                        body: Some(body),
                        storage_class,
                        func_type,
                    })
                } else {
                    self.expect_token(TokenType::Semicolon);
                    Declaration::FunDecl(FunDecl {
                        name,
                        params,
                        body: None,
                        storage_class,
                        func_type,
                    })
                }
            }
            _ => panic!(
                "parse_declaration Unexpected token in declaration {:?}",
                self.peek_token()
            ),
        }
    }

    fn parse_type_and_storage_class(&mut self) -> (Type, Option<StorageClass>) {
        let mut types = Vec::<Type>::new();
        let mut storage_classes = Vec::<StorageClass>::new();
        while let Some(token) = self.peek_token() {
            match token.token_type {
                TokenType::KeywordInt => {
                    self.consume_token();
                    types.push(Type::Int);
                }
                TokenType::KeywordLong => {
                    self.consume_token();
                    types.push(Type::Long);
                }
                TokenType::KeywordStatic => {
                    self.consume_token();
                    storage_classes.push(StorageClass::Static);
                }
                TokenType::KeywordExtern => {
                    self.consume_token();
                    storage_classes.push(StorageClass::Extern);
                }
                _ => {
                    break;
                }
            }
        }

        let result_type = self.parse_type(types);

        // And at most 1 storage class
        if storage_classes.len() > 1 {
            panic!(
                "Incorrect number of storage classess for declaration {}: {:?}",
                storage_classes.len(),
                storage_classes
            );
        }

        let storage_class: Option<StorageClass> = if storage_classes.len() == 1 {
            Some(storage_classes[0])
        } else {
            None
        };

        (result_type, storage_class)
    }

    fn parse_type(&mut self, types: Vec<Type>) -> Type {
        match types.as_slice() {
            [Type::Int] => Type::Int,
            [Type::Long] | [Type::Int, Type::Long] | [Type::Long, Type::Int] => Type::Long,
            _ => panic!("Invalid type specifier {:?}", types),
        }
    }

    fn parse_param_list(&mut self) -> (Vec<String>, Vec<Type>) {
        let next_token = self.peek_token().map(|t| t.token_type);

        if next_token == Some(TokenType::CloseParen) {
            return (Vec::new(), Vec::new());
        }

        if next_token == Some(TokenType::KeywordVoid) {
            self.consume_token();
            if self.peek_token().map(|t| t.token_type) == Some(TokenType::CloseParen) {
                return (Vec::new(), Vec::new());
            }
            panic!("parse_param_list 'void' must be the only parameter");
        }

        let mut params = Vec::new();
        let mut param_types = Vec::new();
        loop {
            let (ptype, sc) = self.parse_type_and_storage_class();
            if sc.is_some() {
                panic!("parse_param_list Storage class not allowed in parameter list");
            }

            match self.consume_token().map(|t| t.token_type) {
                Some(TokenType::Identifier(name)) => {
                    params.push(name);
                    param_types.push(ptype);
                }
                _ => panic!("parse_param_list Expected identifier"),
            }

            if let Some(TokenType::Comma) = self.peek_token().map(|t| t.token_type) {
                self.consume_token();
            } else {
                break;
            }
        }
        (params, param_types)
    }

    fn parse_statement(&mut self) -> Statement {
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
            Some(TokenType::KeywordBreak) => {
                self.consume_token();
                self.expect_token(TokenType::Semicolon);
                Statement::Break {
                    label: "dummy_break".to_string(), // to be replaced during semantic validation
                }
            }
            Some(TokenType::KeywordContinue) => {
                self.consume_token();
                self.expect_token(TokenType::Semicolon);
                Statement::Continue {
                    label: "dummy_continue".to_string(), // to be replaced during semantic validation
                }
            }
            Some(TokenType::KeywordWhile) => {
                self.consume_token();
                self.expect_token(TokenType::OpenParen);
                let expr = self.parse_expression(0);
                self.expect_token(TokenType::CloseParen);
                let statement = self.parse_statement();
                Statement::While {
                    condition: expr,
                    body: Box::new(statement),
                    label: "dummy_while".to_string(), // to be replaced during semantic validation
                }
            }
            Some(TokenType::KeywordDo) => {
                self.consume_token();
                let statement = self.parse_statement();
                self.expect_token(TokenType::KeywordWhile);
                self.expect_token(TokenType::OpenParen);
                let expr = self.parse_expression(0);
                self.expect_token(TokenType::CloseParen);
                self.expect_token(TokenType::Semicolon);

                Statement::DoWhile {
                    body: Box::new(statement),
                    condition: expr,
                    label: "dummy_dowhile".to_string(), // to be replaced during semantic validation
                }
            }
            Some(TokenType::KeywordFor) => {
                self.consume_token();
                self.expect_token(TokenType::OpenParen);

                // For loop is for (for_init; condition; post)
                // Condition and post are optional, ; are not
                let for_init = self.parse_for_init();

                let condition =
                    if self.peek_token().map(|t| t.token_type) != Some(TokenType::Semicolon) {
                        Some(Box::new(self.parse_expression(0)))
                    } else {
                        None
                    };
                self.expect_token(TokenType::Semicolon);

                let post = if self.peek_token().map(|t| t.token_type) != Some(TokenType::CloseParen)
                {
                    Some(Box::new(self.parse_expression(0)))
                } else {
                    None
                };

                self.expect_token(TokenType::CloseParen);

                let body = self.parse_statement();

                Statement::For {
                    init: for_init,
                    condition,
                    post,
                    body: Box::new(body),
                    label: "dummy_for".to_string(),
                }
            }
            _ => {
                let expr = self.parse_expression(0);
                self.expect_token(TokenType::Semicolon);
                Statement::Expression(expr)
            }
        }
    }

    fn parse_for_init(&mut self) -> ForInit {
        if let Some(TokenType::KeywordInt)
        | Some(TokenType::KeywordLong)
        | Some(TokenType::KeywordStatic)
        | Some(TokenType::KeywordExtern) = self.peek_token().map(|t| t.token_type)
        {
            match self.parse_declaration() {
                Declaration::VarDecl(var_decl) => ForInit::InitDeclaration(var_decl),
                _ => {
                    panic!("parse_for_init Expected variable declaration!");
                }
            }
        } else {
            let expr = if self.peek_token().map(|t| t.token_type) != Some(TokenType::Semicolon) {
                Some(Box::new(self.parse_expression(0)))
            } else {
                None
            };
            self.expect_token(TokenType::Semicolon);

            ForInit::InitExpression(expr)
        }
    }

    fn parse_factor(&mut self) -> TypedExpression {
        match self.consume_token() {
            Some(token) => match token.token_type {
                TokenType::Integer32(val) => TypedExpression {
                    expr: Expression::Constant(Const::ConstInt(val)),
                    etype: None,
                },
                TokenType::Integer64(val) => TypedExpression {
                    expr: Expression::Constant(Const::ConstLong(val)),
                    etype: None,
                },
                TokenType::Hyphen => TypedExpression {
                    expr: Expression::UnaryExpr(
                        UnaryOperator::Negate,
                        Box::new(self.parse_factor()),
                    ),
                    etype: None,
                },
                TokenType::Tilde => TypedExpression {
                    expr: Expression::UnaryExpr(
                        UnaryOperator::Complement,
                        Box::new(self.parse_factor()),
                    ),
                    etype: None,
                },
                TokenType::Not => TypedExpression {
                    expr: Expression::UnaryExpr(UnaryOperator::Not, Box::new(self.parse_factor())),
                    etype: None,
                },
                TokenType::OpenParen => {
                    let next_token = self.peek_token().map(|t| t.token_type);
                    if next_token == Some(TokenType::KeywordInt)
                        || next_token == Some(TokenType::KeywordLong)
                    {
                        let mut types = Vec::new();
                        while let Some(token) = self.peek_token() {
                            match token.token_type {
                                TokenType::KeywordInt => {
                                    self.consume_token();
                                    types.push(Type::Int);
                                }
                                TokenType::KeywordLong => {
                                    self.consume_token();
                                    types.push(Type::Long);
                                }
                                _ => break,
                            }
                        }
                        let target_type = self.parse_type(types);
                        self.expect_token(TokenType::CloseParen);
                        let exp = self.parse_factor();
                        TypedExpression {
                            expr: Expression::Cast {
                                target_type,
                                exp: Box::new(exp),
                            },
                            etype: None,
                        }
                    } else {
                        let inner_expr = self.parse_expression(0);
                        self.expect_token(TokenType::CloseParen);
                        inner_expr
                    }
                }
                TokenType::Identifier(name) => {
                    if let Some(TokenType::OpenParen) = self.peek_token().map(|t| t.token_type) {
                        self.consume_token();

                        let args = self.parse_argument_list();
                        self.expect_token(TokenType::CloseParen);
                        TypedExpression {
                            expr: Expression::FunctionCall { name, args },
                            etype: None,
                        }
                    } else {
                        TypedExpression {
                            expr: Expression::Var(name),
                            etype: None,
                        }
                    }
                }

                _ => panic!("Expected factor got {:?}", token),
            },
            None => panic!("Unexpected EOF while parsing factor"),
        }
    }

    fn parse_argument_list(&mut self) -> Vec<TypedExpression> {
        let mut args = Vec::new();

        if let Some(TokenType::CloseParen) = self.peek_token().map(|t| t.token_type) {
            return args;
        }

        args.push(self.parse_expression(0));

        while let Some(TokenType::Comma) = self.peek_token().map(|t| t.token_type) {
            self.consume_token();
            args.push(self.parse_expression(0));
        }

        args
    }

    fn parse_expression(&mut self, min_prec: u32) -> TypedExpression {
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
                left = TypedExpression {
                    expr: Expression::Assignment(Box::new(left), Box::new(right)),
                    etype: None,
                };
            } else if next_token.token_type == TokenType::QuestionMark {
                // Parse middle expression of ? <exp> : <exp> until :
                let middle = self.parse_expression(0);
                self.expect_token(TokenType::Colon);

                // right is after :
                let right = self.parse_expression(precedence);

                // left = Expression::Assignment(Box::new(left), Box::new(right));
                left = TypedExpression {
                    expr: Expression::Conditional {
                        condition: Box::new(left),
                        exp1: Box::new(middle),
                        exp2: Box::new(right),
                    },
                    etype: None,
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
                left = TypedExpression {
                    expr: Expression::BinaryExp(Box::new(left), op, Box::new(right)),
                    etype: None,
                };
            }
        }

        left
    }

    fn expect_token(&mut self, expected: TokenType) {
        match self.consume_token() {
            Some(token) => {
                if std::mem::discriminant(&token.token_type) != std::mem::discriminant(&expected) {
                    panic!("Expected {:?} but found {:?}", expected, token.token_type);
                }
            }
            None => panic!("Expected {:?}, but found EOF", expected),
        }
    }

    fn peek_token(&mut self) -> Option<Token> {
        if self.current_positon >= self.tokens.len() {
            return None;
        }

        let token = self.tokens[self.current_positon].clone();
        Some(token)
    }

    fn consume_token(&mut self) -> Option<Token> {
        if self.current_positon >= self.tokens.len() {
            return None;
        }
        let token = self.tokens[self.current_positon].clone();
        self.current_positon += 1;
        Some(token)
    }
}
