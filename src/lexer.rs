use core::panic;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Keywords
    KeywordInt,
    KeywordReturn,
    KeywordVoid,
    KeywordIf,
    KeywordElse,
    KeywordDo,
    KeywordWhile,
    KeywordFor,
    KeywordBreak,
    KeywordContinue,
    KeywordStatic,
    KeywordExtern,

    OpenParen,    // (
    CloseParen,   // )
    OpenBrace,    // {
    CloseBrace,   // }
    Semicolon,    // ;
    Tilde,        // ~
    Hyphen,       // -
    Plus,         // +
    Asterisk,     // *
    ForwardSlash, // /
    Percent,      // %
    Assignment,   // =
    QuestionMark, // ?
    Colon,        // ;
    Comma,

    // Logical operators
    Not,            // !
    And,            // &&
    Or,             // ||
    EqualTo,        // ==
    NotEqualTo,     // !=
    LessThan,       // <
    GreaterThan,    // <
    LessOrEqual,    // <=
    GreaterOrEqual, // >=

    // Literals
    Integer(i32),
    Identifier(String),

    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    //TODO Add info about location etc
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            chars: input.chars().peekable(),
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(&c) = self.chars.peek() {
            match c {
                // Skip if whitespace
                ' ' | '\t' | '\r' => {
                    self.chars.next();
                }
                '\n' => {
                    self.chars.next();
                }

                // Parse single line chars now
                '(' => {
                    tokens.push(Token {
                        token_type: TokenType::OpenParen,
                    });
                    self.chars.next();
                }
                ')' => {
                    tokens.push(Token {
                        token_type: TokenType::CloseParen,
                    });
                    self.chars.next();
                }
                '{' => {
                    tokens.push(Token {
                        token_type: TokenType::OpenBrace,
                    });
                    self.chars.next();
                }
                '}' => {
                    tokens.push(Token {
                        token_type: TokenType::CloseBrace,
                    });
                    self.chars.next();
                }
                ';' => {
                    tokens.push(Token {
                        token_type: TokenType::Semicolon,
                    });
                    self.chars.next();
                }
                '/' => {
                    self.lex_forwardslash(&mut tokens);
                }
                '-' => {
                    tokens.push(Token {
                        token_type: TokenType::Hyphen,
                    });
                    self.chars.next();
                }
                '~' => {
                    tokens.push(Token {
                        token_type: TokenType::Tilde,
                    });
                    self.chars.next();
                }
                '+' => {
                    tokens.push(Token {
                        token_type: TokenType::Plus,
                    });
                    self.chars.next();
                }
                '*' => {
                    tokens.push(Token {
                        token_type: TokenType::Asterisk,
                    });
                    self.chars.next();
                }
                '%' => {
                    tokens.push(Token {
                        token_type: TokenType::Percent,
                    });
                    self.chars.next();
                }
                '!' => {
                    self.chars.next();
                    if Some('=') == self.chars.peek().copied() {
                        tokens.push(Token {
                            token_type: TokenType::NotEqualTo,
                        });
                        self.chars.next();
                    } else {
                        tokens.push(Token {
                            token_type: TokenType::Not,
                        });
                    }
                }
                '&' => {
                    self.chars.next();
                    if Some('&') == self.chars.peek().copied() {
                        tokens.push(Token {
                            token_type: TokenType::And,
                        });
                        self.chars.next();
                    }
                    //TODO Single appersand support
                }
                '|' => {
                    self.chars.next();
                    if Some('|') == self.chars.peek().copied() {
                        tokens.push(Token {
                            token_type: TokenType::Or,
                        });
                        self.chars.next();
                    } else {
                        panic!("Single | not supported!");
                    }
                }
                '=' => {
                    self.chars.next();
                    if Some('=') == self.chars.peek().copied() {
                        tokens.push(Token {
                            token_type: TokenType::EqualTo,
                        });
                        self.chars.next();
                    } else {
                        tokens.push(Token {
                            token_type: TokenType::Assignment,
                        });
                    }
                }
                '<' => {
                    self.chars.next();
                    if Some('=') == self.chars.peek().copied() {
                        tokens.push(Token {
                            token_type: TokenType::LessOrEqual,
                        });
                        self.chars.next();
                    } else {
                        tokens.push(Token {
                            token_type: TokenType::LessThan,
                        });
                    }
                }
                '>' => {
                    self.chars.next();
                    if Some('=') == self.chars.peek().copied() {
                        tokens.push(Token {
                            token_type: TokenType::GreaterOrEqual,
                        });
                        self.chars.next();
                    } else {
                        tokens.push(Token {
                            token_type: TokenType::GreaterThan,
                        });
                    }
                }
                '#' => {
                    // Dirty workaround for the test sets containing #pragma and #include for some reason at this stage....
                    while let Some(&c) = self.chars.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.chars.next();
                    }
                }
                '?' => {
                    tokens.push(Token {
                        token_type: TokenType::QuestionMark,
                    });
                    self.chars.next();
                }
                ':' => {
                    tokens.push(Token {
                        token_type: TokenType::Colon,
                    });
                    self.chars.next();
                }
                ',' => {
                    tokens.push(Token {
                        token_type: TokenType::Comma,
                    });
                    self.chars.next();
                }

                '0'..='9' => {
                    let num = self.lex_integer();
                    tokens.push(Token {
                        token_type: TokenType::Integer(num),
                    });
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let word = self.lex_word();
                    // Match keywords
                    // Otherwise it is identifier
                    let word_type = match word.as_str() {
                        "int" => TokenType::KeywordInt,
                        "void" => TokenType::KeywordVoid,
                        "return" => TokenType::KeywordReturn,
                        "else" => TokenType::KeywordElse,
                        "if" => TokenType::KeywordIf,
                        "do" => TokenType::KeywordDo,
                        "while" => TokenType::KeywordWhile,
                        "for" => TokenType::KeywordFor,
                        "break" => TokenType::KeywordBreak,
                        "continue" => TokenType::KeywordContinue,
                        "static" => TokenType::KeywordStatic,
                        "extern" => TokenType::KeywordExtern,
                        _ => TokenType::Identifier(word),
                    };
                    tokens.push(Token {
                        token_type: word_type,
                    });
                }
                _ => {
                    println!("Tokens so far {:?}", tokens);
                    panic!("Unexpected character {}", c)
                }
            }
        }

        tokens.push(Token {
            token_type: TokenType::EOF,
        });

        tokens
    }

    fn lex_integer(&mut self) -> i32 {
        let mut num_str = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_ascii_digit() {
                num_str.push(c);
                self.chars.next();
            } else if c.is_ascii_alphabetic() {
                panic!("Alphabetic chars have no place next to digits!");
            } else {
                break;
            }
        }
        num_str.parse().expect("Expected to parse an integer")
    }

    fn lex_word(&mut self) -> String {
        let mut word = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                word.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        word
    }

    fn lex_forwardslash(&mut self, tokens: &mut Vec<Token>) {
        // Consume the first '/'
        self.chars.next();

        // Check if the next character is also a '/', indicating a comment
        if let Some('/') = self.chars.peek().copied() {
            // It's a comment, consume the second '/'
            self.chars.next();

            // Consume characters until a newline or EOF is reached
            while let Some(&c) = self.chars.peek() {
                if c == '\n' {
                    break;
                }
                self.chars.next();
            }
        // Check if it is /* comment */
        } else if let Some('*') = self.chars.peek().copied() {
            // Consume *
            self.chars.next();

            // Consume characters until */
            while let Some(c) = self.chars.next() {
                if c == '*' && Some('/') == self.chars.peek().copied() {
                    self.chars.next();
                    break;
                }
            }
        } else {
            tokens.push(Token {
                token_type: TokenType::ForwardSlash,
            });
            self.chars.next();
        }
    }
}
