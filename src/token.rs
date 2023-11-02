use std::collections::HashMap;
use lazy_static::lazy_static;


#[derive(Debug, Clone, PartialEq, Hash, Copy, Eq)]
pub enum TokenType {
    Illegal,
    Eof,
    Ident,
    Int,
    String,

    // Contains the integer value
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq, // ==
    NotEq, // !=

    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    LBracket,
    RBracket,
    Colon,

}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenType::Function);
        m.insert("let", TokenType::Let);
        m.insert("true", TokenType::True);
        m.insert("false", TokenType::False);
        m.insert("if", TokenType::If);
        m.insert("else", TokenType::Else);
        m.insert("return", TokenType::Return);
        m
    };
}

impl TokenType {
    pub fn lookup_ident(ident: &str) -> Self {
        KEYWORDS.get(ident).cloned().unwrap_or(TokenType::Ident)
    }
}


impl Default for TokenType {
    fn default() -> Self {
        TokenType::Illegal
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Token {
            token_type,
            literal,
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            token_type: TokenType::default(), // Assuming TokenType implements Default
            literal: String::from(""),       // Default literal is an empty string
        }
    }
}