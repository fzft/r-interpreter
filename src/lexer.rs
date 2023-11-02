use super::token::{Token, TokenType};

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    // current position in input (points to current char)
    read_position: usize,
    // current reading position in input (after current char)
    ch: u8,                  // current char under examination
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0u8;
        } else {
            // Danger: This can panic if the input contains non-ASCII characters
            // and you try to index at a non-character boundary.
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let start_position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[start_position..self.position].to_string()
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(self.read_position)
    }

    fn read_string(&mut self) -> String {
        let start_position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == b'\0' {
                break;
            }
        }
        self.input[start_position..self.position].to_string()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            b'=' => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(TokenType::Eq, "==".to_string())
                } else {
                    Token::new(TokenType::Assign, (self.ch as char).to_string())
                }
            }
            b';' => Token::new(TokenType::Semicolon, (self.ch as char).to_string()),
            b'(' => Token::new(TokenType::LParen, (self.ch as char).to_string()),
            b')' => Token::new(TokenType::RParen, (self.ch as char).to_string()),
            b',' => Token::new(TokenType::Comma, (self.ch as char).to_string()),
            b'+' => Token::new(TokenType::Plus, (self.ch as char).to_string()),
            b'-' => Token::new(TokenType::Minus, (self.ch as char).to_string()),
            b'!' => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".to_string())
                } else {
                    Token::new(TokenType::Bang, (self.ch as char).to_string())
                }
            },
            b'/' => Token::new(TokenType::Slash, (self.ch as char).to_string()),
            b'*' => Token::new(TokenType::Asterisk, (self.ch as char).to_string()),
            b'<' => Token::new(TokenType::Lt, (self.ch as char).to_string()),
            b'>' => Token::new(TokenType::Gt, (self.ch as char).to_string()),
            b'{' => Token::new(TokenType::LBrace, (self.ch as char).to_string()),
            b'}' => Token::new(TokenType::RBrace, (self.ch as char).to_string()),
            b'"' => {
                let literal = self.read_string();
                Token::new(TokenType::String, literal)
            }
            b'[' => Token::new(TokenType::LBracket, (self.ch as char).to_string()),
            b']' => Token::new(TokenType::RBracket, (self.ch as char).to_string()),
            b':' => Token::new(TokenType::Colon, (self.ch as char).to_string()),
            0 => {
                Token::new(TokenType::Eof, '\0'.to_string())
            }
            _ => {
                let tok;
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    tok = Token::new(TokenType::lookup_ident(&literal), literal);
                    return tok;
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    tok = Token::new(TokenType::Int, literal);
                    return tok;
                } else {
                    tok = Token::new(TokenType::Illegal, (self.ch as char).to_string());
                }
                tok
            } // or some other handling for unexpected characters
        };

        self.read_char();
        tok
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }
}

fn is_letter(ch: u8) -> bool {
    (ch >= b'a' && ch <= b'z') || (ch >= b'A' && ch <= b'Z') || ch == b'_'
}

fn is_digit(ch: u8) -> bool {
    ch >= b'0' && ch <= b'9'
}

#[cfg(test)]
mod tests {
    use super::{TokenType};
    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";
        let tests = vec![
            (TokenType::Assign, "="),
            (TokenType::Plus, "+"),
            (TokenType::LParen, "("),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::RBrace, "}"),
            (TokenType::Comma, ","),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, "\0"),
        ];

        let mut lexer = Lexer::new(input);
        for (i, (expected_type, expected_literal)) in tests.iter().enumerate() {
            let tok = lexer.next_token();

            assert_eq!(tok.token_type, *expected_type, "Test[{}] - TokenType wrong. Expected {:?}, got {:?}", i, expected_type, tok.token_type);
            assert_eq!(tok.literal, *expected_literal, "Test[{}] - Literal wrong. Expected {:?}, got {:?}", i, expected_literal, tok.literal);
        }
    }


    #[test]
    fn test_next_token2() {
        let input = "let five = 5;\
                     let ten = 10;\
                     let add = fn(x, y) {\
                         x + y;\
                     };\
                     let result = add(five, ten);";

        let tests = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, "\0"), // If you've decided to use '\0' as EOF representation
        ];

        let mut lexer = Lexer::new(input);
        for (i, (expected_type, expected_literal)) in tests.iter().enumerate() {
            let tok = lexer.next_token();

            assert_eq!(tok.token_type, *expected_type, "Test[{}] - TokenType wrong. Expected {:?}, got {:?}", i, expected_type, tok.token_type);
            assert_eq!(tok.literal, *expected_literal, "Test[{}] - Literal wrong. Expected {:?}, got {:?}", i, expected_literal, tok.literal);
        }
    }

    #[test]
    fn test_next_token3() {
        let input = r#"let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
    return true;
} else {
    return false;
}
        10 == 10;
        10 != 9;
        "foobar"
        "foo bar"
        [1, 2];
        {"foo": "bar"}
        "#;

        let tests = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Gt, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            // Add the tokens for "10 == 10;"
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),

            // Add the tokens for "10 != 9;"
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),

            // "foobar", "foo bar"
            (TokenType::String, "foobar"),
            (TokenType::String, "foo bar"),
            // Add the tokens for "[1, 2];"
            (TokenType::LBracket, "["),
            (TokenType::Int, "1"),
            (TokenType::Comma, ","),
            (TokenType::Int, "2"),
            (TokenType::RBracket, "]"),
            (TokenType::Semicolon, ";"),
            // Add the tokens for "{"foo": "bar"};"
            (TokenType::LBrace, "{"),
            (TokenType::String, "foo"),
            (TokenType::Colon, ":"),
            (TokenType::String, "bar"),
            (TokenType::RBrace, "}"),

            (TokenType::Eof, "\0"),
        ];

        let mut lexer = Lexer::new(input);
        for (i, (expected_type, expected_literal)) in tests.iter().enumerate() {
            let tok = lexer.next_token();

            assert_eq!(tok.token_type, *expected_type, "Test[{}] - TokenType wrong. Expected {:?}, got {:?}", i, expected_type, tok.token_type);
            assert_eq!(tok.literal, *expected_literal, "Test[{}] - Literal wrong. Expected {:?}, got {:?}", i, expected_literal, tok.literal);
        }
    }
}



