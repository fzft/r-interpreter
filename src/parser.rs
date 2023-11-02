use std::collections::HashMap;
use crate::{ast, lexer};
use crate::token::{Token, TokenType};
use lazy_static::lazy_static;
use crate::ast::{Node, Statement};
use std::any::Any;


#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
enum Precedence {
    LOWEST = 0,
    EQUALS,
    // ==
    LESSGREATER,
    // > or <
    SUM,
    // +
    PRODUCT,
    // *
    PREFIX,
    // -X or !X
    CALL,         // myFunction(X)

    INDEX,
}

lazy_static! {
    static ref PRECEDENCES: HashMap<TokenType, Precedence> = {
        let mut m = HashMap::new();
        m.insert(TokenType::Eq, Precedence::EQUALS);
        m.insert(TokenType::NotEq, Precedence::EQUALS);
        m.insert(TokenType::Lt, Precedence::LESSGREATER);
        m.insert(TokenType::Gt, Precedence::LESSGREATER);
        m.insert(TokenType::Plus, Precedence::SUM);
        m.insert(TokenType::Minus, Precedence::SUM);
        m.insert(TokenType::Slash, Precedence::PRODUCT);
        m.insert(TokenType::Asterisk, Precedence::PRODUCT);
        m.insert(TokenType::LParen, Precedence::CALL);
        m.insert(TokenType::LBracket, Precedence::INDEX);
        m
    };
}

type PrefixParseFn<'a> = fn(&mut Parser<'a>) -> Option<Box<dyn ast::Expression>>;
type InfixParseFn<'a> = fn(&mut Parser<'a>, Option<Box<dyn ast::Expression>>) -> Option<Box<dyn ast::Expression>>;

pub struct Parser<'a> {
    l: lexer::Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn<'a>>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(l: lexer::Lexer<'a>) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::default(),  // assuming a default implementation for Token
            peek_token: Token::default(),
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        p.register_prefix(TokenType::Ident, Parser::parse_identifier);
        p.register_prefix(TokenType::Int, Parser::parse_integer_literal);
        p.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);

        p.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        p.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        p.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        p.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        p.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        p.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        p.register_infix(TokenType::Lt, Parser::parse_infix_expression);
        p.register_infix(TokenType::Gt, Parser::parse_infix_expression);
        p.register_infix(TokenType::LParen, Parser::parse_call_expression);
        p.register_infix(TokenType::LBracket, Parser::parse_index_expression);

        p.prefix_parse_fns.insert(TokenType::True, Parser::parse_boolean);
        p.prefix_parse_fns.insert(TokenType::False, Parser::parse_boolean);

        p.register_prefix(TokenType::If, Parser::parse_if_expression);
        p.register_prefix(TokenType::LParen, Parser::parse_grouped_expression);
        p.register_prefix(TokenType::Function, Parser::parse_function_literal);
        p.register_prefix(TokenType::String, Parser::parse_string_literal);
        p.register_prefix(TokenType::LBracket, Parser::parse_array_literal);
        p.register_prefix(TokenType::LBrace, Parser::parse_hash_literal);

        p.next_token();
        p.next_token();
        p
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn<'a>) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn<'a>) {
        self.infix_parse_fns.insert(token_type, func);
    }


    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program {
            statements: Vec::new(),
        };

        while self.cur_token.token_type != TokenType::Eof {
            let stmt = self.parse_statement();
            if let Some(statement) = stmt {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        match self.cur_token.token_type {
            TokenType::Let => {
                let let_stmt = self.parse_let_statement()?;
                Some(let_stmt as Box<dyn ast::Statement>)
            }
            TokenType::Return => {
                let return_stmt = self.parse_return_statement()?;
                Some(return_stmt as Box<dyn ast::Statement>)
            }
            _ => {
                let stmt = self.parse_expression_statement()?;
                Some(stmt as Box<dyn ast::Statement>)
            }
        }
    }


    fn parse_let_statement(&mut self) -> Option<Box<ast::LetStatement>> {
        let mut stmt = Box::new(ast::LetStatement::new(self.cur_token.clone(), ast::Identifier::default(), None));

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        stmt.name = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        self.next_token();

        stmt.value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_function_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        let mut lit = ast::FunctionLiteral {
            token: self.cur_token.clone(),
            ..Default::default()
        };

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        lit.parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        lit.body = self.parse_block_statement();

        Some(Box::new(lit) as Box<dyn ast::Expression>)
    }

    fn parse_function_parameters(&mut self) -> Vec<ast::Identifier> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return identifiers;
        }

        self.next_token();
        let ident = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        identifiers.push(ident);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            let ident = ast::Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };
            identifiers.push(ident);
        }

        if !self.expect_peek(TokenType::RParen) {
            return vec![];  // This returns an empty vector to indicate an error.
        }

        identifiers
    }

    pub fn parse_index_expression(&mut self, left: Option<Box<dyn ast::Expression>>) -> Option<Box<dyn ast::Expression>> {
        let mut exp = ast::IndexExpression {
            token: self.cur_token.clone(),
            left,
            index: None,
        };

        self.next_token();
        exp.index = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(TokenType::RBracket) {
            return None;
        }


        Some(Box::new(exp))
    }

    fn parse_hash_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        let mut hash = ast::HashLiteral {
            token: self.cur_token.clone(),
            pairs: HashMap::new(),
        };

        while !self.peek_token_is(TokenType::RBrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST);

            if !self.expect_peek(TokenType::Colon) {
                return None;
            }

            self.next_token();
            let value = self.parse_expression(Precedence::LOWEST);

            hash.pairs.insert(key.unwrap(), value.unwrap());

            if !self.peek_token_is(TokenType::RBrace) && !self.expect_peek(TokenType::Comma) {
                return None;
            }
        }

        if !self.expect_peek(TokenType::RBrace) {
            return None;
        }

        Some(Box::new(hash))
    }


    fn parse_return_statement(&mut self) -> Option<Box<ast::ReturnStatement>> {
        let mut stmt = Box::new(ast::ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None,
        });

        self.next_token();

        stmt.return_value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expression_statement(&mut self) -> Option<Box<ast::ExpressionStatement>> {
        let mut stmt = Box::new(ast::ExpressionStatement {
            token: self.cur_token.clone(),
            expression: None,
        });

        // Parse the expression here, once implemented
        stmt.expression = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_string_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        Some(Box::new(ast::StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_block_statement(&mut self) -> Option<ast::BlockStatement> {
        let mut block = ast::BlockStatement {
            token: self.cur_token.clone(),
            statements: Vec::new(),
        };

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::Eof) {
            if let Some(stmt) = self.parse_statement() {
                block.statements.push(stmt);
            }
            self.next_token();
        }

        Some(block)
    }

    fn parse_call_expression(&mut self, function: Option<Box<dyn ast::Expression>>) -> Option<Box<dyn ast::Expression>> {
        let exp = ast::CallExpression {
            token: self.cur_token.clone(),
            function,
            arguments: self.parse_expression_list(TokenType::RParen),
        };
        Some(Box::new(exp) as Box<dyn ast::Expression>)
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        let mut expression = ast::IfExpression {
            token: self.cur_token.clone(),
            ..Default::default()
        };

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        self.next_token();
        expression.condition = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        expression.consequence = self.parse_block_statement();

        if self.peek_token_is(TokenType::Else) {
            self.next_token();

            if !self.expect_peek(TokenType::LBrace) {
                return None;
            }

            expression.alternative = self.parse_block_statement();
        }

        Some(Box::new(expression))
    }


    fn parse_prefix_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        let mut expression = Box::new(ast::PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            right: None,  // This will be populated in the next line
        });

        self.next_token();
        expression.right = self.parse_expression(Precedence::PREFIX);

        Some(expression)
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        exp
    }

    fn parse_infix_expression(&mut self, left: Option<Box<dyn ast::Expression>>) -> Option<Box<dyn ast::Expression>> {
        let mut expression = ast::InfixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            left: left,
            right: None,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        expression.right = self.parse_expression(precedence);

        Some(Box::new(expression) as Box<dyn ast::Expression>)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn ast::Expression>> {
        let prefix_fn = self.prefix_parse_fns.get(&self.cur_token.token_type);

        let mut left_exp;
        match prefix_fn {
            Some(prefix) => {
                left_exp = prefix(self)
            }
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.token_type);
                return None;
            }
        }

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            let infix = self.infix_parse_fns.get(&self.peek_token.token_type).cloned();
            match infix {
                Some(infix_fn) => {
                    self.next_token();
                    left_exp = infix_fn(self, left_exp);
                }
                None => {
                    return left_exp;
                }
            }
        }

        left_exp
    }


    fn parse_identifier(&mut self) -> Option<Box<dyn ast::Expression>> {
        Some(Box::new(ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_array_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        let array = ast::ArrayLiteral {
            token: self.cur_token.clone(),
            elements: self.parse_expression_list(TokenType::RBracket),
        };
        Some(Box::new(array))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Box<dyn ast::Expression>> {
        let mut list: Vec<Box<dyn ast::Expression>> = Vec::new();

        if self.peek_token_is(end) {
            self.next_token();
            return list;
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::LOWEST).unwrap());

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::LOWEST).unwrap());
        }

        if !self.expect_peek(end) {
            return Vec::new();
        }

        list
    }

    fn parse_boolean(&mut self) -> Option<Box<dyn ast::Expression>> {
        Some(Box::new(ast::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::True),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        let value = self.cur_token.literal.parse::<i64>();

        match value {
            Ok(val) => {
                let lit = Box::new(ast::IntegerLiteral {
                    token: self.cur_token.clone(),
                    value: val,
                });
                Some(lit)
            }
            Err(_) => {
                let msg = format!("could not parse {} as integer", self.cur_token.literal);
                self.errors.push(msg);
                None
            }
        }
    }

    fn peek_precedence(&self) -> Precedence {
        *PRECEDENCES.get(&self.peek_token.token_type).unwrap_or(&Precedence::LOWEST)
    }

    fn cur_precedence(&self) -> Precedence {
        *PRECEDENCES.get(&self.cur_token.token_type).unwrap_or(&Precedence::LOWEST)
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        let msg = format!("no prefix parse function for {:?} found", t);
        self.errors.push(msg);
    }

    fn peek_error(&mut self, expected: TokenType, got: TokenType) { // Assuming you have a TokenType enum in Rust
        let msg = format!("expected next token to be {:?}, got {:?} instead", expected, got);
        self.errors.push(msg);
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.token_type == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.token_type == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t, self.peek_token.token_type);
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Expression;
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 3, "program.statements does not contain 3 statements");

        let expected_identifiers = vec!["x", "y", "foobar"];

        for (i, expected_identifier) in expected_identifiers.iter().enumerate() {
            test_let_statement(&program.statements[i], expected_identifier);
        }
    }


    fn test_let_statement(s: &Box<dyn ast::Statement>, name: &str) {
        assert_eq!(s.token_literal(), "let", "s.token_literal not 'let'");

        if let Some(let_stmt) = s.as_any().downcast_ref::<ast::LetStatement>() {
            assert_eq!(let_stmt.name.value, name, "letStmt.Name.Value not '{}'", name);
            assert_eq!(let_stmt.name.token_literal(), name, "letStmt.Name.TokenLiteral() not '{}'", name);
        } else {
            panic!("let statement code is not yet implemented.");
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 3, "program.Statements does not contain 3 statements. got={}", program.statements.len());
        for stmt in &program.statements {
            if let Some(return_stmt) = stmt.as_any().downcast_ref::<ast::ReturnStatement>() {
                assert_eq!(return_stmt.token_literal(), "return", "returnStmt.TokenLiteral not 'return', got {}", return_stmt.token_literal());
            } else {
                panic!("return statements code is not yet implemented.");
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);  // Assuming you have a check_parser_errors function


        assert_eq!(program.statements.len(), 1, "program has not enough statements. got={}", program.statements.len());

        match program.statements[0].as_any().downcast_ref::<ast::ExpressionStatement>() {
            Some(expr_stmt) => {
                // Checking if the expression is present
                match &expr_stmt.expression {
                    Some(expr) => {
                        match expr.as_any().downcast_ref::<ast::Identifier>() {
                            Some(ident) => {
                                assert_eq!(ident.value, "foobar", "ident.Value not {}. got={}", "foobar", ident.value);
                                assert_eq!(ident.token_literal(), "foobar", "ident.TokenLiteral not {}. got={}", "foobar", ident.token_literal());
                            }
                            None => panic!("Expression is not an Identifier."),
                        }
                    }
                    None => panic!("Expression is missing in ExpressionStatement."),
                }
            }
            None => panic!("Statement is not an ExpressionStatement."),
        }
    }

    #[test]
    fn test_string() {
        let program = ast::Program {
            statements: vec![
                Box::new(ast::LetStatement {
                    token: Token {
                        token_type: TokenType::Let,
                        literal: "let".to_string(),
                    },
                    name: ast::Identifier {
                        token: Token {
                            token_type: TokenType::Ident,
                            literal: "myVar".to_string(),
                        },
                        value: "myVar".to_string(),
                    },
                    value: Some(Box::new(ast::Identifier {
                        token: Token {
                            token_type: TokenType::Ident,
                            literal: "anotherVar".to_string(),
                        },
                        value: "anotherVar".to_string(),
                    })),
                    // ... other fields ...
                })
            ],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    #[test]
    fn test_boolean_expression_parsing() {
        let tests = vec![
            ("true".to_string(), "true".to_string()),
            ("false".to_string(), "false".to_string()),
            ("3 > 5 == false".to_string(), "((3 > 5) == false)".to_string()),
            ("3 < 5 == true".to_string(), "((3 < 5) == true)".to_string()),
        ];

        for (input, expected) in tests {
            let l = Lexer::new(&input);
            let mut parser = Parser::new(l);
            let program = parser.parse_program();
            let actual = program.to_string();

            assert_eq!(expected, actual, "Expected {}, but got {}", expected, actual);
        }
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1, "program.Statements does not contain {} statements. got={}", 1, program.statements.len());
        match program.statements[0].as_any().downcast_ref::<ast::ExpressionStatement>() {
            Some(expr_stmt) => {
                match &expr_stmt.expression {
                    Some(expr) => {
                        match expr.as_any().downcast_ref::<ast::IndexExpression>() {
                            Some(index_expr) => {
                                test_identifier(index_expr.left.as_ref().unwrap().as_ref(), "myArray");
                                test_infix_expression(index_expr.index.as_ref().unwrap().as_ref(), &1 as &dyn Any, "+", &1 as &dyn Any);
                            }
                            None => panic!("Expression is not an IndexExpression."),
                        }
                    }
                    None => panic!("Expression is missing in ExpressionStatement."),
                }
            }
            None => panic!("Statement is not an ExpressionStatement."),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);  // Assuming you have a check_parser_errors function similar to Go's checkParserErrors

        assert_eq!(program.statements.len(), 1, "program has not enough statements. got={}", program.statements.len());

        match program.statements[0].as_any().downcast_ref::<ast::ExpressionStatement>() {
            Some(expr_stmt) => {
                // Checking if the expression is present
                match &expr_stmt.expression {
                    Some(expr) => {
                        match expr.as_any().downcast_ref::<ast::IntegerLiteral>() {
                            Some(literal) => {
                                assert_eq!(literal.value, 5, "literal.Value not {}. got={}", 5, literal.value);
                                assert_eq!(literal.token_literal(), "5", "literal.TokenLiteral not {}. got={}", "5", literal.token_literal());
                            }
                            None => panic!("Expression is not an Identifier."),
                        }
                    }
                    None => panic!("Expression is missing in ExpressionStatement."),
                }
            }
            None => panic!("Statement is not an ExpressionStatement."),
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            ("!5;", "!", 5),
            ("-15;", "-", 15),
        ];

        for tt in &prefix_tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1, "program.Statements does not contain {} statements. got={}", 1, program.statements.len());

            match program.statements[0].as_any().downcast_ref::<ast::ExpressionStatement>() {
                Some(expr_stmt) => {
                    // Checking if the expression is present
                    match &expr_stmt.expression {
                        Some(expr) => {
                            match expr.as_any().downcast_ref::<ast::PrefixExpression>() {
                                Some(op_expr) => {
                                    assert_eq!(op_expr.operator, tt.1, "exp.Operator is not '{}'. got={}", tt.1, op_expr.operator);
                                    match &op_expr.right {
                                        Some(right) => {
                                            if !test_integer_literal(right.as_ref(), tt.2) {
                                                return;
                                            }
                                        }
                                        _ => panic!("Expression is missing in ExpressionStatement."),
                                    }
                                }
                                None => panic!("Expression is not an Identifier."),
                            }
                        }
                        None => panic!("Expression is missing in ExpressionStatement."),
                    }
                }
                None => panic!("Statement is not an ExpressionStatement."),
            }
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1, "Expected program.Statements to contain 1 statement. got={}", program.statements.len());
        match program.statements[0].as_any().downcast_ref::<ast::ExpressionStatement>() {
            Some(stmt_expr) => {
                let func = stmt_expr.expression.as_ref().unwrap().as_any().downcast_ref::<ast::FunctionLiteral>().unwrap();
                assert_eq!(func.parameters.len(), 2, "Expected func.Parameters to contain 2 parameters. got={}", func.parameters.len());

                test_literal_expression(&func.parameters[0], &("x".to_string()) as &dyn Any);
                test_literal_expression(&func.parameters[1], &("y".to_string()) as &dyn Any);

                assert_eq!(func.body.as_ref().unwrap().statements.len(), 1, "Expected func.Body.Statements to contain 1 statement. got={}", func.body.as_ref().unwrap().statements.len());

                match func.body.as_ref().unwrap().statements[0].as_any().downcast_ref::<ast::ExpressionStatement>() {
                    Some(expr_stmt) => {
                        test_infix_expression(expr_stmt.expression.as_ref().unwrap().as_ref(), &("x".to_string()) as &dyn Any, "+", &("y".to_string()) as &dyn Any);
                    }
                    _ => panic!("Expected func.Body.Statements[0] to be ast.ExpressionStatement."),
                }
            }
            _ => panic!("Expected program.Statements[0] to be ast.ExpressionStatement."),
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1, "program.Statements does not contain {} statements. got={}", 1, program.statements.len());

        match program.statements[0].as_any().downcast_ref::<ast::ExpressionStatement>() {
            Some(stmt_expr) => {
                let call_expr = stmt_expr.expression.as_ref().unwrap().as_any().downcast_ref::<ast::CallExpression>().unwrap();
                test_identifier(call_expr.function.as_ref().unwrap().as_ref(), "add");
                assert_eq!(call_expr.arguments.len(), 3, "Expected callExpr.Arguments to contain 3 arguments. got={}", call_expr.arguments.len());
                test_literal_expression(call_expr.arguments[0].as_ref(), &1 as &dyn Any);
                test_infix_expression(call_expr.arguments[1].as_ref(), &2 as &dyn Any, "*", &3 as &dyn Any);
                test_infix_expression(call_expr.arguments[2].as_ref(), &4 as &dyn Any, "+", &5 as &dyn Any);
            }
            _ => panic!("Expected program.Statements[0] to be ast.ExpressionStatement."),
        }
    }

    #[test]
    fn test_let_statements2() {
        let tests = vec![
            ("let x = 5;", "x"),
            ("let y = true;", "y"),
            ("let foobar = y;", "foobar"),
        ];


        let tests_expected_expr: Vec<Box<dyn ast::Expression>> = vec![
            Box::new(ast::Identifier {
                token: Token {
                    token_type: TokenType::Ident,
                    literal: "x".to_string(),
                },
                value: "x".to_string(),
            }),
            Box::new(ast::Boolean {
                token: Token {
                    token_type: TokenType::True,
                    literal: "true".to_string(),
                },
                value: true,
            }),
            Box::new(ast::Identifier {
                token: Token {
                    token_type: TokenType::Ident,
                    literal: "y".to_string(),
                },
                value: "y".to_string(),
            }),
        ];


        for (tt, expr) in tests.iter().zip(tests_expected_expr.iter()) {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1,
                       "program.statements does not contain 1 statements. got={}", program.statements.len());

            let stmt = &program.statements[0];
            test_let_statement(stmt, tt.1);

            if !test_literal_expression(stmt.as_any().downcast_ref::<ast::LetStatement>().unwrap().value.as_ref().unwrap().as_ref(), expr) {
                return;
            }
        }
    }


    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1, "Expected program.Statements to contain 1 statement. got={}", program.statements.len());

        match program.statements[0].as_any().downcast_ref::<ast::ExpressionStatement>() {
            Some(expr_stmt) => {
                match &expr_stmt.expression {
                    Some(expr) => {
                        match expr.as_any().downcast_ref::<ast::IfExpression>() {
                            Some(if_expr) => {
                                test_infix_expression(if_expr.condition.as_ref().unwrap().as_ref(), &("x".to_string()) as &dyn Any, "<", &("y".to_string()) as &dyn Any);
                                assert_eq!(if_expr.consequence.as_ref().unwrap().statements.len(), 1, "Expected ifExpr.Consequence.Statements to contain 1 statement. got={}", if_expr.consequence.as_ref().unwrap().statements.len());
                                let statement = if_expr.consequence.as_ref().unwrap().statements[0].as_any();
                                match statement.downcast_ref::<ast::ExpressionStatement>() {
                                    Some(ident) => {
                                        test_identifier(ident.expression.as_ref().unwrap().as_ref(), "x");
                                    }
                                    _ => panic!("Expected ifStmt.Consequence.Statements[0] to be ast.ExpressionStatement."),
                                }
                            }
                            None => panic!("Expression is not an IfExpression."),
                        }
                    }
                    None => panic!("Expression is missing in ExpressionStatement."),
                }
            }
            None => panic!("Statement is not an ExpressionStatement."),
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for tt in &infix_tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1,
                       "program.Statements does not contain {} statements. got={}",
                       1, program.statements.len());

            match program.statements[0].as_any().downcast_ref::<ast::ExpressionStatement>() {
                Some(expr_stmt) => {
                    match &expr_stmt.expression {
                        Some(expr) => {
                            match expr.as_any().downcast_ref::<ast::InfixExpression>() {
                                Some(infix_expr) => {
                                    assert!(test_integer_literal(infix_expr.left.as_ref().unwrap().as_ref(), tt.1),
                                            "exp.Left test failed for operator: {}", tt.2);
                                    assert_eq!(infix_expr.operator, tt.2,
                                               "exp.Operator is not '{}'. got={}", tt.2, infix_expr.operator);
                                    assert!(test_integer_literal(infix_expr.right.as_ref().unwrap().as_ref(), tt.3),
                                            "exp.Right test failed for operator: {}", tt.2);
                                }
                                None => panic!("Expression is not an InfixExpression."),
                            }
                        }
                        None => panic!("Expression is missing in ExpressionStatement."),
                    }
                }
                None => panic!("Statement is not an ExpressionStatement."),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            ("add(a * b[2], b[1], 2 * [1, 2][1])", "(add((a * (b[2])), (b[1]), (2 * ([1, 2][1]))))"),
        ];

        for tt in tests.iter() {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            let actual = program.to_string();
            assert_eq!(tt.1, actual, "For input {}: expected {}, got {}", tt.0, tt.1, actual);
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world""#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_parser_errors(&p);

        let stmt = &program.statements[0];
        if let Some(expression_stmt) = stmt.as_any().downcast_ref::<ast::ExpressionStatement>() {
            if let Some(literal) = expression_stmt.expression.as_ref().unwrap().as_any().downcast_ref::<ast::StringLiteral>() {
                assert_eq!(literal.value, "hello world", "literal.Value not \"hello world\". got={}", literal.value);
            } else {
                panic!("exp not StringLiteral. got={:?}", expression_stmt.expression);
            }
        } else {
            panic!("stmt not ExpressionStatement. got={:?}", stmt);
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = &program.statements[0];
        if let Some(expression_stmt) = stmt.as_any().downcast_ref::<ast::ExpressionStatement>() {
            if let Some(array) = expression_stmt.expression.as_ref().unwrap().as_any().downcast_ref::<ast::ArrayLiteral>() {
                assert_eq!(array.elements.len(), 3, "len(array.Elements) not 3. got={}", array.elements.len());
                test_integer_literal(array.elements[0].as_ref(), 1);
                test_infix_expression(array.elements[1].as_ref(), &2 as &dyn Any, "*", &2 as &dyn Any);
                test_infix_expression(array.elements[2].as_ref(), &3 as &dyn Any, "+", &3 as &dyn Any);
            } else {
                panic!("exp not ArrayLiteral. got={:?}", expression_stmt.expression);
            }
        } else {
            panic!("stmt not ExpressionStatement. got={:?}", stmt);
        }
    }


    fn test_integer_literal(exp: &dyn ast::Expression, expected_value: i64) -> bool {
        if let Some(int_lit) = exp.as_any().downcast_ref::<ast::IntegerLiteral>() {
            if int_lit.value != expected_value {
                eprintln!("int_lit.Value not {}. got={}", expected_value, int_lit.value);
            }
            if int_lit.token_literal() != format!("{}", expected_value) {
                eprintln!("int_lit.TokenLiteral not {}. got={}", expected_value, int_lit.token_literal());
            }
            true
        } else {
            eprintln!("exp not ast.IntegerLiteral.");
            false
        }
    }

    fn test_identifier(exp: &dyn ast::Expression, value: &str) -> bool {
        if let Some(ident) = exp.as_any().downcast_ref::<ast::Identifier>() {
            if ident.value != value {
                eprintln!("ident.value not {}. got={}", value, ident.value);
                return false;
            }
            if ident.token_literal() != value {
                eprintln!("ident.token_literal not {}. got={}", value, ident.token_literal());
                return false;
            }
            true
        } else {
            eprintln!("exp is not an Identifier.");
            false
        }
    }

    fn test_literal_expression(exp: &dyn ast::Expression, expected: &dyn Any) -> bool {
        if let Some(v) = expected.downcast_ref::<i32>() {
            return test_integer_literal(exp, *v as i64);
        }
        if let Some(v) = expected.downcast_ref::<i64>() {
            return test_integer_literal(exp, *v);
        }
        if let Some(v) = expected.downcast_ref::<String>() {
            return test_identifier(exp, v);
        }

        eprintln!("type of exp not handled.");
        false
    }

    fn test_infix_expression(exp: &dyn ast::Expression, left: &dyn Any, operator: &str, right: &dyn Any) -> bool {
        if let Some(op_exp) = exp.as_any().downcast_ref::<ast::InfixExpression>() {
            if !test_literal_expression(op_exp.left.as_ref().unwrap().as_ref(), left) {
                return false;
            }
            if op_exp.operator != operator {
                eprintln!("exp.Operator is not '{}'. got={}", operator, op_exp.operator);
                return false;
            }
            if !test_literal_expression(op_exp.right.as_ref().unwrap().as_ref(), right) {
                return false;
            }
            true
        } else {
            eprintln!("exp is not an InfixExpression.");
            false
        }
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let expected: HashMap<String, i64> = [
            ("one".to_string(), 1),
            ("two".to_string(), 2),
            ("three".to_string(), 3),
        ].iter().cloned().collect();

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_parser_errors(&p);


        let stmt = &program.statements[0];

        if let Some(expression_stmt) = stmt.as_any().downcast_ref::<ast::ExpressionStatement>() {
            if let Some(hash_literal) = expression_stmt.expression.as_ref().unwrap().as_any().downcast_ref::<ast::HashLiteral>() {
                for (key, value) in expected.iter() {
                    let val = hash_literal.pairs.get(ast::StringLiteral::new(key.clone()).clone_box().as_ref()).unwrap();
                    test_integer_literal(val.as_ref(), *value);
                }
            } else {
                panic!("exp not HashLiteral. got={:?}", expression_stmt.expression);
            }
        } else {
            panic!("stmt not ExpressionStatement. got={:?}", stmt);
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if let Some(statement) = program.statements.get(0) {
            if let Some(expression_stmt) = statement.as_any().downcast_ref::<ast::ExpressionStatement>() {
                if let Some(hash_literal) = expression_stmt.expression.as_ref().unwrap().as_any().downcast_ref::<ast::HashLiteral>() {
                    assert_eq!(hash_literal.pairs.len(), 0, "hashLiteral.Pairs has wrong length. got={}", hash_literal.pairs.len());
                } else {
                    panic!("exp not HashLiteral. got={:?}", expression_stmt.expression);
                }
            } else {
                panic!("stmt not ExpressionStatement. got={:?}", statement);
            }
        } else {
            panic!("program.Statements does not contain {} statements. got={}", 1, program.statements.len());
        }
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let tests = [
            ("one", (0, "+", 1)),
            ("two", (10, "-", 8)),
            ("three", (15, "/", 5))
        ];

        if let Some(statement) = program.statements.get(0) {
            if let Some(expression_stmt) = statement.as_any().downcast_ref::<ast::ExpressionStatement>() {
                if let Some(hash_literal) = expression_stmt.expression.as_ref().unwrap().as_any().downcast_ref::<ast::HashLiteral>() {
                    assert_eq!(hash_literal.pairs.len(), 3, "hashLiteral.Pairs has wrong length. got={}", hash_literal.pairs.len());

                    for (key, (left, op, right)) in tests.iter() {
                        let val = hash_literal.pairs.get(ast::StringLiteral::new(key.to_string()).clone_box().as_ref()).unwrap();
                        test_infix_expression(val.as_ref(), left, op, right);
                    }
                } else {
                    panic!("exp not HashLiteral. got={:?}", expression_stmt.expression);
                }
            } else {
                panic!("stmt not ExpressionStatement. got={:?}", statement);
            }
        } else {
            panic!("program.Statements does not contain {} statements. got={}", 1, program.statements.len());
        }
    }





    fn check_parser_errors(p: &Parser) {
        let errors = p.errors();
        if errors.is_empty() {
            return;
        }

        eprintln!("parser has {} errors", errors.len());
        for msg in errors.iter() {
            eprintln!("parser error: {:?}", msg);
        }

        panic!("parser has {} errors", errors.len());
    }
}

