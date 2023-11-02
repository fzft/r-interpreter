use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display};
use std::hash::Hash;
use crate::token;

pub trait Node {
    fn token_literal(&self) -> String;

    fn to_string(&self) -> String;

    fn as_any(&self) -> &dyn Any;
}


pub trait Statement: Node {
    fn statement_node(&self);

    fn clone_box(&self) -> Box<dyn Statement>;
}

impl Clone for Box<dyn Statement> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

impl std::fmt::Debug for dyn Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Statement")
            .field("token_literal", &self.token_literal())
            .field("to_string", &self.to_string())
            .finish()
    }
}

pub trait Expression: Node {
    fn expression_node(&self);

    fn clone_box(&self) -> Box<dyn Expression>;
}

impl std::fmt::Debug for dyn Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Expression")
            .field("token_literal", &self.token_literal())
            .field("to_string", &self.to_string())
            .finish()
    }
}

impl Hash for dyn Expression {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.token_literal().hash(state);
        self.to_string().hash(state);
    }
}

impl Eq for dyn Expression {}

impl PartialEq for dyn Expression {
    fn eq(&self, other: &Self) -> bool {
        self.token_literal() == other.token_literal() && self.to_string() == other.to_string()
    }
}


impl Clone for Box<dyn Expression> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}


pub struct Program {
    pub(crate) statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if let Some(first_statement) = self.statements.first() {
            first_statement.token_literal()
        } else {
            "".to_string()
        }
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        for s in &self.statements {
            out.push_str(&s.to_string());
        }
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct LetStatement {
    pub(crate) token: token::Token,
    pub(crate) name: Identifier,
    pub(crate) value: Option<Box<dyn Expression>>,
}

impl LetStatement {
    pub fn new(token: token::Token, name: Identifier, value: Option<Box<dyn Expression>>) -> Self {
        LetStatement {
            token,
            name,
            value,
        }
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}

    fn clone_box(&self) -> Box<dyn Statement> {
        Box::new(LetStatement {
            token: self.token.clone(),
            name: self.name.clone(),
            value: self.value.clone(),
        })
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push_str(" ");
        out.push_str(&self.name.to_string());
        out.push_str(" = ");
        if let Some(ref value) = self.value {
            out.push_str(&value.to_string());
        }
        out.push_str(";");
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub(crate) token: token::Token,
    // the first token of the expression
    pub(crate) expression: Option<Box<dyn Expression>>,
}


impl ExpressionStatement {
    pub fn new(token: token::Token, expression: Option<Box<dyn Expression>>) -> Self {
        ExpressionStatement {
            token,
            expression,
        }
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}

    fn clone_box(&self) -> Box<dyn Statement> {
        Box::new(ExpressionStatement {
            token: self.token.clone(),
            expression: self.expression.clone(),
        })
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        if let Some(ref expression) = self.expression {
            expression.to_string()
        } else {
            "".to_string()
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct ReturnStatement {
    pub(crate) token: token::Token,
    // the 'return' token
    pub(crate) return_value: Option<Box<dyn Expression>>,
}

impl ReturnStatement {
    pub fn new(token: token::Token, return_value: Option<Box<dyn Expression>>) -> Self {
        ReturnStatement {
            token,
            return_value,
        }
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}

    fn clone_box(&self) -> Box<dyn Statement> {
        Box::new(ReturnStatement {
            token: self.token.clone(),
            return_value: self.return_value.clone(),
        })
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push_str(" ");
        if let Some(ref value) = self.return_value {
            out.push_str(&value.to_string());
        }
        out.push_str(";");
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}


#[derive(Debug)]
pub struct BlockStatement {
    pub(crate) token: token::Token,
    pub(crate) statements: Vec<Box<dyn Statement>>,
}

impl BlockStatement {
    pub fn new(token: token::Token, statements: Vec<Box<dyn Statement>>) -> Self {
        BlockStatement {
            token,
            statements,
        }
    }
}


impl Statement for BlockStatement {
    fn statement_node(&self) {}

    fn clone_box(&self) -> Box<dyn Statement> {
        Box::new(BlockStatement {
            token: self.token.clone(),
            statements: self.statements.iter().map(|s| (*s).clone()).collect(),
        })
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        for s in &self.statements {
            out.push_str(&s.to_string());
        }
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Clone for BlockStatement {
    fn clone(&self) -> Self {
        BlockStatement {
            token: self.token.clone(),
            statements: self.statements.iter().map(|s| (*s).clone()).collect(),
        }
    }
}


#[derive(Debug, Clone)]
pub struct Identifier {
    pub(crate) token: token::Token,
    pub(crate) value: String,
}

impl Identifier {
    pub fn new(token: token::Token, value: String) -> Self {
        Identifier {
            token,
            value,
        }
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(Identifier {
            token: self.token.clone(),
            value: self.value.clone(),
        })
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Default for Identifier {
    fn default() -> Self {
        Identifier {
            token: token::Token::default(),
            value: "".to_string(),
        }
    }
}


pub struct PrefixExpression {
    pub token: token::Token,
    // Assuming you have a token module
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,  // Assuming you have an Expression trait
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push('(');
        out.push_str(&self.operator);
        if let Some(ref right) = self.right {
            out.push_str(&right.to_string());
        }
        out.push(')');
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(PrefixExpression {
            token: self.token.clone(),
            operator: self.operator.clone(),
            right: self.right.clone(),
        })
    }
}

pub struct IfExpression {
    pub(crate) token: token::Token,
    pub(crate) condition: Option<Box<dyn Expression>>,
    pub(crate) consequence: Option<BlockStatement>,
    pub(crate) alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push_str("if");
        if let Some(ref condition) = self.condition {
            out.push_str(&condition.to_string());
        }
        out.push_str(" ");
        if let Some(ref consequence) = self.consequence {
            out.push_str(&consequence.to_string());
        }
        if let Some(ref alternative) = self.alternative {
            out.push_str("else ");
            out.push_str(&alternative.to_string());
        }
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IfExpression {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(IfExpression {
            token: self.token.clone(),
            condition: self.condition.clone(),
            consequence: self.consequence.clone(),
            alternative: self.alternative.clone(),
        })
    }
}

impl Default for IfExpression {
    fn default() -> Self {
        IfExpression {
            token: token::Token::default(),
            condition: None,
            consequence: None,
            alternative: None,
        }
    }
}


pub struct InfixExpression {
    pub(crate) token: token::Token,
    pub(crate) left: Option<Box<dyn Expression>>,
    pub(crate) operator: String,
    pub(crate) right: Option<Box<dyn Expression>>,
}


impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push('(');
        if let Some(ref left) = self.left {
            out.push_str(&left.to_string());
        }
        out.push(' ');
        out.push_str(&self.operator);
        out.push(' ');
        if let Some(ref right) = self.right {
            out.push_str(&right.to_string());
        }
        out.push(')');
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(InfixExpression {
            token: self.token.clone(),
            left: self.left.clone(),
            operator: self.operator.clone(),
            right: self.right.clone(),
        })
    }
}

pub struct CallExpression {
    pub(crate) token: token::Token,
    pub(crate) function: Option<Box<dyn Expression>>,
    pub(crate) arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        let mut args = Vec::new();
        for a in &self.arguments {
            args.push(a.to_string());
        }
        out.push('(');
        if let Some(ref function) = self.function {
            out.push_str(&function.to_string());
        }
        out.push('(');
        out.push_str(&args.join(", "));
        out.push(')');
        out.push(')');
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for CallExpression {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(CallExpression {
            token: self.token.clone(),
            function: self.function.clone(),
            arguments: self.arguments.iter().map(|a| (*a).clone()).collect(),
        })
    }
}


pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: i64,
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(IntegerLiteral {
            token: self.token.clone(),
            value: self.value,
        })
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Default for IntegerLiteral {
    fn default() -> Self {
        IntegerLiteral {
            token: token::Token::default(),
            value: 0,
        }
    }
}

pub struct StringLiteral {
    pub(crate) token: token::Token,
    pub(crate) value: String,
}

impl Expression for StringLiteral {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(StringLiteral {
            token: self.token.clone(),
            value: self.value.clone(),
        })
    }
}

impl Node for StringLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Default for StringLiteral {
    fn default() -> Self {
        StringLiteral {
            token: token::Token::default(),
            value: "".to_string(),
        }
    }
}

impl StringLiteral {
    pub fn new(value: String) -> Self {
        let token = token::Token::new(token::TokenType::String, value.clone());
        StringLiteral {
            value,
            token
        }
    }
}

pub struct ArrayLiteral {
    pub(crate) token: token::Token,
    pub(crate) elements: Vec<Box<dyn Expression>>,
}

impl Expression for ArrayLiteral {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(ArrayLiteral {
            token: self.token.clone(),
            elements: self.elements.iter().map(|e| (*e).clone()).collect(),
        })
    }
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        let mut elements = Vec::new();
        for e in &self.elements {
            elements.push(e.to_string());
        }
        out.push('[');
        out.push_str(&elements.join(", "));
        out.push(']');
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Default for ArrayLiteral {
    fn default() -> Self {
        ArrayLiteral {
            token: token::Token::default(),
            elements: Vec::new(),
        }
    }
}


pub struct IndexExpression {
    pub(crate) token: token::Token,
    pub(crate) left: Option<Box<dyn Expression>>,
    pub(crate) index: Option<Box<dyn Expression>>,
}

impl Expression for IndexExpression {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(IndexExpression {
            token: self.token.clone(),
            left: self.left.clone(),
            index: self.index.clone(),
        })
    }
}

impl Node for IndexExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push('(');
        if let Some(ref left) = self.left {
            out.push_str(&left.to_string());
        }
        out.push('[');
        if let Some(ref index) = self.index {
            out.push_str(&index.to_string());
        }
        out.push(']');
        out.push(')');
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Boolean {
    pub(crate) token: token::Token,
    pub(crate) value: bool,
}

impl Expression for Boolean {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(Boolean {
            token: self.token.clone(),
            value: self.value,
        })
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Default for Boolean {
    fn default() -> Self {
        Boolean {
            token: token::Token::default(),
            value: false,
        }
    }
}

pub struct FunctionLiteral {
    pub(crate) token: token::Token,
    pub(crate) parameters: Vec<Identifier>,
    pub(crate) body: Option<BlockStatement>,
}

impl Expression for FunctionLiteral {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(FunctionLiteral {
            token: self.token.clone(),
            parameters: self.parameters.iter().map(|p| (*p).clone()).collect(),
            body: self.body.clone(),
        })
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        let mut params = Vec::new();
        for p in &self.parameters {
            params.push(p.to_string());
        }
        out.push_str(&self.token_literal());
        out.push('(');
        out.push_str(&params.join(", "));
        out.push_str(") ");
        if let Some(ref body) = self.body {
            out.push_str(&body.to_string());
        }
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Default for FunctionLiteral {
    fn default() -> Self {
        FunctionLiteral {
            token: token::Token::default(),
            parameters: Vec::new(),
            body: None,
        }
    }
}


pub struct HashLiteral {
    pub(crate) token: token::Token,
    pub(crate) pairs: HashMap<Box<dyn Expression>, Box<dyn Expression>>
}

impl Expression for HashLiteral {
    fn expression_node(&self) {}

    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(HashLiteral {
            token: self.token.clone(),
            pairs: self.pairs.clone(),
        })
    }
}

impl Node for HashLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        let mut pairs = Vec::new();
        for p in &self.pairs {
            pairs.push(format!("{}: {}", p.0.to_string(), p.1.to_string()));
        }
        out.push('{');
        out.push_str(&pairs.join(", "));
        out.push('}');
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Default for HashLiteral {
    fn default() -> Self {
        HashLiteral {
            token: token::Token::default(),
            pairs: HashMap::new(),
        }
    }
}

