use std::any::Any;
use std::fmt::{Debug, Display};
use crate::ast;
use crate::ast::Node;
use crate::env::Environment;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, BuiltinFunction> = {
        let mut m = HashMap::new();
        m.insert(
            "len",
            BuiltinFunction::new(|args: Vec<Box<dyn Object>>| -> Box<dyn Object> {
                if args.len() != 1 {
                    return Box::new(Error {
                        message: format!(
                            "wrong number of arguments. got={}, want=1",
                            args.len()
                        ),
                    });
                }
                if let Some(s) = args[0].as_any().downcast_ref::<StringObject>() {
                    return Box::new(Integer {
                        value: s.value.len() as i64,
                    });
                } else if let Some(a) = args[0].as_any().downcast_ref::<ArrayObject>() {
                    return Box::new(Integer {
                        value: a.elements.len() as i64,
                    });
                } else {
                    return Box::new(Error {
                        message: format!(
                            "argument to `len` not supported, got={}",
                            args[0].object_type()
                        ),
                    });
                }
            }),
        );
        m.insert(
            "first",
            BuiltinFunction::new(|args: Vec<Box<dyn Object>>| -> Box<dyn Object> {
                if args.len() != 1 {
                    return Box::new(Error {
                        message: format!(
                            "wrong number of arguments. got={}, want=1",
                            args.len()
                        ),
                    });
                }
                if let Some(a) = args[0].as_any().downcast_ref::<ArrayObject>() {
                    if a.elements.len() > 0 {
                        return a.elements[0].clone();
                    } else {
                        return Box::new(Null);
                    }
                } else {
                    return Box::new(Error {
                        message: format!(
                            "argument to `first` must be ARRAY, got {}",
                            args[0].object_type()
                        ),
                    });
                }
            }),
        );
        m.insert(
            "last",
            BuiltinFunction::new(|args: Vec<Box<dyn Object>>| -> Box<dyn Object> {
                if args.len() != 1 {
                    return Box::new(Error {
                        message: format!(
                            "wrong number of arguments. got={}, want=1",
                            args.len()
                        ),
                    });
                }
                if let Some(a) = args[0].as_any().downcast_ref::<ArrayObject>() {
                    let length = a.elements.len();
                    if length > 0 {
                        return a.elements[length - 1].clone();
                    } else {
                        return Box::new(Null);
                    }
                } else {
                    return Box::new(Error {
                        message: format!(
                            "argument to `last` must be ARRAY, got {}",
                            args[0].object_type()
                        ),
                    });
                }
            }),
        );
        m.insert(
            "rest",
            BuiltinFunction::new(|args: Vec<Box<dyn Object>>| -> Box<dyn Object> {
                if args.len() != 1 {
                    return Box::new(Error {
                        message: format!(
                            "wrong number of arguments. got={}, want=1",
                            args.len()
                        ),
                    });
                }
                if let Some(a) = args[0].as_any().downcast_ref::<ArrayObject>() {
                    let length = a.elements.len();
                    if length > 0 {
                        let mut new_elements = Vec::new();
                        for i in 1..length {
                            new_elements.push(a.elements[i].clone());
                        }
                        return Box::new(ArrayObject { elements: new_elements });
                    } else {
                        return Box::new(Null);
                    }
                } else {
                    return Box::new(Error {
                        message: format!(
                            "argument to `rest` must be ARRAY, got {}",
                            args[0].object_type()
                        ),
                    });
                }
            }),
        );
        m.insert(
            "push",
            BuiltinFunction::new(|args: Vec<Box<dyn Object>>| -> Box<dyn Object> {
                if args.len() != 2 {
                    return Box::new(Error {
                        message: format!(
                            "wrong number of arguments. got={}, want=2",
                            args.len()
                        ),
                    });
                }
                if let Some(a) = args[0].as_any().downcast_ref::<ArrayObject>() {
                    let mut new_elements = Vec::new();
                    for e in &a.elements {
                        new_elements.push(e.clone());
                    }
                    new_elements.push(args[1].clone());
                    return Box::new(ArrayObject { elements: new_elements });
                } else {
                    return Box::new(Error {
                        message: format!(
                            "argument to `push` must be ARRAY, got {}",
                            args[0].object_type()
                        ),
                    });
                }
            }),
        );
        m.insert(
            "print",
            BuiltinFunction::new(|args: Vec<Box<dyn Object>>| -> Box<dyn Object> {
                for arg in args {
                    println!("{}", arg.inspect());
                }
                Box::new(Blank)
            }),
        );
        m
    };
}

#[derive(Debug)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    String,
    Error,
    Blank,
    Builtin,
    Function,
    Array,
    Hash,
}

impl Hash for ObjectType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::Integer => write!(f, "INTEGER"),
            ObjectType::Boolean => write!(f, "BOOLEAN"),
            ObjectType::Null => write!(f, "NULL"),
            ObjectType::String => write!(f, "STRING"),
            ObjectType::Error => write!(f, "ERROR"),
            ObjectType::Blank => write!(f, "BLANK"),
            ObjectType::Builtin => write!(f, "BUILTIN"),
            ObjectType::Function => write!(f, "FUNCTION"),
            ObjectType::Array => write!(f, "ARRAY"),
            ObjectType::Hash => write!(f, "HASH"),
        }
    }
}

pub trait Object: Debug {
    fn object_type(&self) -> ObjectType;

    fn inspect(&self) -> String;

    fn as_any(&self) -> &dyn Any;

    fn clone_box(&self) -> Box<dyn Object>;

    fn hash_key(&self) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.object_type().hash(&mut hasher);
        self.inspect().hash(&mut hasher);
        hasher.finish()
    }
}


impl Clone for Box<dyn Object> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}


#[derive(Debug)]
pub struct Integer {
    pub(crate) value: i64,
}

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Integer { value: self.value })
    }
}

impl Integer {
    pub fn new(value: i64) -> Box<dyn Object> {
        Box::new(Integer { value })
    }
}


#[derive(Debug)]
pub struct Boolean {
    pub(crate) value: bool,
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Boolean { value: self.value })
    }
}

impl Boolean {
    pub fn new(value: bool) -> Box<dyn Object> {
        Box::new(Boolean { value })
    }
}

#[derive(Debug)]
pub struct Null;

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Null)
    }
}


pub struct ReturnValue {
    pub(crate) value: Box<dyn Object>,
}

impl Object for ReturnValue {
    fn object_type(&self) -> ObjectType {
        self.value.object_type()
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(ReturnValue { value: self.value.clone() })
    }
}

impl Clone for ReturnValue {
    fn clone(&self) -> Self {
        ReturnValue { value: self.value.clone() }
    }
}

impl Debug for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReturnValue")
            .field("value", &self.value.inspect())
            .finish()
    }
}

pub struct Error {
    pub(crate) message: String,
}

impl Object for Error {
    fn object_type(&self) -> ObjectType {
        ObjectType::Error
    }

    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Error { message: self.message.clone() })
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Error")
            .field("message", &self.message)
            .finish()
    }
}

#[derive(Debug)]
pub struct Blank;

impl Object for Blank {
    fn object_type(&self) -> ObjectType {
        ObjectType::Blank
    }

    fn inspect(&self) -> String {
        "".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Blank)
    }
}

#[derive(Debug)]
pub struct Function {
    pub(crate) parameters: Vec<ast::Identifier>,
    pub(crate) body: Box<ast::BlockStatement>,
    pub(crate) env: Environment,
}

impl Object for Function {
    fn object_type(&self) -> ObjectType {
        ObjectType::Function
    }

    fn inspect(&self) -> String {
        let mut out = String::new();
        let mut params = Vec::new();
        for p in &self.parameters {
            params.push(p.to_string());
        }
        out.push_str("fn");
        out.push_str("(");
        out.push_str(&params.join(", "));
        out.push_str(") {\n");
        out.push_str(&self.body.to_string());
        out.push_str("\n}");
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Function {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            env: self.env.clone(),
        })
    }
}


#[derive(Debug)]
pub struct StringObject {
    pub(crate) value: String,
}

impl Object for StringObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::String
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(StringObject { value: self.value.clone() })
    }
}

impl StringObject {
    pub fn new(value: &str) -> Box<dyn Object> {
        Box::new(StringObject {
            value: value.to_string(),
        })
    }
}


#[derive(Debug)]
pub struct BuiltinFunction {
    pub(crate) func: fn(Vec<Box<dyn Object>>) -> Box<dyn Object>,
}

impl Object for BuiltinFunction {
    fn object_type(&self) -> ObjectType {
        ObjectType::Builtin
    }

    fn inspect(&self) -> String {
        "builtin function".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(BuiltinFunction { func: self.func })
    }
}

impl BuiltinFunction {
    pub fn new(func: fn(Vec<Box<dyn Object>>) -> Box<dyn Object>) -> Self {
        BuiltinFunction { func }
    }
}

#[derive(Debug)]
pub struct ArrayObject {
    pub(crate) elements: Vec<Box<dyn Object>>,
}

impl Object for ArrayObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Array
    }

    fn inspect(&self) -> String {
        let mut out = String::new();
        let mut elements = Vec::new();
        for e in &self.elements {
            elements.push(e.inspect());
        }
        out.push_str("[");
        out.push_str(&elements.join(", "));
        out.push_str("]");
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        let mut elements = Vec::new();
        for e in &self.elements {
            elements.push(e.clone());
        }
        Box::new(ArrayObject { elements })
    }
}

#[derive(Debug)]
pub struct HashPair {
    pub(crate) key: Box<dyn Object>,
    pub(crate) value: Box<dyn Object>,
}


#[derive(Debug)]
pub struct HashObject {
    pub(crate) pairs: HashMap<u64, HashPair>,
}

impl Object for HashObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Hash
    }

    fn inspect(&self) -> String {
        let mut out = String::new();
        let mut pairs = Vec::new();
        for (_, pair) in &self.pairs {
            pairs.push(format!("{}: {}", pair.key.inspect(), pair.value.inspect()));
        }
        out.push_str("{");
        out.push_str(&pairs.join(", "));
        out.push_str("}");
        out
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn Object> {
        let mut pairs = HashMap::new();
        for (k, v) in &self.pairs {
            pairs.insert(*k, HashPair {
                key: v.key.clone(),
                value: v.value.clone(),
            });
        }
        Box::new(HashObject { pairs })
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = StringObject::new("Hello World");
        let hello2 = StringObject::new("Hello World");
        let diff1 = StringObject::new("My name is johnny");
        let diff2 = StringObject::new("My name is johnny");

        assert_eq!(
            hello1.hash_key(), hello2.hash_key(),
            "strings with same content have different hash keys"
        );

        assert_eq!(
            diff1.hash_key(), diff2.hash_key(),
            "strings with same content have different hash keys"
        );

        assert_ne!(
            hello1.hash_key(), diff1.hash_key(),
            "strings with different content have same hash keys"
        );
    }
}





