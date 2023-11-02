use std::collections::HashMap;
use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Box<dyn Object>>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Box<Environment>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Box<dyn Object>> {
        match self.store.get(name) {
            Some(val) => Some(val),
            None => match &self.outer {
                Some(outer) => outer.get(name),
                None => None,
            }
        }
    }

    pub fn set(&mut self, name: String, val: Box<dyn Object>) -> &Box<dyn Object> {
        self.store.insert(name.clone(), val);
        self.store.get(&name).unwrap()
    }
}