use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use super::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
  store: HashMap<String, Object>,
  outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
  pub fn new() -> Rc<RefCell<Environment>> {
    Rc::new(RefCell::new(Environment { store: HashMap::new(), outer: None }))
  }

  pub fn new_enclosed_env(outer: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
    Rc::new(RefCell::new(Environment { store: HashMap::new(), outer: Some(outer) }))
  }

  pub fn get(&self, key: &str) -> Option<Object> {
    match self.store.get(key) {
      Some(val) => Some(val.clone()),
      None => match &self.outer {
        Some(env) => env.borrow().get(key),
        None => None,
      } 
    }
  }

  pub fn set(&mut self, key: &str, val: Object) {
    // TODO: to be immutable
    self.store.insert(key.to_string(), val);
  }
}
