use std::fmt;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{self, Hasher};

use crate::ast::ident::Identifier;
use crate::ast::stmt::BlockStatement;
use crate::utils;
use super::environment::Environment;

#[derive(Debug, Clone)]
pub enum Object {
  Integer(Integer),
  Boolean(Boolean),
  Str(Str),
  Array(Array),
  Hash(Hash),
  Return(Return),
  Func(Func),
  Builtin(Builtin),
  External(External),
  Error(Error),
  Null,
}

impl Object {
  pub fn is_primitive(&self) -> bool {
    match self {
      Object::Boolean(_)
      | Object::Str(_)
      | Object::Integer(_) => true,
      _ => false,
    }
  }
}

impl PartialEq for Object {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (
        Object::Boolean(val),
        Object::Boolean(other),
      ) => val.value == other.value,
      (
        Object::Str(val),
        Object::Str(other),
      ) => val.value == other.value,
      (
        Object::Integer(val),
        Object::Integer(other),
      ) => val.value == other.value,
      _ => panic!("PartialEq is not implemented for {}", self),
    }
  }
}

impl Eq for Object {}

impl hash::Hash for Object {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      Object::Boolean(val) => val.value.hash(state),
      Object::Str(val) => val.value.hash(state),
      Object::Integer(val) => val.value.hash(state),
      _ => panic!("Hash is not implemented for {}", self),
    }
  }
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Object::Integer(val) => write!(f, "{}", val),
      Object::Boolean(val) => write!(f, "{}", val),
      Object::Str(val) => write!(f, "{}", val),
      Object::Array(val) => write!(f, "{}", val),
      Object::Hash(val) => write!(f, "{}", val),
      Object::Return(val) => write!(f, "{}", val),
      Object::Func(val) => write!(f, "{}", val),
      Object::Builtin(val) => write!(f, "{}", val),
      Object::External(val) => write!(f, "{:?}", val),
      Object::Error(val) => write!(f, "{}", val),
      Object::Null => write!(f, "null"),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer {
  pub value: i64,
}

impl Integer {
  pub fn new(value: i64) -> Integer {
    Integer { value }
  }
}

impl fmt::Display for Integer {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Boolean {
  pub value: bool,
}

impl fmt::Display for Boolean {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Str {
  pub value: String,
}

impl Str {
  pub fn new(value: String) -> Str {
    Str { value }
  }
}

impl fmt::Display for Str {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "\"{}\"", self.value)
  }
}

#[derive(Debug, Clone)]
pub struct Return {
  pub value: Box<Object>,
}

impl Return {
  pub fn new(value: Box<Object>) -> Return {
    Return { value }
  }
}

impl fmt::Display for Return {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

#[derive(Debug, Clone)]
pub struct Array {
  pub elements: Vec<Object>,
}

impl Array {
  pub fn new(elements: Vec<Object>) -> Array {
    Array { elements }
  }
}

impl fmt::Display for Array {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[")?;
    utils::write_object_list(&self.elements, f)?;
    write!(f, "]")
  }
}

#[derive(Debug, Clone)]
pub struct Hash {
  pub pairs: HashMap<Object, Object>,
}

impl Hash {
  pub fn new(pairs: HashMap<Object, Object>) -> Hash {
    Hash { pairs }
  }
}

impl fmt::Display for Hash {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{{")?;
    let mut pairs = vec![];
    for (key, val) in &self.pairs {
      pairs.push(format!("{}: {}", key, val));
    }
    utils::write_object_list(&pairs, f)?;
    write!(f, "}}")
  }
}

#[derive(Debug, Clone)]
pub struct Func {
  pub args: Vec<Identifier>,
  pub body: BlockStatement,
  pub env: Rc<RefCell<Environment>>,
}

impl Func {
  pub fn new(args: Vec<Identifier>, body: BlockStatement, env: Rc<RefCell<Environment>>) -> Func {
    Func { args, body, env }
  }
}

impl fmt::Display for Func {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "fn(")?;

    utils::write_object_list(&self.args, f)?;

    write!(f, ") {}", &self.body)?;
    Ok(())
  }
}

pub type BuiltinFunc = fn(Vec<Object>) -> Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Builtin {
  pub func: BuiltinFunc,
}

impl Builtin {
  pub fn new(func: BuiltinFunc) -> Builtin {
    Builtin { func }
  }
}

impl fmt::Display for Builtin {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[Builtin Function]")
  }
}

pub type ExternalFunc = Rc<RefCell<dyn FnMut(Vec<Object>) -> Object>>;

#[derive(Clone)]
pub struct External {
  pub func: ExternalFunc,
}

impl External {
  pub fn new(func: ExternalFunc) -> External {
    External { func }
  }
}

impl fmt::Debug for External {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "[External Function]")
  }
}

impl PartialEq for External {
  fn eq(&self, _: &Self) -> bool {
      panic!("External struct can not be compered.")
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
  pub value: String,
}

impl Error {
  pub fn new(value: String) -> Error {
    Error { value }
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[Internal Error] {}", self.value)
  }
}
