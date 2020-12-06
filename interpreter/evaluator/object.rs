use std::fmt;
use std::cmp::PartialEq;
use std::rc::Rc;
use std::cell::RefCell;

use crate::ast::ident::Identifier;
use crate::ast::stmt::BlockStatement;
use crate::utils;
use super::environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
  Integer(Integer),
  Boolean(Boolean),
  Str(Str),
  Array(Array),
  Return(Return),
  Func(Func),
  Builtin(Builtin),
  Error(Error),
  Null,
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Object::Integer(val) => write!(f, "{}", val),
      Object::Boolean(val) => write!(f, "{}", val),
      Object::Str(val) => write!(f, "{}", val),
      Object::Array(val) => write!(f, "{}", val),
      Object::Return(val) => write!(f, "{}", val),
      Object::Func(val) => write!(f, "{}", val),
      Object::Builtin(val) => write!(f, "{:?}", val),
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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
