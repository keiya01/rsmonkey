use std::fmt;
use std::cmp::PartialEq;
use std::rc::Rc;
use std::cell::RefCell;

use crate::ast::ident::Identifier;
use crate::ast::stmt::BlockStatement;
use super::environment::Environment;


#[derive(Debug, PartialEq, Clone)]
pub enum Object {
  Integer(Integer),
  Boolean(Boolean),
  Return(Return),
  Func(Func),
  Error(Error),
  Null,
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Object::Integer(val) => write!(f, "{}", val),
      Object::Boolean(val) => write!(f, "{}", val),
      Object::Return(val) => write!(f, "{}", val),
      Object::Func(val) => write!(f, "{}", val),
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

    let mut iter = self.args.iter();    
    let mut next = iter.next();
    while let Some(val) = next {
      next = iter.next();
      write!(f, "{}", val)?;
      if let Some(_) = next {
        write!(f, ", ")?;
      }
    }

    write!(f, ") {}", &self.body)?;
    Ok(())
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
