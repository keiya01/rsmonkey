use std::fmt;
use std::cmp::PartialEq;

#[derive(Debug, PartialEq)]
pub enum Object {
  Integer(Integer),
  Boolean(Boolean),
  Return(Return),
  Error(Error),
  Null,
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Object::Integer(val) => write!(f, "{}", val),
      Object::Boolean(val) => write!(f, "{}", val),
      Object::Return(val) => write!(f, "{}", val),
      Object::Error(val) => write!(f, "{}", val),
      Object::Null => write!(f, "null"),
    }
  }
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Boolean {
  pub value: bool,
}

impl fmt::Display for Boolean {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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
