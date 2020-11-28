use std::fmt;
use std::cmp::PartialEq;

#[derive(Debug, PartialEq)]
pub enum Object {
  Integer(Integer),
  Boolean(Boolean),
  Null,
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Object::Integer(val) => write!(f, "{}", val),
      Object::Boolean(val) => write!(f, "{}", val),
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
