use std::fmt;

use crate::utils;
use super::ident::Identifier;
use super::stmt::BlockStatement;
use super::expr::Expression;

#[derive(Debug, Clone)]
pub enum Literal {
  Integer(Integer),
  Boolean(Boolean),
  Str(Str),
  Array(Array),
  Hash(Hash),
  Func(Func),
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Literal::Integer(int) => write!(f, "{}", int),
      Literal::Boolean(v) => write!(f, "{}", v),
      Literal::Str(v) => write!(f, "{}", v),
      Literal::Array(v) => write!(f, "{}", v),
      Literal::Hash(v) => write!(f, "{}", v),
      Literal::Func(func) => write!(f, "{}", func),
    }
  }
}

#[derive(Debug, Clone)]
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
    write!(f, "{}", &self.value)
  }
}

#[derive(Debug, Clone)]
pub struct Boolean {
  pub value: bool,
}

impl Boolean {
  pub fn new(value: bool) -> Boolean {
    Boolean { value }
  }
}

impl fmt::Display for Boolean {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", &self.value)
  }
}

#[derive(Debug, Clone)]
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
    write!(f, "\"{}\"", &self.value)
  }
}

#[derive(Debug, Clone)]
pub struct Array {
  pub elements: Vec<Expression>,
}

impl Array {
  pub fn new(elements: Vec<Expression>) -> Array {
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
  pub pairs: Vec<(Expression, Expression)>,
}

impl Hash {
  pub fn new(pairs: Vec<(Expression, Expression)>) -> Hash {
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
}

impl Func {
  pub fn new(args: Vec<Identifier>, body: BlockStatement) -> Func {
    Func { args, body }
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
