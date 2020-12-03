use std::cmp::{PartialEq, PartialOrd};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
  /// `-`
  Minus,
  /// `!`
  Bang,
}

impl fmt::Display for Prefix {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Prefix::Minus => write!(f, "-"),
      Prefix::Bang => write!(f, "!"),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Infix {
  /// `+`
  Plus,
  /// `-`
  Minus,
  /// `/`
  Slash,
  /// `*`
  Asterisk,
  /// `>`
  Gt,
  /// `<`
  Lt,
  /// `==`
  Equal,
  /// `!=`
  NotEq,
  /// `(`
  Call,
}

impl fmt::Display for Infix {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Infix::Plus => write!(f, "+"),
      Infix::Minus => write!(f, "-"),
      Infix::Asterisk => write!(f, "*"),
      Infix::Slash => write!(f, "/"),
      Infix::Gt => write!(f, ">"),
      Infix::Lt => write!(f, "<"),
      Infix::Equal => write!(f, "=="),
      Infix::NotEq => write!(f, "!="),
      Infix::Call => write!(f, "("),
    }
  }
}

#[derive(PartialEq, PartialOrd)]
pub enum BinaryOperator {
  Lowest,
  Equals,
  LtGt,
  Sum,
  Product,
  Prefix,
  Call,
}
