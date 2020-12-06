use std::fmt;

use crate::utils;
use super::ident::Identifier;
use super::lit::{Literal};
use super::operator::{Prefix, Infix};
use super::stmt::{BlockStatement};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
  Identifier(Identifier),
  Literal(Literal),
  Prefix(PrefixExpression),
  Infix(InfixExpression),
  If(IfExpression),
  Call(CallExpression),
  Index(IndexExpression),
}

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expression::Identifier(ident) => write!(f, "{}", ident),
      Expression::Literal(lit) => write!(f, "{}", lit),
      Expression::Prefix(pre) => write!(f, "{}", pre),
      Expression::Infix(inf) => write!(f, "{}", inf),
      Expression::If(if_expr) => write!(f, "{}", if_expr),
      Expression::Call(call_expr) => write!(f, "{}", call_expr),
      Expression::Index(index) => write!(f, "{}", index),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
  pub operator: Prefix,
  pub right: Box<Expression>,
}

impl PrefixExpression {
  pub fn new(operator: Prefix, right: Box<Expression>) -> PrefixExpression {
    PrefixExpression { operator, right }
  }
}

impl fmt::Display for PrefixExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({}{})", &self.operator, &self.right)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
  pub left: Box<Expression>,
  pub operator: Infix,
  pub right: Box<Expression>,
}

impl InfixExpression {
  pub fn new(left: Box<Expression>, operator: Infix, right: Box<Expression>) -> InfixExpression {
    InfixExpression { left, operator, right }
  }
}

impl fmt::Display for InfixExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({} {} {})", &self.left, &self.operator, &self.right)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
  pub condition: Box<Expression>,
  pub consequence: BlockStatement,
  pub alternative: Option<BlockStatement>,
}

impl IfExpression {
  pub fn new(condition: Box<Expression>, consequence: BlockStatement, alternative: Option<BlockStatement>) -> IfExpression {
    IfExpression { condition, consequence, alternative }
  }
}

impl fmt::Display for IfExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "if({}) {}", self.condition, self.consequence)?;
    if let Some(alt) = &self.alternative {
      write!(f, " else {}", alt)?;
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
  pub func: Box<Expression>, // Identifier or Func literal
  pub args: Vec<Expression>,
}

impl CallExpression {
  pub fn new(func: Box<Expression>, args: Vec<Expression>) -> CallExpression {
    CallExpression { func, args }
  }
}

impl fmt::Display for CallExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}(", self.func)?;

    utils::write_object_list(&self.args, f)?;

    write!(f, ")")
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
  pub left: Box<Expression>, // Identifier or Func literal
  pub index: Box<Expression>,
}

impl IndexExpression {
  pub fn new(left: Box<Expression>, index: Box<Expression>) -> IndexExpression {
    IndexExpression { left, index }
  }
}

impl fmt::Display for IndexExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({}[{}])", self.left, self.index)
  }
}
