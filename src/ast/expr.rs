use std::fmt;
use super::ident::Identifier;
use super::lit::{Literal};
use super::operator::{Prefix, Infix};
use super::stmt::{BlockStatement};

#[derive(Debug)]
pub enum Expression {
  Identifier(Identifier),
  Literal(Literal),
  Prefix(PrefixExpression),
  Infix(InfixExpression),
  If(IfExpression),
}

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expression::Identifier(ident) => write!(f, "{}", format!("{}", ident)),
      Expression::Literal(lit) => write!(f, "{}", format!("{}", lit)),
      Expression::Prefix(pre) => write!(f, "{}", format!("{}", pre)),
      Expression::Infix(inf) => write!(f, "{}", format!("{}", inf)),
      Expression::If(if_expr) => write!(f, "{}", format!("{}", if_expr)),
    }
  }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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
    write!(f, "if ({}) {}", self.condition, self.consequence).unwrap();
    if let Some(alt) = &self.alternative {
      write!(f, " else {}", alt).unwrap();
    }
    Ok(())
  }
}
