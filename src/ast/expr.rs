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
  Call(CallExpression),
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
    write!(f, "if({}) {}", self.condition, self.consequence)?;
    if let Some(alt) = &self.alternative {
      write!(f, " else {}", alt)?;
    }
    Ok(())
  }
}

#[derive(Debug)]
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

    let mut iter = self.args.iter();    
    let mut next = iter.next();
    while let Some(val) = next {
      next = iter.next();
      write!(f, "{}", val)?;
      if let Some(_) = next {
        write!(f, ", ")?;
      }
    }

    write!(f, ")")
  }
}
