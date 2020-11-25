use std::fmt;

use super::ident::Identifier;
use super::expr::Expression;

#[derive(Debug)]
pub enum Statement {
  Let(LetStatement),
  Return(ReturnStatement),
  Expr(ExpressionStatement),
  Block(BlockStatement),
}

impl fmt::Display for Statement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Statement::Let(let_stmt) => write!(f, "{}", let_stmt),
      Statement::Return(return_stmt) => write!(f, "{}", return_stmt),
      Statement::Expr(expr_stmt) => write!(f, "{}", expr_stmt),
      Statement::Block(block) => write!(f, "{}", block),
    }
  }
}

#[derive(Debug)]
pub struct LetStatement {
  pub ident: Identifier,
  pub value: Expression,
}

impl LetStatement {
  pub fn new(ident: Identifier, value: Expression) -> LetStatement {
    LetStatement { ident, value }
  }
}

impl fmt::Display for LetStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "let ")?;
    write!(f, "{}", self.ident)?;
    write!(f, " = ")?;
    write!(f, "{}", self.value)?;
    write!(f, ";")
  }
}

#[derive(Debug)]
pub struct ReturnStatement {
  value: Expression,
}

impl ReturnStatement {
  pub fn new(value: Expression) -> ReturnStatement {
    ReturnStatement { value }
  }
}

impl fmt::Display for ReturnStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "return ")?;
    write!(f, "{}", self.value)?;
    write!(f, ";")
  }
}

#[derive(Debug)]
pub struct ExpressionStatement {
  pub value: Expression,
}

impl ExpressionStatement {
  pub fn new(value: Expression) -> ExpressionStatement {
    ExpressionStatement { value }
  }
}

impl fmt::Display for ExpressionStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

#[derive(Debug)]
pub struct BlockStatement {
  pub statements: Vec<Statement>,
}

impl BlockStatement {
  pub fn new(statements: Vec<Statement>) -> BlockStatement {
    return BlockStatement { statements }
  }
}

impl fmt::Display for BlockStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for stmt in &self.statements {
      write!(f, "{}", stmt)?;
    }
    Ok(())
  }
}
