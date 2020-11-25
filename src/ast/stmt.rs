use std::fmt;

use super::ident::Identifier;
use super::expr::Expression;

#[derive(Debug)]
pub enum Statement {
  Let(LetStatement),
  Return(ReturnStatement),
  Expr(ExpressionStatement),
}

impl fmt::Display for Statement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Statement::Let(let_stmt) => write!(f, "{}", format!("{}", let_stmt)),
      Statement::Return(return_stmt) => write!(f, "{}", format!("{}", return_stmt)),
      Statement::Expr(expr_stmt) => write!(f, "{}", format!("{}", expr_stmt)),
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
    write!(f, "let ").unwrap();
    write!(f, "{}", format!("{}", self.ident)).unwrap();
    write!(f, " = ").unwrap();
    write!(f, "{}", format!("{}", self.value)).unwrap();
    write!(f, ";").unwrap();
    Ok(())
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
    write!(f, "return ").unwrap();
    write!(f, "{}", format!("{}", self.value)).unwrap();
    write!(f, ";").unwrap();
    Ok(())
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
    write!(f, "{}", format!("{}", self.value)).unwrap();
    Ok(())
  }
}
