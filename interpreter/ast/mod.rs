use std::fmt;

use self::stmt::{Statement};

pub mod stmt;
pub mod expr;
pub mod ident;
pub mod lit;
pub mod operator;

pub struct Program {
  pub statements: Vec<Statement>,
}

impl Program {
  pub fn new() -> Program {
    return Program { statements: vec![] };
  }
}

impl fmt::Display for Program {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for stmt in &self.statements {
      write!(f, "{}", stmt)?;
    }
    Ok(())
  }
}
