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
      write!(f, "{}", format!("{}", stmt)).unwrap();
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_format() {
      let program = Program { 
        statements: vec![
          stmt::Statement::Let(
            stmt::LetStatement {
              ident: ident::Identifier { value: "foo".to_string()  },
              value: expr::Expression::Identifier(ident::Identifier { value: "bar".to_string()  }),
            }
          )
        ]
      };
      if format!("{}", program) != "let foo = bar;" {
        panic!("formatted program is incorrect, got {}", format!("{}", program));
      }
  }
}
