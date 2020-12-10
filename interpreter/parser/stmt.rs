use super::{Parser};
use crate::{token};
use crate::ast::stmt::*;
use crate::ast::ident::{Identifier};
use crate::ast::operator::{BinaryOperator};

impl Parser {
  pub(super) fn parse_statement(&mut self) -> Option<Statement> {
    match &self.current_token {
      token::Token::LET => self.parse_let_statement(),
      token::Token::RETURN => self.parse_return_statement(),
      token::Token::COMMENT(s) => self.parse_comment_statement(s.to_string()),
      _ => self.parse_expression_statement(),
    }
  }

  fn parse_let_statement(&mut self) -> Option<Statement> {
    if !self.expect_ident_peek() {
      return None;
    }

    let val = if let token::Token::IDENT(s) = &self.current_token {
      s.to_string()
    } else {
      return None;
    };

    let ident = Identifier::new(val);

    if !self.expect_peek(token::Token::ASSIGN) {
      return None;
    }

    self.next_token();

    let value = match self.parse_expression(BinaryOperator::Lowest) {
      Some(expr) => expr,
      None => return None,
    };

    if self.peek_token.is(token::Token::SEMICOLON) {
      self.next_token();
    }

    let stmt = Statement::Let(
      LetStatement::new(
        ident,
        value,
      ),
    );

    Some(stmt)
  }

  fn parse_return_statement(&mut self) -> Option<Statement> {
    self.next_token();

    let value = match self.parse_expression(BinaryOperator::Lowest) {
      Some(expr) => expr,
      None => return None,
    };

    if self.peek_token.is(token::Token::SEMICOLON) {
      self.next_token();
    }

    let stmt = Statement::Return(ReturnStatement::new(value));

    Some(stmt)
  }

  fn parse_expression_statement(&mut self) -> Option<Statement> {
    match self.parse_expression(BinaryOperator::Lowest) {
      Some(expr) => {
        if self.peek_token.is(token::Token::SEMICOLON) {
          self.next_token();
        }
        Some(Statement::Expr(ExpressionStatement::new(expr)))
      },
      None => None,
    }
  }

  pub(super) fn parse_block_statement(&mut self) -> BlockStatement {
    self.next_token();

    let mut statements = vec![];

    while !self.current_token.is(token::Token::RBRACE) && !self.current_token.is(token::Token::EOF) {
      if let Some(stmt) = self.parse_statement() {
        statements.push(stmt);
      }
      self.next_token();
    }

    BlockStatement::new(statements)
  }

  fn parse_comment_statement(&self, s: String) -> Option<Statement> {
    Some(Statement::Comment(CommentStatement::new(s)))
  }

  fn expect_ident_peek(&mut self) -> bool {
    match self.peek_token {
      token::Token::IDENT(_) => {
        self.next_token();
        true
      },
      _ => {
        self.peek_error(token::Token::IDENT("something".to_string()));
        false
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::lexer;
  use crate::parser::expr::*;
  use super::*;

  #[test]
  fn test_parse_let_statement() {
    let tests = vec![
      ("let x = 5;", "x", ExpressionLiteral::Int(5)),
      ("let y = true;", "y", ExpressionLiteral::Bool(true)),
      ("let foobar = y;", "foobar", ExpressionLiteral::Str("y".to_string())),
    ];

    for (input, expected_ident, expected_expr) in tests.into_iter() {
      let l = lexer::Lexer::new(input.to_string());
      let mut p = Parser::new(l);
  
      let program = p.parse_program();
      if !p.check_parse_errors() {
        panic!();
      }
  
      if program.statements.len() != 1 {
        panic!("program.statements does not contain 1 statements. got={}", program.statements.len());
      }  
      
      let stmt = &program.statements[0];
      let let_stmt = match stmt {
        Statement::Let(let_stmt) => let_stmt,
        _ => panic!("stmt.token_literal() not 'let'. get={:?}", stmt),
      };
  
      assert_eq!(
        &let_stmt.ident.value,
        expected_ident,
        "expect={}, actual={}",
        expected_ident,
        &let_stmt.ident.value,
      );

      test_literal_expression(&let_stmt.value, expected_expr);
    }
  }
  
  #[test]
  fn test_parse_return_statement() {
    let tests = vec![
      ("return 5;", ExpressionLiteral::Int(5)),
      ("return true;", ExpressionLiteral::Bool(true)),
      ("return y;", ExpressionLiteral::Str("y".to_string())),
    ];

    for (input, expected_expr) in tests.into_iter() {
      let l = lexer::Lexer::new(input.to_string());
      let mut p = Parser::new(l);

      let program = p.parse_program();
      if !p.check_parse_errors() {
        panic!();
      }

      if program.statements.len() != 1 {
        panic!("program.statements does not contain 1 statements. got={}", program.statements.len());
      }
      
      let return_stmt = match &program.statements[0] {
        Statement::Return(return_stmt) => return_stmt,
        _ => panic!("ReturnStatement is not included, got {:?}", &program.statements[0]),
      };

      test_literal_expression(&return_stmt.value, expected_expr);
    }
  }

  #[test]
  fn test_parse_comment_statement() {
    let tests = vec![
      ("// foo bar", "foo bar"),
    ];

    for (input, expected) in tests.into_iter() {
      let l = lexer::Lexer::new(input.to_string());
      let mut p = Parser::new(l);

      let program = p.parse_program();
      if !p.check_parse_errors() {
        panic!();
      }

      if program.statements.len() != 1 {
        panic!("program.statements does not contain 1 statements. got={}", program.statements.len());
      }
      
      let comment_stmt = match &program.statements[0] {
        Statement::Comment(comment) => comment,
        _ => panic!("CommentStatement is not included, got {:?}", &program.statements[0]),
      };

      assert_eq!(&comment_stmt.value, expected);
    }
  }
}
