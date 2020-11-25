use super::{Parser};
use crate::{token};
use crate::ast::stmt::*;
use crate::ast::expr::*;
use crate::ast::ident::{Identifier};
use crate::ast::operator::{BinaryOperator};

impl Parser {
  pub(super) fn parse_statement(&mut self) -> Option<Statement> {
    match self.current_token {
      token::Token::LET => self.parse_let_statement(),
      token::Token::RETURN => self.parse_return_statement(),
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

    while !self.current_token.is(token::Token::SEMICOLON) {
      self.next_token();
    }

    let stmt = Statement::Let(
      LetStatement::new(
        ident,
        Expression::Identifier(
          // TODO: fix
          Identifier::new("".to_string()),
        ),
      ),
    );

    Some(stmt)
  }

  fn parse_return_statement(&mut self) -> Option<Statement> {
    self.next_token();

    while !self.current_token.is(token::Token::SEMICOLON) {
      self.next_token();
    }

    let stmt = Statement::Return(
      ReturnStatement::new(
        Expression::Identifier(
          // TODO: fix
          Identifier::new("".to_string()),
        ),
      ),
    );

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
