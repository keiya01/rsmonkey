use std::mem;

use crate::{token, lexer};
use crate::ast::{Program};

pub mod stmt;
pub mod expr;

impl token::Token {
  pub(super) fn is(&self, token: token::Token) -> bool {
    *self == token
  }
}

pub struct Parser {
  l: lexer::Lexer,
  current_token: token::Token,
  peek_token: token::Token,
  pub errors: Vec<String>,
}

impl Parser {
  pub fn new(mut l: lexer::Lexer) -> Parser {
    let current_token = l.next_token();
    let peek_token = l.next_token();
    Parser { l, current_token, peek_token, errors: vec![] }
  }

  pub(super) fn next_token(&mut self) {
    self.current_token = mem::replace(&mut self.peek_token, self.l.next_token());
  }

  pub fn parse_program(&mut self) -> Program {
    let mut program = Program::new();
    
    while !self.current_token.is(token::Token::EOF) {
      if let Some(stmt) = self.parse_statement() {
        program.statements.push(stmt);
      }
      self.next_token();
    }

    program
  }

  pub(super) fn peek_error(&mut self, t: token::Token) {
    let msg = format!("expected next token to be {:?}, got {:?} instead", t, self.peek_token);
    self.errors.push(msg);
  }

  pub(super) fn expect_peek(&mut self, t: token::Token) -> bool {
    if self.peek_token == t {
      self.next_token();
      true
    } else {
      self.peek_error(t);
      false
    }
  }

  pub fn check_parse_errors(&self) -> bool {
    let errs = &self.errors;
    if errs.len() == 0 {
      return true;
    }

    eprintln!("error: Parser has {} errors", errs.len());
    for err in errs.into_iter() {
      eprintln!("error: Parser Error: {}", err);
    }
    eprintln!("error: {} parser errors occurred.", errs.len());

    return false;
  }
}
