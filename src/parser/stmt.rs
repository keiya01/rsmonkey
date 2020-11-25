use super::{Parser};
use crate::{token};
use crate::ast::stmt::{Statement, LetStatement, ReturnStatement, ExpressionStatement};
use crate::ast::expr::{Expression, PrefixExpression, InfixExpression};
use crate::ast::lit::*;
use crate::ast::ident::{Identifier};
use crate::ast::operator::{Prefix, Infix, BinaryOperator};

impl token::Token {
  fn to_binary_operator(&self) -> BinaryOperator {
    match self {
      token::Token::EQ | token::Token::NotEq => BinaryOperator::Equals,
      token::Token::LT | token::Token::GT => BinaryOperator::LtGt,
      token::Token::PLUS | token::Token::MINUS => BinaryOperator::Sum,
      token::Token::ASTERISK | token::Token::SLASH => BinaryOperator::Product,
      _ => BinaryOperator::Lowest,
    }
  }
}

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

  fn parse_expression(&mut self, op: BinaryOperator) -> Option<Expression> {
    let mut left = match self.parse_prefix() {
      Some(expr) => expr,
      None => return None,
    };
    
    while !self.peek_token.is(token::Token::SEMICOLON) && op < self.peek_token.to_binary_operator() {
      self.next_token();
      left = match self.parse_infix(left) {
        Some(expr) => expr,
        None => return None,
      };
    }

    Some(left)
  }

  fn parse_prefix(&mut self) -> Option<Expression> {
    match &self.current_token {
      token::Token::IDENT(s) => self.parse_identifier(s.to_string()),
      token::Token::INT(int) => self.parse_integer_literal(*int),
      token::Token::TRUE | token::Token::FALSE => self.parse_bool(),
      token::Token::BANG | token::Token::MINUS => self.parse_prefix_expression(),
      token::Token::LPAREN => self.parse_grouped_expression(),
      _ => {
        self.no_prefix_parse_error();
        return None;
      }
    }
  }

  fn parse_infix(&mut self, left: Expression) -> Option<Expression> {
    let operator = match self.current_token {
      token::Token::PLUS => Infix::Plus,
      token::Token::MINUS => Infix::Minus,
      token::Token::SLASH => Infix::Slash,
      token::Token::ASTERISK => Infix::Asterisk,
      token::Token::GT => Infix::Gt,
      token::Token::LT => Infix::Lt,
      token::Token::EQ => Infix::Equal,
      token::Token::NotEq => Infix::NotEq,
      _ => return None,
    };

    let precedence = self.current_token.to_binary_operator();

    self.next_token();

    let right = match self.parse_expression(precedence) {
      Some(expr) => expr,
      None => return None,
    };

    let expr = Expression::Infix(InfixExpression::new(Box::new(left), operator, Box::new(right)));
    Some(expr)
  }

  fn parse_identifier(&self, value: String) -> Option<Expression> {
    Some(Expression::Identifier(Identifier::new(value)))
  }

  fn parse_integer_literal(&self, int: i64) -> Option<Expression> {
    Some(
      Expression::Literal(
        Literal::Integer(
          Integer::new(int),
        ),
      )
    )
  }

  fn parse_bool(&self) -> Option<Expression> {
    Some(
      Expression::Literal(
        Literal::Boolean(
          Boolean::new(self.current_token == token::Token::TRUE),
        ),
      ),
    )
  }

  fn parse_prefix_expression(&mut self) -> Option<Expression> {
    let operator = match self.current_token {
      token::Token::MINUS => Prefix::Minus,
      token::Token::BANG => Prefix::Bang,
      _ => {
        self.no_prefix_parse_error();
        return None;
      }
    };
    
    self.next_token();

    let right = match self.parse_expression(BinaryOperator::Prefix) {
      Some(expr) => expr,
      None => return None,
    };

    Some(Expression::Prefix(PrefixExpression::new(operator, Box::new(right))))
  }

  fn parse_grouped_expression(&mut self) -> Option<Expression> {
    self.next_token();

    let left = self.parse_expression(BinaryOperator::Lowest);

    if !self.expect_peek(token::Token::RPAREN) {
      return None;
    }

    left
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

  fn no_prefix_parse_error(&mut self) {
    let msg = format!("no prefix parse function for {:?}", self.current_token);
    self.errors.push(msg);
  }
}
