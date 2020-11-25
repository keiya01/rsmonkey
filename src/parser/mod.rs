use std::mem;

use crate::{token, lexer};
use crate::ast::{Program};

pub mod stmt;

impl token::Token {
  pub(super) fn is(&self, token: token::Token) -> bool {
    *self == token
  }
}

pub struct Parser {
  l: lexer::Lexer,
  current_token: token::Token,
  peek_token: token::Token,
  pub(super) errors: Vec<String>,
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
}

#[cfg(test)]
mod tests {
  use crate::ast::stmt::{Statement};
  use crate::ast::ident::{Identifier};
  use crate::ast::expr::{Expression};
  use crate::ast::lit::{Literal};
  use crate::ast::operator::{Prefix, Infix};
  use super::*;

  #[test]
  fn test_parse_let_statement() {
    let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";

    let l = lexer::Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parse_errors(&p);

    if program.statements.len() != 3 {
      panic!("program.statements does not contain 3 statements. got={}", program.statements.len());
    }

    let tests = vec![
      Identifier::new("x".to_string()),
      Identifier::new("y".to_string()),
      Identifier::new("foobar".to_string()),
    ];

    
    for (i, tt) in tests.into_iter().enumerate() {
      let stmt = &program.statements[i];
      comp_let_statement(stmt, tt);
    }
  }

  fn comp_let_statement(stmt: &Statement, tt: Identifier) {
    match stmt {
      Statement::Let(_) => (),
      _ => panic!("stmt.token_literal() not 'let'. get={:?}", stmt),
    };

    if let Statement::Let(let_stmt) = stmt {
      assert_eq!(&let_stmt.ident.value, &tt.value, "expect={}, actual={}", &tt.value, &let_stmt.ident.value);
    }
  }
  
  #[test]
  fn test_parse_return_statement() {
    let input = "
return 5;
return 10;
return 993322;
";

    let l = lexer::Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parse_errors(&p);

    if program.statements.len() != 3 {
      panic!("program.statements does not contain 3 statements. got={}", program.statements.len());
    }
    
    for stmt in &program.statements {
      match stmt {
        Statement::Return(_) => (),
        _ => panic!("ReturnStatement is not included, got {:?}", stmt),
      }
    }
  }

  #[test]
  fn test_parse_identifier_expression() {
    let input = "foobar;";

    let l = lexer::Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parse_errors(&p);

    if program.statements.len() != 1 {
      panic!("program.statements should has only 1 statement, but got {}", program.statements.len());
    }

    let expr = match &program.statements[0] {
      Statement::Expr(expr) => expr,
      _ => panic!("program.statements should has ExpressionStatement, but got {:?}", program.statements[0]),
    };

    test_identifier(&expr.value, "foobar");
  }

  #[test]
  fn test_parse_int_literal_expression() {
    let input = "5;";

    let l = lexer::Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parse_errors(&p);

    if program.statements.len() != 1 {
      panic!("program.statements should has only 1 statement, but got {}", program.statements.len());
    }

    let expr = match &program.statements[0] {
      Statement::Expr(expr) => expr,
      _ => panic!("program.statements should has ExpressionStatement, but got {:?}", program.statements[0]),
    };

    test_integer_literal(&expr.value, &5);
  }
  
  #[test]
  fn test_boolean_expression() {
    let input = "
true;
false;
";

    let tests = vec![true, false];

    let l = lexer::Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parse_errors(&p);

    if program.statements.len() != 2 {
      panic!("program.statements should has only 1 statement, but got {}", program.statements.len());
    }

    for (i, tt) in tests.iter().enumerate() {
      let expr = match &program.statements[i] {
        Statement::Expr(expr) => expr,
        _ => panic!("program.statements should has ExpressionStatement, but got {:?}", program.statements[i]),
      };
  
      test_boolean(&expr.value, tt);
    }
  }
  
  #[test]
  fn test_parse_prefix_expression() {
    struct PrefixExpressionTest {
      input: String,
      operator: Prefix,
      right: ExpressionLiteral,
    }
    let prefix_expression_tests = vec![
      PrefixExpressionTest { 
        input: "!5;".to_string(),
        operator: Prefix::Bang,
        right: ExpressionLiteral::Int(5),
      },
      PrefixExpressionTest {
        input: "-15;".to_string(),
        operator: Prefix::Minus,
        right: ExpressionLiteral::Int(15),
      },
      PrefixExpressionTest {
        input: "!true;".to_string(),
        operator: Prefix::Bang,
        right: ExpressionLiteral::Bool(true),
      },
      PrefixExpressionTest {
        input: "!false;".to_string(),
        operator: Prefix::Bang,
        right: ExpressionLiteral::Bool(false),
      },
    ];

    for tt in prefix_expression_tests.into_iter() {
      let l = lexer::Lexer::new(tt.input.clone());
      let mut p = Parser::new(l);
  
      let program = p.parse_program();
      check_parse_errors(&p);
  
      if program.statements.len() != 1 {
        panic!("program.statements should has only 1 statement, but got {}", program.statements.len());
      }
  
      let expr = match &program.statements[0] {
        Statement::Expr(expr) => expr,
        _ => panic!("program.statements should has ExpressionStatement, but got {:?}", program.statements[0]),
      };
  
      let pre = match &expr.value {
        Expression::Prefix(pre) => pre,
        _ => panic!("Expression should has PrefixExpression, got {:?}", &expr.value)
      };

      if &pre.operator != &tt.operator {
        panic!("Prefix should has '{:?}', but got '{:?}'", &tt.operator, &pre.operator);
      }

      test_literal_expression(&pre.right, tt.right);
    }
  }

  #[test]
  fn test_parse_infix_expression() {
    struct InfixExpressionTest {
      input: String,
      left: ExpressionLiteral,
      operator: Infix,
      right: ExpressionLiteral,
    }
    let infix_expression_tests = vec![
      InfixExpressionTest {
        input: "5 + 5;".to_string(),
        left: ExpressionLiteral::Int(5),
        operator: Infix::Plus,
        right: ExpressionLiteral::Int(5),
      },
      InfixExpressionTest { 
        input: "5 - 5;".to_string(),
        left: ExpressionLiteral::Int(5),
        operator: Infix::Minus,
        right: ExpressionLiteral::Int(5),
      },
      InfixExpressionTest { 
        input: "5 * 5;".to_string(),
        left: ExpressionLiteral::Int(5),
        operator: Infix::Asterisk,
        right: ExpressionLiteral::Int(5),
      },
      InfixExpressionTest { 
        input: "5 / 5;".to_string(),
        left: ExpressionLiteral::Int(5),
        operator: Infix::Slash,
        right: ExpressionLiteral::Int(5),
      },
      InfixExpressionTest { 
        input: "5 > 5;".to_string(),
        left: ExpressionLiteral::Int(5),
        operator: Infix::Gt,
        right: ExpressionLiteral::Int(5),
      },
      InfixExpressionTest { 
        input: "5 < 5;".to_string(),
        left: ExpressionLiteral::Int(5),
        operator: Infix::Lt,
        right: ExpressionLiteral::Int(5),
      },
      InfixExpressionTest { 
        input: "5 == 5;".to_string(),
        left: ExpressionLiteral::Int(5),
        operator: Infix::Equal,
        right: ExpressionLiteral::Int(5),
      },
      InfixExpressionTest { 
        input: "5 != 5;".to_string(),
        left: ExpressionLiteral::Int(5),
        operator: Infix::NotEq,
        right: ExpressionLiteral::Int(5),
      },
      InfixExpressionTest { 
        input: "true == true;".to_string(),
        left: ExpressionLiteral::Bool(true),
        operator: Infix::Equal,
        right: ExpressionLiteral::Bool(true),
      },
      InfixExpressionTest { 
        input: "true != false;".to_string(),
        left: ExpressionLiteral::Bool(true),
        operator: Infix::NotEq,
        right: ExpressionLiteral::Bool(false),
      },
      InfixExpressionTest { 
        input: "false == false;".to_string(),
        left: ExpressionLiteral::Bool(false),
        operator: Infix::Equal,
        right: ExpressionLiteral::Bool(false),
      },
    ];

    for tt in infix_expression_tests.into_iter() {
      let l = lexer::Lexer::new(tt.input.clone());
      let mut p = Parser::new(l);
  
      let program = p.parse_program();
      check_parse_errors(&p);
  
      if program.statements.len() != 1 {
        panic!("program.statements should has only 1 statement, but got {}", program.statements.len());
      }
  
      let expr = match &program.statements[0] {
        Statement::Expr(expr) => expr,
        _ => panic!("program.statements should has ExpressionStatement, but got {:?}", program.statements[0]),
      };

      let inf = match &expr.value {
        Expression::Infix(inf) => inf,
        _ => panic!("Expression should has InfixExpression, got {:?}", &expr.value),
      };
      
      test_literal_expression(&inf.left, tt.left);
      
      if &inf.operator != &tt.operator {
        panic!("Infix should has '{:?}', but got '{:?}'", &tt.operator, &inf.operator);
      }

      test_literal_expression(&inf.right, tt.right);
    }
  }

  #[test]
  fn test_operator_precedence_parsing() {
    struct PrecedenceTest {
      input: String,
      expected: String,
    }
    let precedence_tests = [
      PrecedenceTest { 
        input: "true".to_string(),
        expected: "true".to_string(),
      },
      PrecedenceTest { 
        input: "false".to_string(),
        expected: "false".to_string(),
      },
      PrecedenceTest { 
        input: "3 > 5 == false".to_string(),
        expected: "((3 > 5) == false)".to_string(),
      },
      PrecedenceTest { 
        input: "3 < 5 == true".to_string(),
        expected: "((3 < 5) == true)".to_string(),
      },
      PrecedenceTest { 
        input: "-a + b".to_string(),
        expected: "((-a) + b)".to_string(),
      },
      PrecedenceTest { 
        input: "!-a".to_string(),
        expected: "(!(-a))".to_string(),
      },
      PrecedenceTest { 
        input: "a + b + c".to_string(),
        expected: "((a + b) + c)".to_string(),
      },
      PrecedenceTest { 
        input: "a + b - c".to_string(),
        expected: "((a + b) - c)".to_string(),
      },
      PrecedenceTest { 
        input: "a * b * c".to_string(),
        expected: "((a * b) * c)".to_string(),
      },
      PrecedenceTest { 
        input: "a * b / c".to_string(),
        expected: "((a * b) / c)".to_string(),
      },
      PrecedenceTest { 
        input: "a + b / c".to_string(),
        expected: "(a + (b / c))".to_string(),
      },
      PrecedenceTest { 
        input: "a + b * c + d / e - f".to_string(),
        expected: "(((a + (b * c)) + (d / e)) - f)".to_string(),
      },
      PrecedenceTest { 
        input: "3 + 4; -5 * 5".to_string(),
        expected: "(3 + 4)((-5) * 5)".to_string(),
      },
      PrecedenceTest { 
        input: "5 > 4 == 3 < 4".to_string(),
        expected: "((5 > 4) == (3 < 4))".to_string(),
      },
      PrecedenceTest { 
        input: "5 < 4 != 3 > 4".to_string(),
        expected: "((5 < 4) != (3 > 4))".to_string(),
      },
      PrecedenceTest { 
        input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
        expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
      },
      PrecedenceTest { 
        input: "1 + (2 + 3) + 4".to_string(),
        expected: "((1 + (2 + 3)) + 4)".to_string(),
      },
      PrecedenceTest { 
        input: "(5 + 5) * 2".to_string(),
        expected: "((5 + 5) * 2)".to_string(),
      },
      PrecedenceTest { 
        input: "2 / (5 + 5)".to_string(),
        expected: "(2 / (5 + 5))".to_string(),
      },
      PrecedenceTest { 
        input: "-(5 + 5)".to_string(),
        expected: "(-(5 + 5))".to_string(),
      },
      PrecedenceTest { 
        input: "!(true == true)".to_string(),
        expected: "(!(true == true))".to_string(),
      },
    ];

    for tt in precedence_tests.iter() {
      let l = lexer::Lexer::new(tt.input.clone());
      let mut p = Parser::new(l);
  
      let program = p.parse_program();
      check_parse_errors(&p);
  
      let actual = format!("{}", program);
      if actual != tt.expected {
        panic!("expected={}, actual={}", tt.expected, actual);
      }
    }
  }

  enum ExpressionLiteral {
    Int(i64),
    Bool(bool),
    Str(String),
  }

  fn check_parse_errors(p: &Parser) {
    let errs = &p.errors;
    if errs.len() == 0 {
      return;
    }

    eprintln!("Parser has {} errors", errs.len());
    for err in errs.into_iter() {
      eprintln!("Parser Error: {}", err);
    }
    panic!();
  }

  fn test_literal_expression(expr: &Expression, expect: ExpressionLiteral) {
    match expect {
      ExpressionLiteral::Int(v) => test_integer_literal(expr, &v),
      ExpressionLiteral::Str(v) => test_identifier(expr, &v),
      ExpressionLiteral::Bool(v) => test_boolean(expr, &v),
    };
  }

  fn test_identifier(expr: &Expression, value: &str) {
    let ident = match &expr {
      Expression::Identifier(ident) => ident,
      _ => panic!("Expression should has Identifier, got {}", &expr)
    };

    if &ident.value != value {
      panic!("Identifier should has foobar, but got {}", &ident.value);
    }
  }

  fn test_integer_literal(expr: &Expression, comp: &i64) {
    let lit = match &expr {
      Expression::Literal(lit) => lit,
      _ => panic!("Expression should has Literal, got {}", &expr)
    };
    
    let int = match &lit {
      Literal::Integer(int) => int,
      _ => panic!("Literal should has Integer, got {}", &lit)
    };

    if &int.value != comp {
      panic!("Identifier should has {}, but got {}", int.value, comp);
    }
  }

  fn test_boolean(expr: &Expression, comp: &bool) {
    let lit = match &expr {
      Expression::Literal(lit) => lit,
      _ => panic!("Expression should has Literal, got {}", &expr)
    };
    
    let int = match &lit {
      Literal::Boolean(v) => v,
      _ => panic!("Literal should has Boolean, got {}", &lit)
    };

    if &int.value != comp {
      panic!("Identifier should has {}, but got {}", int.value, comp);
    }
  }
}
