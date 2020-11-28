use crate::ast::Program;
use crate::ast::stmt::{Statement};
use crate::ast::expr::{Expression};
use crate::ast::lit::{Literal};
use crate::ast::operator::{Prefix, Infix};

mod object;

const TRUE: object::Object = object::Object::Boolean(object::Boolean { value: true });
const FALSE: object::Object = object::Object::Boolean(object::Boolean { value: false });
const NULL: object::Object = object::Object::Null;

pub fn eval(node: Program) -> object::Object {
  let mut result: object::Object = NULL;
  for stmt in &node.statements {
    result = eval_statement(stmt);
  }
  result
}

fn eval_statement(statement: &Statement) -> object::Object {
  match statement {
    Statement::Expr(expr) => eval_expression(&expr.value),
    _ => NULL,
  }

}

fn eval_expression(expr: &Expression) -> object::Object {
  match expr {
    Expression::Literal(lit) => eval_literal(&lit),
    Expression::Prefix(pre) => {
      let right = eval_expression(&pre.right);
      eval_prefix_expression(&pre.operator, right)
    },
    Expression::Infix(inf) => {
      let left = eval_expression(&inf.left);
      let right = eval_expression(&inf.right);
      eval_infix_expression(left, &inf.operator, right)
    },
    _ => NULL,
  }
}

fn eval_literal(lit: &Literal) -> object::Object {
  match lit {
    Literal::Integer(int) => object::Object::Integer(
      object::Integer::new(int.value),
    ),
    Literal::Boolean(val) => native_bool_to_boolean_object(val.value),
    _ => return NULL,
  }
}

fn eval_prefix_expression(operator: &Prefix, right: object::Object) -> object::Object {
  match operator {
    Prefix::Bang => eval_bang_operator_expression(right),
    Prefix::Minus => eval_minus_operator_expression(right),
  }
}

fn eval_bang_operator_expression(right: object::Object) -> object::Object {
  match right {
    object::Object::Boolean(val) => native_bool_to_boolean_object(!val.value),
    object::Object::Null => TRUE,
    _ => FALSE,
  }
}

fn eval_minus_operator_expression(right: object::Object) -> object::Object {
  match right {
    object::Object::Integer(int) => 
      object::Object::Integer(object::Integer::new(-int.value)),
    _ => NULL,
  }
}

fn eval_infix_expression(left: object::Object, operator: &Infix, right: object::Object) -> object::Object {
  if is_object_integer(&left) && is_object_integer(&right) {
    return eval_integer_infix_expression(left, operator, right);
  }

  match operator {
    Infix::Equal => native_bool_to_boolean_object(left == right),
    Infix::NotEq => native_bool_to_boolean_object(left != right),
    _ => NULL,
  }
}

fn eval_integer_infix_expression(left: object::Object, operator: &Infix, right: object::Object) -> object::Object {
  let left = if let object::Object::Integer(int) = left {
    int.value
  } else {
    return NULL;
  };
  let right = if let object::Object::Integer(int) = right {
    int.value
  } else {
    return NULL;
  };

  let int = match operator {
    Infix::Plus => object::Integer::new(left + right),
    Infix::Minus => object::Integer::new(left - right),
    Infix::Asterisk => object::Integer::new(left * right),
    Infix::Slash => object::Integer::new(left / right),
    Infix::Lt => return native_bool_to_boolean_object(left < right),
    Infix::Gt => return native_bool_to_boolean_object(left > right),
    Infix::Equal => return native_bool_to_boolean_object(left == right),
    Infix::NotEq => return native_bool_to_boolean_object(left != right),
    _ => return NULL,
  };

  object::Object::Integer(int)
}

fn is_object_integer(obj: &object::Object) -> bool {
  match obj {
    object::Object::Integer(_) => true,
    _ => false,
  }
}

fn native_bool_to_boolean_object(input: bool) -> object::Object {
  if input {
    return TRUE;
  }
  FALSE
}

#[cfg(test)] 
mod tests {
  use crate::lexer::Lexer;
  use crate::parser::Parser;
  use super::*;

  #[test]
  fn test_eval_integer_expression() {
      let tests: Vec<(&str, i64)> = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
      ];

      for (input, expected) in tests.into_iter() {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
      }
  }

  #[test]
  fn test_eval_boolean_expression() {
      let tests: Vec<(&str, bool)> = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("true == true", true),
        ("false == true", false),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) != true", true),
        ("(1 > 2) != false", false),
      ];

      for (input, expected) in tests.into_iter() {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, expected);
      }
  }

  #[test]
  fn test_eval_bang_operator() {
      let tests: Vec<(&str, bool)> = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
      ];

      for (input, expected) in tests.into_iter() {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, expected);
      }
  }

  fn test_eval(input: &str) -> object::Object {
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();

    return eval(program);
  }

  fn test_integer_object(obj: object::Object, expected: i64) {
    let int = match &obj {
      object::Object::Integer(int) => int,
      _ => panic!("Object should has Integer, but got {:?}", obj),
    };

    assert_eq!(int.value, expected, "actual={}, expected={}", int.value, expected);
  }
  
  fn test_boolean_object(obj: object::Object, expected: bool) {
    let val = match &obj {
      object::Object::Boolean(val) => val,
      _ => panic!("Object should has Boolean, but got {:?}", obj),
    };

    assert_eq!(val.value, expected, "actual={}, expected={}", val.value, expected);
  }
}
