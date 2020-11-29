use crate::ast::Program;
use crate::ast::stmt::{Statement, BlockStatement};
use crate::ast::expr::{Expression, IfExpression};
use crate::ast::lit::{Literal};
use crate::ast::operator::{Prefix, Infix};

mod object;

const TRUE: object::Object = object::Object::Boolean(object::Boolean { value: true });
const FALSE: object::Object = object::Object::Boolean(object::Boolean { value: false });
const NULL: object::Object = object::Object::Null;

pub fn eval(node: Program) -> object::Object {
  eval_program(&node)
}

fn eval_program(node: &Program) -> object::Object {
  let mut result: object::Object = NULL;
  for stmt in &node.statements {
    result = match stmt {
      Statement::Expr(expr) => eval_expression(&expr.value),
      Statement::Return(rtn) => {
        let expr = eval_expression(&rtn.value);
        if is_error(&expr) {
          return expr;
        }
        object::Object::Return(
          object::Return::new(Box::new(expr)),
        )
      },
      _ => NULL,
    };

    if let object::Object::Return(return_obj) = result {
      return *return_obj.value;
    }

    if let object::Object::Error(_) = result {
      return result;
    }
  }
  result
}

fn eval_expression(expr: &Expression) -> object::Object {
  match expr {
    Expression::Literal(lit) => eval_literal(&lit),
    Expression::Prefix(pre) => {
      let right = eval_expression(&pre.right);
      if is_error(&right) {
        return right;
      }
      eval_prefix_expression(&pre.operator, right)
    },
    Expression::Infix(inf) => {
      let left = eval_expression(&inf.left);
      if is_error(&left) {
        return left;
      }
      let right = eval_expression(&inf.right);
      if is_error(&right) {
        return right;
      }
      eval_infix_expression(left, &inf.operator, right)
    },
    Expression::If(if_expr) => eval_if_expression(if_expr),
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
    _ => new_error(
      format!("unknown operator: -{}", right),
    ),
  }
}

fn eval_infix_expression(left: object::Object, operator: &Infix, right: object::Object) -> object::Object {
  if is_object_integer(&left) && is_object_integer(&right) {
    return eval_integer_infix_expression(left, operator, right);
  }

  match (&left, &right) {
    (object::Object::Boolean(_), object::Object::Boolean(_))
    | (object::Object::Null, object::Object::Null)
    // TODO: | (object::Object::String(_), object::Object::String(_))
    => (),
    _ => return new_error(
      format!("type mismatch: {} {} {}", left, operator, right),
    ),
  }

  match operator {
    Infix::Equal => native_bool_to_boolean_object(left == right),
    Infix::NotEq => native_bool_to_boolean_object(left != right),
    _ => new_error(
      format!("unknown operator: {} {} {}", left, operator, right),
    ),
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
    _ => return new_error(
      format!("unknown operator: {} {} {}", left, operator, right),
    ),
  };

  object::Object::Integer(int)
}

fn eval_if_expression(expr: &IfExpression) -> object::Object {
  let condition = eval_expression(&expr.condition);
  if is_error(&condition) {
    return condition;
  }

  if is_truthy(condition) {
    eval_block_statement(&expr.consequence)
  } else if let Some(alt) = &expr.alternative {
    eval_block_statement(&alt)
  } else {
    NULL
  }
}

fn eval_block_statement(block: &BlockStatement) -> object::Object {
  let mut result: object::Object = NULL;
  for stmt in &block.statements {
    result = match stmt {
      Statement::Expr(expr) => eval_expression(&expr.value),
      Statement::Return(rtn) => {
        let expr = eval_expression(&rtn.value);
        if is_error(&expr) {
          return expr;
        }
        object::Object::Return(
          object::Return::new(Box::new(expr)),
        )
      },
      _ => NULL,
    };

    match result {
      object::Object::Return(_)
      | object::Object::Error(_) => return result,
      _ => (),
    }
  }
  result
}

fn new_error(msg: String) -> object::Object {
  object::Object::Error(object::Error::new(msg))
}

fn is_error(obj: &object::Object) -> bool {
  match obj {
    object::Object::Error(_) => true,
    _ => false,
  }
}

fn is_object_integer(obj: &object::Object) -> bool {
  match obj {
    object::Object::Integer(_) => true,
    _ => false,
  }
}

fn is_truthy(obj: object::Object) -> bool {
  if obj == TRUE {
    true
  } else if obj == FALSE {
    false
  } else if obj == NULL {
    false
  } else {
    true
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

  #[test]
  fn test_if_else_expression() {
      let tests: Vec<(&str, Option<i64>)> = vec![
        ("if(true) { 10 }", Some(10)),
        ("if(false) { 10 }", None),
        ("if(1) { 10 }", Some(10)),
        ("if(1 < 2) { 10 }", Some(10)),
        ("if(1 > 2) { 10 }", None),
        ("if(1 > 2) { 10 } else { 20 }", Some(20)),
        ("if(1 < 2) { 10 } else { 20 }", Some(10)),
      ];

      for (input, expected) in tests.into_iter() {
        let evaluated = test_eval(input);
        match expected {
          Some(exp) => test_integer_object(evaluated, exp),
          None => test_null_object(evaluated),
        }
      }
  }

  #[test]
  fn test_return_expression() {
      let tests: Vec<(&str, i64)> = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("
if(10 > 1) {
  if(10 > 1) {
    return 10;
  }
  return 1;
}
", 10),
      ];

      for (input, expected) in tests.into_iter() {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
      }
  }

  #[test]
  fn test_error_handling() {
      let tests: Vec<(&str, &str)> = vec![
        ("5 + true", "type mismatch: 5 + true"),
        ("5 + true; 5;", "type mismatch: 5 + true"),
        ("-true", "unknown operator: -true"),
        ("true + false", "unknown operator: true + false"),
        ("5; true + false; 5", "unknown operator: true + false"),
        ("if(10 > 1) { true + false }", "unknown operator: true + false"),
        ("
if(10 > 1) {
  if(10 > 1) {
    return true + false;
  }
  return 1;
}
", "unknown operator: true + false"),
      ];

      for (input, expected) in tests.into_iter() {
        let evaluated = test_eval(input);
        match evaluated {
          object::Object::Error(err) => {
            if &err.value != expected {
              panic!("wrong error message. actual={}, expected={}", &err.value, expected);
            }
          }
          _ => panic!("Object should has Error, but got {:?}", evaluated),
        };
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

  fn test_null_object(obj: object::Object) {
    match &obj {
      object::Object::Null => (),
      _ => panic!("Object should has NULL object, but got {:?}", obj),
    };
  }
}
