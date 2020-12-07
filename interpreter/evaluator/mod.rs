use std::rc::Rc;
use std::cell::RefCell;

use crate::ast::Program;
use crate::ast::stmt::{Statement, BlockStatement};
use crate::ast::expr::{Expression, IfExpression, CallExpression};
use crate::ast::ident::{Identifier};
use crate::ast::lit::{Literal};
use crate::ast::operator::{Prefix, Infix};

mod object;
pub mod environment;
pub mod builtins;

use environment::Environment;

const TRUE: object::Object = object::Object::Boolean(object::Boolean { value: true });
const FALSE: object::Object = object::Object::Boolean(object::Boolean { value: false });
const NULL: object::Object = object::Object::Null;

pub fn eval(node: Program, env: &Rc<RefCell<Environment>>) -> object::Object {
  eval_program(&node, env)
}

fn eval_program(node: &Program, env: &Rc<RefCell<Environment>>) -> object::Object {
  let mut result: object::Object = NULL;
  for stmt in &node.statements {
    result = eval_statement(stmt, env);

    if let object::Object::Return(return_obj) = result {
      return *return_obj.value;
    }

    if let object::Object::Error(_) = result {
      return result;
    }
  }
  result
}

fn eval_expression(expr: &Expression, env: &Rc<RefCell<Environment>>) -> object::Object {
  match expr {
    Expression::Literal(lit) => eval_literal(&lit, env),
    Expression::Prefix(pre) => {
      let right = eval_expression(&pre.right, env);
      if is_error(&right) {
        return right;
      }
      eval_prefix_expression(&pre.operator, right)
    },
    Expression::Infix(inf) => {
      let left = eval_expression(&inf.left, env);
      if is_error(&left) {
        return left;
      }
      let right = eval_expression(&inf.right, env);
      if is_error(&right) {
        return right;
      }
      eval_infix_expression(left, &inf.operator, right)
    },
    Expression::Index(idx) => {
      let left = eval_expression(&idx.left, env);
      if is_error(&left) {
        return left;
      }
      let index = eval_expression(&idx.index, env);
      if is_error(&index) {
        return index;
      }
      eval_index_expression(left, index)
    },
    Expression::If(if_expr) => eval_if_expression(if_expr, env),
    Expression::Identifier(ident) => eval_ident_expression(ident, env),
    Expression::Call(call) => eval_call_expression(call, env),
  }
}

fn eval_literal(lit: &Literal, env: &Rc<RefCell<Environment>>) -> object::Object {
  match lit {
    Literal::Integer(int) => object::Object::Integer(
      object::Integer::new(int.value),
    ),
    Literal::Boolean(val) => native_bool_to_boolean_object(val.value),
    Literal::Str(val) => object::Object::Str(
      object::Str::new(val.value.clone()),
    ),
    Literal::Array(val) => {
      let mut elms = eval_expressions(&val.elements, env);
      if elms.len() == 1 && is_error(&elms[0]) {
        return elms.pop().unwrap();
      }
      object::Object::Array(
        object::Array::new(elms),
      )
    },
    Literal::Func(func) => object::Object::Func(
      object::Func::new(func.args.clone(), func.body.clone(), Rc::clone(env))
    ),
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
      format!("unknown operator: -{}.", right),
    ),
  }
}

fn eval_infix_expression(left: object::Object, operator: &Infix, right: object::Object) -> object::Object {
  if let (object::Object::Integer(_), object::Object::Integer(_)) = (&left, &right) {
    return eval_integer_infix_expression(left, operator, right);
  }

  if let (object::Object::Str(_), object::Object::Str(_)) = (&left, &right) {
    return eval_string_infix_expression(left, operator, right);
  }

  let is_eq = match (&left, &right) {
    (object::Object::Boolean(left), object::Object::Boolean(right)) => left.value == right.value,
    (object::Object::Null, object::Object::Null) => true,
    _ => return new_error(
      format!("type mismatch: {} {} {}.", left, operator, right),
    ),
  };

  match operator {
    Infix::Equal => native_bool_to_boolean_object(is_eq),
    Infix::NotEq => native_bool_to_boolean_object(!is_eq),
    _ => new_error(
      format!("unknown operator: {} {} {}.", left, operator, right),
    ),
  }
}

fn eval_index_expression(left: object::Object, index: object::Object) -> object::Object {
  let index = match index {
    object::Object::Integer(i) => i,
    _ => return new_error(format!("specified index type is not supported: {}", index)),
  };
  match left {
    object::Object::Array(arr) => eval_array_index_expression(arr, index),
    _ => new_error(format!("index operator not supported: {}", left)),
  }
}

#[allow(unused_comparisons)]
fn eval_array_index_expression(arr: object::Array, idx: object::Integer) -> object::Object {
  let idx = idx.value as usize;
  if idx < 0 || idx > arr.elements.len() - 1 {
    return NULL;
  }
  arr.elements[idx].clone()
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
      format!("unknown operator: {} {} {}.", left, operator, right),
    ),
  };

  object::Object::Integer(int)
}

fn eval_string_infix_expression(left: object::Object, operator: &Infix, right: object::Object) -> object::Object {
  let (left, right) = if let (object::Object::Str(left), object::Object::Str(right)) = (left, right) {
    (left.value, right.value)
  } else {
    return NULL;
  };

  let s = match operator {
    Infix::Plus => object::Str::new(format!("{}{}", left, right)),
    Infix::Equal => return native_bool_to_boolean_object(left == right),
    Infix::NotEq => return native_bool_to_boolean_object(left != right),
    _ => return new_error(
      format!("unknown operator: \"{}\" {} \"{}\".", left, operator, right),
    ),
  };

  object::Object::Str(s)
}

fn eval_if_expression(expr: &IfExpression, env: &Rc<RefCell<Environment>>) -> object::Object {
  let condition = eval_expression(&expr.condition, env);
  if is_error(&condition) {
    return condition;
  }

  if is_truthy(condition) {
    eval_block_statement(&expr.consequence, env)
  } else if let Some(alt) = &expr.alternative {
    eval_block_statement(&alt, env)
  } else {
    NULL
  }
}

fn eval_block_statement(block: &BlockStatement, env: &Rc<RefCell<Environment>>) -> object::Object {
  let mut result: object::Object = NULL;
  for stmt in &block.statements {
    result = eval_statement(stmt, env);

    match result {
      object::Object::Return(_)
      | object::Object::Error(_) => return result,
      _ => (),
    }
  }
  result
}

fn eval_statement(stmt: &Statement, env: &Rc<RefCell<Environment>>) -> object::Object {
  match stmt {
    Statement::Expr(expr) => eval_expression(&expr.value, env),
    Statement::Return(rtn) => {
      let expr = eval_expression(&rtn.value, env);
      if is_error(&expr) {
        return expr;
      }
      object::Object::Return(
        object::Return::new(Box::new(expr)),
      )
    },
    Statement::Let(let_stmt) => {
      let expr = eval_expression(&let_stmt.value, env);
      if is_error(&expr) {
        return expr;
      }
      if let Some(_) = env.borrow().get_builtin(&let_stmt.ident.value) {
        return new_error(format!("`{}` is already used as a builtin function.", &let_stmt.ident.value))
      }
      env.borrow_mut().set(&let_stmt.ident.value, expr.clone());
      expr
    },
    _ => NULL,
  }
}

fn eval_ident_expression(ident: &Identifier, env: &Rc<RefCell<Environment>>) -> object::Object {
  match env.borrow().get(&ident.value) {
    Some(val) => return val.clone(),
    None => (),
  }

  match env.borrow().get_builtin(&ident.value) {
    Some(val) => val.clone(),
    None => new_error(format!("identifier not found: {}.", ident.value)),
  }
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

fn eval_call_expression(call: &CallExpression, env: &Rc<RefCell<Environment>>) -> object::Object {
  let func = eval_expression(&call.func, env);
  if is_error(&func) {
    return func;
  }

  let mut args = eval_expressions(&call.args, env);
  if args.len() == 1 && is_error(&args[0]) {
    return args.pop().unwrap();
  }

  return apply_func(func, args);
}

fn eval_expressions(args: &Vec<Expression>, env: &Rc<RefCell<Environment>>) -> Vec<object::Object> {
  let mut result: Vec<object::Object> = vec![];
  for arg in args {
    let evaluated = eval_expression(arg, env);
    if is_error(&evaluated) {
      return vec![evaluated];
    }
    result.push(evaluated);
  }
  result
}

fn apply_func(obj: object::Object, args: Vec<object::Object>) -> object::Object {
  let func = match obj {
    object::Object::Func(func) => func,
    object::Object::Builtin(builtin) => return (builtin.func)(args),
    _ => return new_error(format!("not a function: {}.", obj)),
  };

  let env = Environment::new_enclosed_env(Rc::clone(&func.env));
  for (i, arg) in args.into_iter().enumerate() {
    (&env).borrow_mut().set(&func.args[i].value, arg);
  }

  let evaluated = eval_block_statement(&func.body, &env);

  unwrap_returned_value(evaluated)
}

fn unwrap_returned_value(obj: object::Object) -> object::Object {
  if let object::Object::Return(rtn) = obj {
    return *rtn.value;
  }
  obj
}

fn is_truthy(obj: object::Object) -> bool {
  match obj {
    object::Object::Boolean(val) => val.value,
    object::Object::Null => false,
    _ => true,
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
        ("\"hello\" == \"hello\"", true),
        ("\"hello\" == \"world\"", false),
        ("\"hello\" != \"world\"", true),
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
  fn test_eval_string_expression() {
      let tests: Vec<(&str, &str)> = vec![
        ("\"Hello World!\"", "Hello World!"),
      ];

      for (input, expected) in tests.into_iter() {
        let evaluated = test_eval(input);
        let str_lit = match evaluated {
          object::Object::Str(s) => s,
          _ => panic!("Object should has Str, but got {}", evaluated),
        };
        assert_eq!(&str_lit.value, expected, "actual={}, expected={}", &str_lit.value, expected);
      }
  }

  #[test]
  fn test_string_concatenation() {
      let tests: Vec<(&str, &str)> = vec![
        ("\"Hello\" + \" \" + \"World!\"", "Hello World!"),
      ];

      for (input, expected) in tests.into_iter() {
        let evaluated = test_eval(input);
        let str_lit = match evaluated {
          object::Object::Str(s) => s,
          _ => panic!("Object should has Str, but got {}", evaluated),
        };
        assert_eq!(&str_lit.value, expected, "actual={}, expected={}", &str_lit.value, expected);
      }
  }

  #[test]
  fn test_eval_array_expression() {
      let input = "[1, 2 * 2, 3 + 3]";

      let evaluated = test_eval(input);
      let arr_lit = match evaluated {
        object::Object::Array(arr) => arr,
        _ => panic!("Object should has Array, but got {}", evaluated),
      };

      if arr_lit.elements.len() != 3 {
        panic!("array should has 3 items, but got {} items", arr_lit.elements.len());
      }

      test_integer_object(arr_lit.elements[0].clone(), 1);
      test_integer_object(arr_lit.elements[1].clone(), 4);
      test_integer_object(arr_lit.elements[2].clone(), 6);
  }

  #[test]
  fn test_eval_index_expression() {
      let tests: Vec<(&str, Option<i64>)> = vec![
        ("[1, 2, 3][0]", Some(1)),
        ("[1, 2, 3][1]", Some(2)),
        ("[1, 2, 3][2]", Some(3)),
        ("let i = 0; [1][i]", Some(1)),
        ("[1, 2, 3][1 + 1]", Some(3)),
        ("let arr = [1, 2, 3]; arr[2]", Some(3)),
        ("let arr = [1, 2, 3]; arr[0] + arr[1] + arr[2];", Some(6)),
        ("let arr = [1, 2, 3]; let i = arr[0]; arr[i];", Some(2)),
        ("[1, 2, 3][3]", None),
        ("[1, 2, 3][-1]", None),
      ];

      for (input, expected) in tests.into_iter() {
        let evaluated = test_eval(input);
        match expected {
          Some(val) => test_integer_object(evaluated, val),
          None => test_null_object(evaluated),
        };
      }
  }

  #[test]
  fn test_eval_bang_operator() {
      let tests: Vec<(&str, bool)> = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!\"hello\"", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
        ("!!\"hello\"", true),
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
  fn test_let_statement() {
      let tests: Vec<(&str, i64)> = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
      ];

      for (input, expected) in tests.into_iter() {
        test_integer_object(test_eval(input), expected);
      }
  }

  #[test]
  fn test_func_object() {
      let evaluated = test_eval("fn(x) { x + 2 }");
      let func = match evaluated {
        object::Object::Func(func) => func,
        _ => panic!("Object should has Func, but got {:?}", evaluated)
      };

      if func.args.len() != 1 {
        panic!("Func should has 1 args, but got {}", func.args.len());
      }

      if &func.args[0].value != "x" {
        panic!("func.args[0] should be 'x', but got {}", func.args[0].value);
      }

      if &format!("{}", func.body) != "{ (x + 2) }" {
        panic!("func.body should be '(x + 2)', but got {}", func.body);
      }
  }

  #[test]
  fn test_func_application() {
      let tests: Vec<(&str, i64)> = vec![
        ("let f = fn(x) { x; }; f(5);", 5),
        ("let f = fn() { 5; }; f();", 5),
        ("let f = fn(x) { return x; }; f(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("let add = fn(x, y) { if(x > 10) { x + y } else { add(x + 1, y) }; }; add(0, 9);", 20),
      ];

      for (input, expected) in tests.into_iter() {
        test_integer_object(test_eval(input), expected);
      }
  }

  #[test]
  fn test_closure() {
      let input = "
let new_adder = fn(x) {
  fn(y) { x + y };
};
let f = new_adder(2);
f(3)
";

      test_integer_object(test_eval(input), 5);
  }

  #[test]
  fn test_buildin_functions() {
      let tests: Vec<(&str, i64)> = vec![
        ("len(\"\")", 0),
        ("len(\"four\")", 4),
        ("len(\"hello world\")", 11),
        ("let f = fn() { len(\"abc\") }; f()", 3),
      ];

      for (input, expected) in tests.into_iter() {
        test_integer_object(test_eval(input), expected);
      }
  }

  #[test]
  fn test_error_handling() {
      let tests: Vec<(&str, &str)> = vec![
        ("5 + true", "type mismatch: 5 + true."),
        ("5 + true; 5;", "type mismatch: 5 + true."),
        ("-true", "unknown operator: -true."),
        ("true + false", "unknown operator: true + false."),
        ("5; true + false; 5", "unknown operator: true + false."),
        ("if(10 > 1) { true + false }", "unknown operator: true + false."),
        ("foobar", "identifier not found: foobar."),
        ("\"hello\" - \"world\"", "unknown operator: \"hello\" - \"world\"."),
        ("len(\"one\", \"two\")", "wrong number of argument: got=2, want=1."),
        ("len(1)", "argument to `len` not supported: got=1"),
        ("let len = 0", "`len` is already used as a builtin function."),
        ("
if(10 > 1) {
  if(10 > 1) {
    return true + false;
  }
  return 1;
}
", "unknown operator: true + false."),
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
    if !p.check_parse_errors() {
      panic!();
    }
    let mut env = Rc::new(Environment::new(builtins::new_builtins()));

    return eval(program, &mut env);
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
