use std::rc::Rc;
use std::cell::RefCell;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::{lexer, parser, evaluator};

fn log_token(buf: String, env: &mut Rc<RefCell<evaluator::environment::Environment>>) {
  let l = lexer::Lexer::new(buf);
  let mut p = parser::Parser::new(l);
  let program = p.parse_program();
  
  if !p.check_parse_errors() {
    return;
  }
  
  println!("{}", evaluator::eval(program, env));
}

pub fn start() {
  let mut rl = Editor::<()>::new();
  let mut env = evaluator::environment::Environment::new();
  loop {
    let readline = rl.readline(">> ");
    match readline {
      Ok(line) => {
        log_token(line, &mut env);
      },
      Err(ReadlineError::Interrupted) => {
        println!("CTRL-C");
        break
      },
      Err(ReadlineError::Eof) => {
        println!("CTRL-D");
        break
      },
      Err(err) => {
        println!("Error: {:?}", err);
        break
      }
    }
  }
}
