use std::rc::Rc;
use std::cell::RefCell;

use rustyline::error::ReadlineError;
use rustyline::Editor;

pub mod lexer;
pub mod token;
pub mod ast;
pub mod parser;
pub mod evaluator;

pub fn exec(buf: String, env: &mut Rc<RefCell<evaluator::environment::Environment>>) {
  let l = lexer::Lexer::new(buf);
  let mut p = parser::Parser::new(l);
  let program = p.parse_program();
  
  if !p.check_parse_errors() {
    return;
  }
  
  println!("{}", evaluator::eval(program, env));
}

pub fn start(env: &mut Rc<RefCell<evaluator::environment::Environment>>) {
  let mut rl = Editor::<()>::new();
  loop {
    let readline = rl.readline(">> ");
    match readline {
      Ok(line) => exec(line, env),
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
