use std::rc::Rc;
use std::cell::RefCell;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::{env, fs};

use interpreter::{evaluator, lexer, parser};
use evaluator::builtins;
use evaluator::environment::{Environment};

fn exec(buf: String, env: &mut Rc<RefCell<evaluator::environment::Environment>>) {
  let l = lexer::Lexer::new(buf);
  let mut p = parser::Parser::new(l);
  let program = p.parse_program();
  
  if !p.check_parse_errors() {
    return;
  }
  
  println!("{}", evaluator::eval(program, env));
}
  
fn start(env: &mut Rc<RefCell<evaluator::environment::Environment>>) {
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

fn main() {
    let mut environment = Environment::new(builtins::new_builtins());
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let filename = &args[1];
        let contents = fs::read_to_string(filename)
            .expect("Something went wrong reading the file");
        exec(contents, &mut environment);
    } else {
        start(&mut environment);
    }
}
