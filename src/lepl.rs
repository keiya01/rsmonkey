use std::io::{self, Write};

use crate::{lexer, parser};

fn log_token(buf: String) {
  let l = lexer::Lexer::new(buf);
  let mut p = parser::Parser::new(l);
  let program = p.parse_program();

  if p.check_parse_errors() {
    return;
  }

  println!("{}", program);
}

pub fn start() {
  loop {
    print!("> ");
    io::stdout().flush().unwrap();

    let mut buf = String::new();
    let stdin = io::stdin();
    stdin.read_line(&mut buf).unwrap();
    log_token(buf);
  }
}
