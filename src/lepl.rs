use std::io::{self, Write};

use crate::{lexer, token};

fn log_token(buf: String) {
  let mut l = lexer::Lexer::new(buf);
  loop {
    let t = l.next_token();
    if let token::Token::EOF = t {
      break;
    }
    println!("{:?}", t);
  }
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
