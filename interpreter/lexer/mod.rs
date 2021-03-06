use crate::token;

#[derive(Debug)]
pub struct Lexer {
  input: String,
  position: usize,
  read_position: usize,
  ch: u8,
}

impl Lexer {
  pub fn new(input: String) -> Lexer {
    let mut l = Lexer {
      input,
      position: 0,
      read_position: 0,
      ch: 0,
    };
    &l.read_char();
    l
  }

  fn read_char(&mut self) {
    if self.read_position >= self.input.len() {
      self.ch = 0;
    } else {
      self.ch = self.input.as_bytes()[self.read_position];
    }
    self.position = self.read_position;
    self.read_position += 1;
  }

  fn peek_char(&self) -> u8 {
    if self.read_position >= self.input.len() {
      0
    } else {
      self.input.as_bytes()[self.read_position]
    }
  }

  fn skip_whitespace(&mut self) {
    while let b' ' | b'\t' | b'\n' | b'\r' = self.ch {
      self.read_char()
    }
  }

  pub fn next_token(&mut self) -> token::Token {
    self.skip_whitespace();

    let tok = match self.ch {
      b'=' => {
        if let b'=' = self.peek_char() {
          self.read_char();
          token::Token::EQ
        } else {
          token::Token::ASSIGN
        }
      },
      b'!' => {
        if let b'=' = self.peek_char() {
          self.read_char();
          token::Token::NotEq
        } else {
          token::Token::BANG
        }
      },
      b':' => token::Token::COLON,
      b';' => token::Token::SEMICOLON,
      b'(' => token::Token::LPAREN,
      b')' => token::Token::RPAREN,
      b'{' => token::Token::LBRACE,
      b'}' => token::Token::RBRACE,
      b'[' => token::Token::LBRACKET,
      b']' => token::Token::RBRACKET,
      b',' => token::Token::COMMA,
      b'+' => token::Token::PLUS,
      b'-' => token::Token::MINUS,
      b'*' => token::Token::ASTERISK,
      b'/' => self.read_slash(),
      b'<' => token::Token::LT,
      b'>' => token::Token::GT,
      b'"' => self.read_string(),
      b'0'..=b'9' => return self.read_int(),
      b'a'..=b'z' | b'A'..=b'Z' | b'_' => return self.read_ident(),
      0 => token::Token::EOF,
      _ => token::Token::ILLEGAL,
    };
    self.read_char();
    tok
  }

  fn read_ident(&mut self) -> token::Token {
    let position = self.position;
    while let b'a'..=b'z' | b'A'..=b'Z' | b'_' = self.ch {
      self.read_char();
    };
    
    let ident = &self.input[position..self.position];

    match ident {
      "let" => token::Token::LET,
      "fn" => token::Token::FUNCTION,
      "true" => token::Token::TRUE,
      "false" => token::Token::FALSE,
      "if" => token::Token::IF,
      "else" => token::Token::ELSE,
      "return" => token::Token::RETURN,
      _ => token::Token::IDENT(ident.to_string()),
    }
  }

  fn read_int(&mut self) -> token::Token {
    let position = self.position;
    while let b'0'..=b'9' = self.ch {
      self.read_char();
    }

    let int = &self.input[position..self.position];

    token::Token::INT(int.parse().unwrap())
  }

  fn read_string(&mut self) -> token::Token {
    let position = self.position + 1;
    loop {
      self.read_char();
      if let b'"' = self.ch {
        break;
      }
      if let 0 = self.ch {
        break;
      }
    }
    let str_lit = &self.input[position..self.position];
    token::Token::STRING(str_lit.to_string())
  }

  fn read_slash(&mut self) -> token::Token {
    if let b'/' = self.peek_char() {
      self.read_char();
      self.read_char();
    } else {
      return token::Token::SLASH;
    }

    // skip whitespace
    if let b' ' | b'\t' = self.ch {
      self.read_char();
    }

    let position = self.position;
    loop {
      self.read_char();
      if let b'\n' = self.ch {
        break;
      }
      if let 0 = self.ch {
        break;
      }
    }
    let comment = &self.input[position..self.position];
    token::Token::COMMENT(comment.into())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn get_next_token() {
      let input = "let five = 5;
let ten = 10;

// function
let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;
\"foobar\"
\"foo bar\"
[1, 2];
{\"foo\": \"bar\"}
// foo bar";

      let tests: Vec<token::Token> = vec![
        token::Token::LET,
        token::Token::IDENT("five".to_string()),
        token::Token::ASSIGN,
        token::Token::INT(5),
        token::Token::SEMICOLON,
        token::Token::LET,
        token::Token::IDENT("ten".to_string()),
        token::Token::ASSIGN,
        token::Token::INT(10),
        token::Token::SEMICOLON,
        token::Token::COMMENT("function".into()),
        token::Token::LET,
        token::Token::IDENT("add".to_string()),
        token::Token::ASSIGN,
        token::Token::FUNCTION,
        token::Token::LPAREN,
        token::Token::IDENT("x".to_string()),
        token::Token::COMMA,
        token::Token::IDENT("y".to_string()),
        token::Token::RPAREN,
        token::Token::LBRACE,
        token::Token::IDENT("x".to_string()),
        token::Token::PLUS,
        token::Token::IDENT("y".to_string()),
        token::Token::SEMICOLON,
        token::Token::RBRACE,
        token::Token::SEMICOLON,
        token::Token::LET,
        token::Token::IDENT("result".to_string()),
        token::Token::ASSIGN,
        token::Token::IDENT("add".to_string()),
        token::Token::LPAREN,
        token::Token::IDENT("five".to_string()),
        token::Token::COMMA,
        token::Token::IDENT("ten".to_string()),
        token::Token::RPAREN,
        token::Token::SEMICOLON,
        token::Token::BANG,
        token::Token::MINUS,
        token::Token::SLASH,
        token::Token::ASTERISK,
        token::Token::INT(5),
        token::Token::SEMICOLON,
        token::Token::INT(5),
        token::Token::LT,
        token::Token::INT(10),
        token::Token::GT,
        token::Token::INT(5),
        token::Token::SEMICOLON,
        token::Token::IF,
        token::Token::LPAREN,
        token::Token::INT(5),
        token::Token::LT,
        token::Token::INT(10),
        token::Token::RPAREN,
        token::Token::LBRACE,
        token::Token::RETURN,
        token::Token::TRUE,
        token::Token::SEMICOLON,
        token::Token::RBRACE,
        token::Token::ELSE,
        token::Token::LBRACE,
        token::Token::RETURN,
        token::Token::FALSE,
        token::Token::SEMICOLON,
        token::Token::RBRACE,
        token::Token::INT(10),
        token::Token::EQ,
        token::Token::INT(10),
        token::Token::SEMICOLON,
        token::Token::INT(10),
        token::Token::NotEq,
        token::Token::INT(9),
        token::Token::SEMICOLON,
        token::Token::STRING("foobar".to_string()),
        token::Token::STRING("foo bar".to_string()),
        token::Token::LBRACKET,
        token::Token::INT(1),
        token::Token::COMMA,
        token::Token::INT(2),
        token::Token::RBRACKET,
        token::Token::SEMICOLON,
        token::Token::LBRACE,
        token::Token::STRING("foo".to_string()),
        token::Token::COLON,
        token::Token::STRING("bar".to_string()),
        token::Token::RBRACE,
        token::Token::COMMENT("foo bar".into()),
        token::Token::EOF,
      ];

      let mut l = Lexer::new(input.to_string());

      for tt in tests {
        let tok = l.next_token();
        assert_eq!(tok, tt, "Token type is wrong: expect={:?}, actual={:?}", tt, tok);
      }
  }
}
