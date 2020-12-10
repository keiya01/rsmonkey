use std::cmp::PartialEq;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
  ILLEGAL,
  EOF,
  
  // 識別子 + リテラル
  IDENT(String),
  INT(i64),
  STRING(String),
  
  // 演算子
  ASSIGN,
  PLUS,
  MINUS,
  BANG,
  ASTERISK,
  SLASH,

  LT,
  GT,
  EQ,
  NotEq,
  
  // デリミタ
  COMMA,
  COLON,
  SEMICOLON,
  
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  LBRACKET,
  RBRACKET,
  
  // キーワード
  FUNCTION,
  LET,
  TRUE,
  FALSE,
  IF,
  ELSE,
  RETURN,
  COMMENT(String),
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Token::ILLEGAL => write!(f, "ILLEGAL"),
      Token::EOF => write!(f, "EOF"),

      // 識別子 + リテラル
      Token::IDENT(s) => write!(f, "IDENT({})", s),
      Token::INT(i) => write!(f, "INT({})", i),
      Token::STRING(s) => write!(f, "STRING({})", s),
      
      // 演算子
      Token::ASSIGN => write!(f, "ASSIGN"),
      Token::PLUS => write!(f, "PLUS"),
      Token::MINUS => write!(f, "MINUS"),
      Token::BANG => write!(f, "BANG"),
      Token::ASTERISK => write!(f, "ASTERISK"),
      Token::SLASH => write!(f, "SLASH"),
    
      Token::LT => write!(f, "LT"),
      Token::GT => write!(f, "GT"),
      Token::EQ => write!(f, "EQ"),
      Token::NotEq => write!(f, "NotEq"),
      
      // デリミタ
      Token::COMMA => write!(f, "COMMA"),
      Token::COLON => write!(f, "COLON"),
      Token::SEMICOLON => write!(f, "SEMICOLON"),
      
      Token::LPAREN => write!(f, "LPAREN"),
      Token::RPAREN => write!(f, "RPAREN"),
      Token::LBRACE => write!(f, "LBRACE"),
      Token::RBRACE => write!(f, "RBRACE"),
      Token::LBRACKET => write!(f, "LBRACKET"),
      Token::RBRACKET => write!(f, "RBRACKET"),
      
      // キーワード
      Token::FUNCTION => write!(f, "FUNCTION"),
      Token::LET => write!(f, "LET"),
      Token::TRUE => write!(f, "TRUE"),
      Token::FALSE => write!(f, "FALSE"),
      Token::IF => write!(f, "IF"),
      Token::ELSE => write!(f, "ELSE"),
      Token::RETURN => write!(f, "RETURN"),
      Token::COMMENT(s) => write!(f, "COMMENT({})", s),
    }
  }
}
