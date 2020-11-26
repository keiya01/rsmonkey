use std::fmt;
use std::cmp::PartialEq;

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
  pub value: String,
}

impl Identifier {
  pub fn new(value: String) -> Identifier {
    Identifier { value }
  }
}

impl fmt::Display for Identifier {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", &self.value)
  }
}
