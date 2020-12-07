use std::rc::Rc;
use std::collections::HashMap;

use super::object::{Object, Builtin, Integer, Error};

pub fn new_builtins() -> Rc<HashMap<String, Object>> {
  let mut hash = HashMap::new();
  hash.insert("len".into(), Object::Builtin(Builtin::new(len)));
  Rc::new(hash)
}

fn len(args: Vec<Object>) -> Object {
  if args.len() != 1 {
    return Object::Error(
      Error::new(
        format!("wrong number of argument: got={}, want=1.", args.len()),
      )
    )
  }
  let obj = &args[0];
  match obj {
    Object::Str(s) => Object::Integer(Integer::new(s.value.len() as i64)),
    _ => Object::Error(Error::new(format!("argument to `len` not supported: got={}", obj))),
  }
}
