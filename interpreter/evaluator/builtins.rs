use std::collections::HashMap;

use super::object::*;

pub fn new_builtins() -> HashMap<String, Object> {
  let mut hash = HashMap::new();
  hash.insert("len".into(), Object::Builtin(Builtin::new(len)));
  hash.insert("first".into(), Object::Builtin(Builtin::new(first)));
  hash.insert("last".into(), Object::Builtin(Builtin::new(last)));
  hash.insert("rest".into(), Object::Builtin(Builtin::new(rest)));
  hash.insert("push".into(), Object::Builtin(Builtin::new(push)));
  hash.insert("insert".into(), Object::Builtin(Builtin::new(insert)));
  hash.insert("remove".into(), Object::Builtin(Builtin::new(remove)));
  hash
}

fn len(args: Vec<Object>) -> Object {
  if args.len() != 1 {
    return new_error(format!("wrong number of argument: got={}, want=1.", args.len()));
  }

  let obj = &args[0];
  match obj {
    Object::Str(s) => Object::Integer(Integer::new(s.value.len() as i64)),
    Object::Array(arr) => Object::Integer(Integer::new(arr.elements.len() as i64)),
    Object::Hash(hash) => Object::Integer(Integer::new(hash.pairs.len() as i64)),
    _ => new_error(format!("argument to `len` not supported: got={}", obj)),
  }
}

fn first(args: Vec<Object>) -> Object {
  if args.len() != 1 {
    return new_error(
      format!("wrong number of argument: got={}, want=1.", args.len())
    );
  }

  let obj = &args[0];
  match obj {
    Object::Array(arr) => {
      if arr.elements.len() == 0 {
        return Object::Null;
      }
      arr.elements[0].clone()
    },
    _ => new_error(format!("argument to `first` must be ARRAY: got={}", obj)),
  }
}

fn last(args: Vec<Object>) -> Object {
  if args.len() != 1 {
    return new_error(
      format!("wrong number of argument: got={}, want=1.", args.len())
    );
  }

  let obj = &args[0];
  match obj {
    Object::Array(arr) => {
      let len = arr.elements.len();
      if len == 0 {
        return Object::Null;
      }
      arr.elements[len - 1].clone()
    },
    _ => new_error(format!("argument to `last` must be ARRAY: got={}", obj)),
  }
}

fn rest(args: Vec<Object>) -> Object {
  if args.len() != 1 {
    return new_error(
      format!("wrong number of argument: got={}, want=1.", args.len())
    );
  }

  let obj = &args[0];
  match obj {
    Object::Array(arr) => {
      let len = arr.elements.len();
      if len == 0 {
        return Object::Null;
      }
      Object::Array(Array::new(arr.elements[1..len].to_vec()))
    },
    _ => new_error(format!("argument to `rest` must be ARRAY: got={}", obj)),
  }
}

fn push(args: Vec<Object>) -> Object {
  if args.len() != 2 {
    return new_error(
      format!("wrong number of argument: got={}, want=2.", args.len())
    );
  }

  let obj = &args[0];
  match obj {
    Object::Array(arr) => {
      let mut new_elements = arr.elements.clone();
      new_elements.push(args[1].clone());
      Object::Array(Array::new(new_elements))
    },
    _ => new_error(format!("argument to `push` must be ARRAY: got={}", obj)),
  }
}

fn insert(args: Vec<Object>) -> Object {
  if args.len() != 3 {
    return new_error(
      format!("wrong number of argument: got={}, want=3.", args.len())
    );
  }

  let obj = &args[0];
  let key = &args[1];
  let val = &args[2];
  match obj {
    Object::Hash(hash) => {
      let mut new_hash = hash.pairs.clone();
      new_hash.insert(key.clone(), val.clone());
      Object::Hash(Hash::new(new_hash))
    },
    _ => new_error(format!("argument to `insert` must be HASH: got={}", obj)),
  }
}

fn remove(args: Vec<Object>) -> Object {
  if args.len() != 2 {
    return new_error(
      format!("wrong number of argument: got={}, want=2.", args.len())
    );
  }

  let obj = &args[0];
  let key = &args[1];
  match obj {
    Object::Hash(hash) => {
      if hash.pairs.len() == 0 {
        return obj.clone();
      }
      let mut new_hash = hash.pairs.clone();
      new_hash.remove(key);
      Object::Hash(Hash::new(new_hash))
    },
    _ => new_error(format!("argument to `remove` must be HASH: got={}", obj)),
  }
}

fn new_error(msg: String) -> Object {
  Object::Error(Error::new(msg))
}
