use std::fmt;

pub fn write_object_list<T: fmt::Display>(list: &[T], f: &mut fmt::Formatter<'_>) -> fmt::Result {
  let mut iter = list.iter();    
  let mut next = iter.next();

  while let Some(val) = next {
    next = iter.next();
    write!(f, "{}", val)?;
    if let Some(_) = next {
      write!(f, ", ")?;
    }
  }

  Ok(())
}
