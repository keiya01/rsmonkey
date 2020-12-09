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

pub fn format_object_list<T: fmt::Display>(list: &[T], sep: &str) -> String {
  let mut output = String::new();
  
  let mut iter = list.iter();    
  let mut next = iter.next();

  while let Some(val) = next {
    next = iter.next();
    output.push_str(&format!("{}", val));
    if let Some(_) = next {
      output.push_str(&format!("{}", sep));
    }
  }

  output
}
