use std::{env, fs};

use src::{evaluator, start, exec};

fn main() {
    let mut environment = evaluator::environment::Environment::new();
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
