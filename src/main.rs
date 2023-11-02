use std::io::{self, stdout};
use std::env;
use std::process;

extern crate interpreter;

fn main() {
    let user = env::var("USER").unwrap_or_else(|_| {
        eprintln!("Failed to get the current user.");
        process::exit(1);
    });

    println!("Hello {}! This is the Monkey programming language!", user);
    println!("Feel free to type in commands");
    interpreter::repl::start(io::stdin().lock(), &mut stdout().lock());
}

