use std::io::{Write, BufRead, BufReader};
use crate::eval::eval;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::env::Environment;

const PROMPT: &str = ">> ";

pub fn start<R: BufRead, W: Write>(input: R, output: &mut W) {
    let mut reader = BufReader::new(input);
    let mut env = Environment::new();

    loop {
        write!(output, "{}", PROMPT).unwrap();
        output.flush().unwrap();

        let mut line = String::new();
        if reader.read_line(&mut line).unwrap() == 0 {
            // End of file or an error.
            return;
        }

        // Trim whitespace and check if the input is "exit" or "quit"
        let trimmed_line = line.trim();
        if trimmed_line == "exit" || trimmed_line == "quit" {
            write!(output, "Exiting...\n").unwrap();
            return; // Exit the loop and the function
        }

        let lexer = Lexer::new(&line);
        let mut p = Parser::new(lexer);
        let program = p.parse_program();
        if p.errors().len() != 0 {
            print_parse_errors(output, p.errors());
            continue;
        }
        let evaluated = eval(&program, &mut env);
        if evaluated.inspect() != "" {
            write!(output, "{}\n", evaluated.inspect()).unwrap();
        }
    }
}

fn print_parse_errors<W: Write>(output: &mut W, errors: &Vec<String>) {
    for error in errors {
        write!(output, "\t{}\n", error).unwrap();
    }
}