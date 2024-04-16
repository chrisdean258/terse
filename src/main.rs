use std::{env, process::ExitCode};
mod expression;
mod interpretter;
mod intrinsics;
mod lexer;
mod parser;
mod runner;
mod span;
mod token;
mod value;

fn main() -> ExitCode {
    let mut args = env::args();
    let _program = args.next().expect("no program name");
    let val = match args.next() {
        Some(program_file) => runner::read_and_run(&program_file).map(|_| ()),
        None => runner::repl(),
    };
    match val {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            println!("{e}");
            ExitCode::FAILURE
        }
    }
}
