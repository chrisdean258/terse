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
    let Some(program_file) = args.next() else {
        return match runner::repl() {
            Ok(_) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("{e}");
                ExitCode::FAILURE
            }
        };
    };
    match runner::read_and_run(&program_file) {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            println!("{e}");
            ExitCode::FAILURE
        }
    }
}
