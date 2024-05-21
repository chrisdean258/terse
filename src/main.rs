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
    let val = args.next().map_or_else(
        || runner::repl(false),
        |program_file| runner::read_and_run(&program_file).map(|_| ()),
    );
    match val {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            println!("{e}");
            ExitCode::FAILURE
        }
    }
}
