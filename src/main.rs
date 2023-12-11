use std::{env, error::Error, fs::read_to_string, process::ExitCode};
mod expression;
mod interpretter;
mod intrinsics;
mod lexer;
mod parser;
mod span;
mod token;
mod value;

fn usage() -> ExitCode {
    eprintln!("Usage: terse <program_file>");
    ExitCode::FAILURE
}

fn main() -> ExitCode {
    let mut args = env::args();
    let _program = args.next().expect("no program name");
    let Some(program_file) = args.next() else {
        return usage();
    };
    match read_and_run(&program_file) {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            println!("{e}");
            ExitCode::FAILURE
        }
    }
}

fn read_and_run(filename: &str) -> Result<value::Value, Box<dyn Error>> {
    let contents = read_to_string(filename)?;
    run(filename, &contents)
}

fn run(name: &str, program: &str) -> Result<value::Value, Box<dyn Error>> {
    let stdlib = read_to_string(env::var("HOME")? + "/git/terse/stdlib/stdlib.trs")?;
    let l = lexer::Lexer::new("stdlib".to_owned(), stdlib.chars().collect());
    let t = parser::parse(l)?;
    let mut intp = interpretter::Interpretter::new();
    intp.interpret(&t)?;

    let l = lexer::Lexer::new(name.to_owned(), program.chars().collect());
    // for t in l {
    // println!("{:?}", t?.value);
    // }
    // return Ok(value::Value::None);
    let t = parser::parse(l)?;
    // eprintln!("{t}");
    Ok(intp.interpret(&t)?)
}
