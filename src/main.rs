use std::{env, error::Error};
mod expression;
mod interpretter;
mod lexer;
mod parser;
mod span;
mod token;
mod value;

fn main() {
    let mut args = env::args();
    let _program = args.next().expect("no program name");
    let test = args.next().unwrap_or("true && \"test\"".into());
    match run(&test) {
        Ok(v) => println!("{v}"),
        Err(e) => println!("{e}"),
    }
}

fn run(test: &str) -> Result<value::Value, Box<dyn Error>> {
    let l = lexer::Lexer::new("test".to_owned(), test.chars());
    let t = parser::parse(l)?;
    let mut intp = interpretter::Interpretter::new();
    Ok(intp.interpret(&t)?)
}
