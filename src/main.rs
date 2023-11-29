use std::error::Error;
mod expression;
mod interpretter;
mod lexer;
mod parser;
mod span;
mod token;
mod value;

fn main() {
    match run() {
        Ok(v) => println!("{v}"),
        Err(e) => println!("{e}"),
    }
}

fn run() -> Result<value::Value, Box<dyn Error>> {
    let test = "true && \"test\"";
    let l = lexer::Lexer::new("test".to_owned(), test.chars());
    let t = parser::parse(l)?;
    let mut intp = interpretter::Interpretter::new();
    Ok(intp.interpret(&t)?)
}
