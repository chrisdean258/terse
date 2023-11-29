use std::error::Error;
mod expression;
mod interpretter;
mod lexer;
mod parser;
mod span;
mod token;
mod value;

fn main() -> Result<(), Box<dyn Error>> {
    let test = "1 + 1, 123";
    let l = lexer::Lexer::new("test".to_owned(), test.chars());
    let t = parser::parse(l)?;
    let mut intp = interpretter::Interpretter::new();
    let val = intp.interpret(&t)?;
    println!("{val}");
    Ok(())
}
