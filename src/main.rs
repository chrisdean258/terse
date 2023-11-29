mod expression;
mod lexer;
mod parser;
mod span;
mod token;

fn main() -> Result<(), parser::ParseError> {
    let test = "1 + 1 * 1, 1, 1";
    let l = lexer::Lexer::new("test".to_owned(), test.chars());
    let t = parser::parse(l)?;
    for expression in t.exprs.iter() {
        println!("{expression}");
    }
    Ok(())
}
