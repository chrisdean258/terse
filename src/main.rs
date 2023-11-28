mod expression;
mod lexer;
mod parser;
mod span;
mod token;

fn main() -> Result<(), parser::ParseError> {
    let test = "\"11\" + 12 // 21 13 14 / 15";
    let l = lexer::Lexer::new("test".to_owned(), test.chars());
    let t = parser::parse(l)?;
    for expression in t.exprs.iter() {
        println!("{expression}");
    }
    Ok(())
}
