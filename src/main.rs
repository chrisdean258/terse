mod expression;
mod lexer;
mod parser;
mod span;
mod token;

fn main() {
    let test = "11";
    let l = lexer::Lexer::new("test".to_owned(), test.chars());
    let t = parser::parse(l);
    println!("{t:?}");
}
