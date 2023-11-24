mod lexer;
mod span;
mod token;

fn main() {
    let test = "11";
    let l = lexer::Lexer::new("test".to_owned(), test.chars());
    for token in l {
        println!("{token:?}");
    }
}
