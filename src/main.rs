mod lexer;
mod textlocator;
mod token;

fn main() {
    let test = "\"this is as\" 1 \"ssfsd\"\"lsdkfjsd";
    let l = lexer::Lexer::new(test.chars());
    for token in l {
        println!("{token:?}");
    }
}
