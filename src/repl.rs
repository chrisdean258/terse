use crate::{interpretter::Interpretter, lexer::Lexer, parser::parse, value::Value};
use std::{env, error::Error, fs::read_to_string};

pub fn repl() -> Result<(), Box<dyn Error>> {
    let stdlib = read_to_string(env::var("HOME")? + "/git/terse/stdlib/stdlib.trs")?;
    let l = Lexer::new("stdlib".to_owned(), stdlib.chars().collect());
    let t = parse(l)?;
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut intp = Interpretter::new();
    intp.interpret(&t)?;

    loop {
        let readline = rl.readline(">> ");
        let Ok(line) = readline else { break };
        let l = Lexer::new("<stdin>".to_owned(), line.chars().collect());
        let t = match parse(l) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        };
        match intp.interpret(&t) {
            Ok(Value::None) => {}
            Ok(t) => println!("{t}"),
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        };
    }
    Ok(())
}
