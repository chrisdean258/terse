use crate::{interpretter::Interpretter, lexer::Lexer, parser, value::Value};
use std::{env, error::Error, fs::read_to_string, path::Path};

pub fn read_and_run(filename: &str) -> Result<Value, Box<dyn Error>> {
    let mut intp = interpretter_with_stdlib()?;
    run_file_in_interpretter(filename.into(), &mut intp, &filename)
}

pub fn interpretter_with_stdlib() -> Result<Interpretter, Box<dyn Error>> {
    let stdlib = env::var("HOME")? + "/git/terse/stdlib/stdlib.trs";
    let mut intp = Interpretter::new();
    run_file_in_interpretter("stdlib".into(), &mut intp, &stdlib)?;
    Ok(intp)
}

fn run_file_in_interpretter<P>(
    name: String,
    intp: &mut Interpretter,
    filename: &P,
) -> Result<Value, Box<dyn Error>>
where
    P: AsRef<Path>,
{
    let chars = read_to_string(filename)?;
    let lexer = Lexer::new(name, chars.chars().collect());
    let tree = parser::parse(lexer)?;
    Ok(intp.interpret(&tree)?)
}

pub fn repl(print_tree: bool) -> Result<(), Box<dyn Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut intp = interpretter_with_stdlib()?;

    loop {
        let readline = rl.readline(">>> ");
        let Ok(mut line) = readline else { break };
        loop {
            let l = Lexer::new("<stdin>".to_owned(), line.chars().collect());
            let t = match parser::parse(l) {
                Ok(t) => t,
                Err(parser::Error::UnexpectedEOF(a)) => {
                    let Ok(readline) = rl.readline("...  ") else {
                        return Err(parser::Error::UnexpectedEOF(a))?;
                    };
                    line += &readline;
                    continue;
                }
                Err(e) => {
                    eprintln!("{e}");
                    break;
                }
            };

            if print_tree {
                println!("{t:#?}");
            }
            rl.add_history_entry(line.as_str())?;
            match intp.interpret(&t) {
                Ok(Value::None) => {}
                Ok(t) => println!("{t}"),
                Err(e) => {
                    eprintln!("{e}");
                }
            };
            break;
        }
    }
    Ok(())
}
