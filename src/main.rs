use std::{
    collections::HashMap, env, error::Error, fs::read_to_string, io::stdin, process::ExitCode,
};
mod expression;
mod interpretter;
mod lexer;
mod nonempty_vec;
mod parser;
mod span;
mod token;
mod value;

fn str_replace(ipt: Vec<value::Value>) -> value::Value {
    assert_eq!(ipt.len(), 3);
    let value::Value::Str(haystack) = ipt[0].clone() else {
        panic!("wrong type")
    };
    let value::Value::Str(needle) = ipt[1].clone() else {
        panic!("wrong type")
    };
    let value::Value::Str(replacement) = ipt[2].clone() else {
        panic!("wrong type")
    };
    value::Value::Str(haystack.replace(&needle, &replacement))
}

fn print_val(ipt: Vec<value::Value>) -> value::Value {
    for val in ipt {
        print!("{val} ");
    }
    println!();
    value::Value::None
}

fn read_stdin() -> value::Value {
    value::Value::Array(
        stdin()
            .lines()
            .map(|s| value::Value::Str(s.expect("stdin")))
            .collect(),
    )
}

fn usage() -> ExitCode {
    eprintln!("Usage: terse <program_file>");
    ExitCode::FAILURE
}

fn main() -> ExitCode {
    let mut args = env::args();
    let _program = args.next().expect("no program name");
    let program_file = match args.next() {
        Some(a) => a,
        None => return usage(),
    };
    match read_and_run(&program_file) {
        Ok(v) => {
            println!("{v}");
            ExitCode::SUCCESS
        }
        Err(e) => {
            println!("{e}");
            ExitCode::FAILURE
        }
    }
}

fn read_and_run(filename: &str) -> Result<value::Value, Box<dyn Error>> {
    let contents = read_to_string(filename)?;
    run(&contents)
}

fn run(program: &str) -> Result<value::Value, Box<dyn Error>> {
    let l = lexer::Lexer::new("test".to_owned(), program.chars().collect());
    // for t in l {
    // println!("{t:?}");
    // }
    // return Ok(value::Value::None);
    let t = parser::parse(l)?;
    // eprintln!("{t}");
    let mut vars = HashMap::new();
    vars.insert(
        "stdin".into(),
        value::Value::Lazy(value::Lazy::new(Box::new(read_stdin))),
    );
    vars.insert("replace".into(), value::Value::Callable(str_replace));
    vars.insert("print".into(), value::Value::Callable(print_val));
    let mut intp = interpretter::Interpretter::with_vars(vars);
    Ok(intp.interpret(&t)?)
}
