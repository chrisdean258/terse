use std::{collections::HashMap, env, error::Error, io::stdin};
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

fn main() {
    let mut args = env::args();
    let _program = args.next().expect("no program name");
    let test = args.next().unwrap_or("true && \"test\"".into());
    match run(&test) {
        Ok(v) => println!("{v}"),
        Err(e) => println!("{e}"),
    }
}

fn run(test: &str) -> Result<value::Value, Box<dyn Error>> {
    let l = lexer::Lexer::new("test".to_owned(), test.chars().collect());
    // for t in l {
    // println!("{t:?}");
    // }
    // return Ok(value::Value::None);
    let t = parser::parse(l)?;
    // eprintln!("{t}");
    let stdinlines = stdin()
        .lines()
        .map(|s| value::Value::Str(s.expect("stdin")))
        .collect();
    let mut vars = HashMap::new();
    vars.insert("stdin".into(), value::Value::Array(stdinlines));
    vars.insert("replace".into(), value::Value::Callable(str_replace));
    vars.insert("print".into(), value::Value::Callable(print_val));
    let mut intp = interpretter::Interpretter::with_vars(vars);
    Ok(intp.interpret(&t)?)
}
