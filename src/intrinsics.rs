use crate::value::{Iterable, Value};
use std::{collections::HashMap, io::stdin};

pub fn intrinsics() -> HashMap<String, Value> {
    let mut vars = HashMap::new();
    vars.insert(
        "stdin".into(),
        Value::Iterable(Iterable::new(
            stdin().lines().map(|s| Value::Str(s.expect("stdin"))),
        )),
    );
    vars.insert("replace".into(), Value::ExternalFunc(str_replace));
    vars.insert("print".into(), Value::ExternalFunc(print_val));
    vars.insert("split".into(), Value::ExternalFunc(split_str));
    vars.insert("join".into(), Value::ExternalFunc(join_str));
    vars.insert("collect".into(), Value::ExternalFunc(collect));
    vars
}

fn str_replace(ipt: &[Value]) -> Value {
    assert_eq!(ipt.len(), 3);
    let Value::Str(haystack) = ipt[0].clone() else {
        panic!("wrong type")
    };
    let Value::Str(needle) = ipt[1].clone() else {
        panic!("wrong type")
    };
    let Value::Str(replacement) = ipt[2].clone() else {
        panic!("wrong type")
    };
    Value::Str(haystack.replace(&needle, &replacement))
}

fn print_val(ipt: &[Value]) -> Value {
    let mut first = true;
    for val in ipt {
        if !first {
            print!(" ");
        }
        first = false;
        print!("{val}");
    }
    println!();
    Value::None
}

fn split_str(ipt: &[Value]) -> Value {
    assert_eq!(ipt.len(), 2);
    let arg = ipt[0].clone();
    let splitter = ipt[1].clone();
    match (arg, splitter) {
        (Value::Str(a), Value::Str(s)) => {
            Value::Array(a.split(&s).map(|s| Value::Str(s.into())).collect())
        }
        (a, b) => todo!("{a} {b}"),
    }
}

fn join_str(ipt: &[Value]) -> Value {
    assert_eq!(ipt.len(), 2);
    let arg = ipt[0].clone();
    let joiner = ipt[1].clone();
    match (arg, joiner) {
        (Value::Array(a), Value::Str(s)) => {
            Value::Str(a.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(&s))
        }
        (a, b) => todo!("{a} {b}"),
    }
}

fn collect(ipt: &[Value]) -> Value {
    assert_eq!(ipt.len(), 1);
    let arg = ipt[0].clone();
    match arg {
        Value::Iterable(a) => Value::Array(a.collect()),
        a => todo!("{a}"),
    }
}
