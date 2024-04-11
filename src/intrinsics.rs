use crate::{
    interpretter::Interpretter,
    value::{Iterable, Value},
};
use std::{cell::RefCell, collections::HashMap, io::stdin, rc::Rc};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[allow(clippy::enum_variant_names)]
    #[error("{0}: Type error: expected `{1}` found `{2}`")]
    TypeError(&'static str, &'static str, Value),
    #[error("{0}: expected {1} argument(s) given {2}")]
    ArgumentCount(&'static str, usize, usize),
    #[error("{0}: cannot take the length of `{1}`")]
    CannotTakeLength(&'static str, Value),
    #[error("{0}: cannot add `{1} + {2}`")]
    CannotAdd(&'static str, Value, Value),
}

type IntrinsicsResult = Result<Value, Error>;

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
    vars.insert("push".into(), Value::ExternalFunc(push));
    vars.insert("len".into(), Value::ExternalFunc(len));
    vars.insert("+".into(), Value::ExternalFunc(add));
    vars
}

const fn expect_args(ipt: &[Value], num: usize, func: &'static str) -> Result<(), Error> {
    if ipt.len() == num {
        Ok(())
    } else {
        Err(Error::ArgumentCount(func, num, ipt.len()))
    }
}

#[allow(clippy::unnecessary_wraps)]
fn print_val(_intp: &mut Interpretter, ipt: &mut [Value]) -> IntrinsicsResult {
    let mut first = true;
    for val in ipt {
        if !first {
            print!(" ");
        }
        first = false;
        print!("{val}");
    }
    println!();
    Ok(Value::None)
}

macro_rules! count {
    () => {
        0
    };
    ({$p:ident => $t:ty}, $({$pattern:ident => $type:ty}),* $(,)? ) => {
        1 + count!( $( {$pattern => $type},)*)
    };
}
macro_rules! get_args {
    ($args:expr, $func:expr, {$pattern:ident => $type:ty}) => {{
        fn _get(ipt: &mut [Value]) -> Result<$type, Error> {
            expect_args(ipt, 1, $func)?;
            Ok(match &ipt[0] {
                Value::$pattern(s) => s.clone(),
                _ => return Err(Error::TypeError($func, stringify!($pattern), ipt[0].clone_or_take())),
            })
        }
        _get($args)
    }};
    ($args:expr, $func:expr $(,{$pattern:ident => $type:ty})*) => {{
        #[allow(unused_assignments)]
        fn _get(ipt: &mut [Value]) -> Result<($($type),*), Error> {
            expect_args(ipt, count!($({$pattern => $type}),*), $func)?;
            let mut i = 0;
            Ok(($({let val = match &ipt[i] {
                Value::$pattern(s) => s.clone(),
                _ => return Err(Error::TypeError($func, stringify!($pattern), ipt[i].clone_or_take())),
            };
            i += 1;
            val
            }),*))
        }
        _get($args)
    }}
}

fn split_str(_intp: &mut Interpretter, ipt: &mut [Value]) -> IntrinsicsResult {
    let (arg, splitter) = get_args!(ipt, "split", { Str => String }, { Str => String })?;
    Ok(Value::array(
        arg.split(&splitter).map(|s| Value::Str(s.into())).collect(),
    ))
}

fn join_str(_intp: &mut Interpretter, ipt: &mut [Value]) -> IntrinsicsResult {
    let (arg, joiner) =
        get_args!(ipt, "join", { Array => Rc<RefCell<Vec<Value>>> }, { Str => String })?;
    let vals = arg.take().into_iter().collect::<Vec<_>>();
    let strings = vals.iter().map(ToString::to_string).collect::<Vec<_>>();
    Ok(Value::Str(strings.join(&joiner)))
}

fn collect(_intp: &mut Interpretter, ipt: &mut [Value]) -> IntrinsicsResult {
    let arg = get_args!(ipt, "join", { Iterable => Iterable})?;
    Ok(Value::array(arg.collect()))
}

fn push(_intp: &mut Interpretter, ipt: &mut [Value]) -> IntrinsicsResult {
    expect_args(ipt, 2, "push")?;
    let val = ipt[1].clone_or_take();
    match &mut ipt[0] {
        Value::Array(a) => a.borrow_mut().push(val),
        a => todo!("{a}"),
    }
    Ok(Value::None)
}

fn str_replace(_intp: &mut Interpretter, ipt: &mut [Value]) -> IntrinsicsResult {
    let (haystack, needle, replacement) =
        get_args!(ipt, "replace", { Str => String }, { Str => String }, { Str => String })?;
    Ok(Value::Str(haystack.replace(&needle, &replacement)))
}

#[allow(clippy::cast_possible_wrap)]
fn len(_intp: &mut Interpretter, ipt: &mut [Value]) -> IntrinsicsResult {
    expect_args(ipt, 1, "len")?;
    match &ipt[0] {
        Value::Array(a) => Ok(Value::Integer(a.borrow().len() as i64)),
        Value::Tuple(a) => Ok(Value::Integer(a.len() as i64)),
        Value::Str(a) => Ok(Value::Integer(a.len() as i64)),
        _ => Err(Error::CannotTakeLength("len", ipt[0].clone_or_take())),
    }
}

fn add(_intp: &mut Interpretter, ipt: &mut [Value]) -> IntrinsicsResult {
    expect_args(ipt, 2, "+")?;
    match (ipt[0].try_clone(), ipt[1].try_clone()) {
        (Some(v1), Some(v2)) => (v1 + v2)
            .map_err(|_| Error::CannotAdd("+", ipt[0].clone_or_take(), ipt[1].clone_or_take())),
        _ => Err(Error::CannotAdd(
            "+",
            ipt[0].clone_or_take(),
            ipt[1].clone_or_take(),
        )),
    }
}
