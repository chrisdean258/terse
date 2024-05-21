use crate::{
    interpretter::{Error, FlowControl, Interpretter},
    span::Span,
    value::{Iterable, Value},
};
use std::{cell::RefCell, collections::HashMap, io::stdin, rc::Rc};

type IntrinsicsResult = Result<Value, FlowControl>;

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
    vars.insert("reduce".into(), Value::ExternalFunc(reduce));
    vars.insert("+".into(), Value::ExternalFunc(add));
    vars
}

fn expect_args(ipt: &[Value], span: &Span, num: usize, func: &'static str) -> Result<(), Error> {
    if ipt.len() == num {
        Ok(())
    } else {
        Err(Error::ArgumentCount(span.clone(), func, num, ipt.len()))
    }
}

#[allow(clippy::unnecessary_wraps)]
fn print_val(_intp: &mut Interpretter, ipt: &mut [Value], _span: &Span) -> IntrinsicsResult {
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
    ($args:expr, $span:expr, $func:expr, {$pattern:ident => $type:ty}) => {{
        fn _get(ipt: &mut [Value], span: &Span) -> Result<$type, Error> {
            expect_args(ipt, span, 1, $func, )?;
            Ok(match &ipt[0] {
                Value::$pattern(s) => s.clone(),
                _ => return Err(Error::TypeError(span.clone(), $func, stringify!($pattern), ipt[0].clone_or_take())),
            })
        }
        _get($args, $span)
    }};
    ($args:expr, $span:expr, $func:expr $(,{$pattern:ident => $type:ty})*) => {{
        #[allow(unused_assignments)]
        fn _get(ipt: &mut [Value], span: &Span) -> Result<($($type),*), Error> {
            expect_args(ipt, span, count!($({$pattern => $type}),*), $func)?;
            let mut i = 0;
            Ok(($({let val = match &ipt[i] {
                Value::$pattern(s) => s.clone(),
                _ => return Err(Error::TypeError(span.clone(), $func, stringify!($pattern), ipt[i].clone_or_take())),
            };
            i += 1;
            val
            }),*))
        }
        _get($args, $span)
    }}
}

fn split_str(_intp: &mut Interpretter, ipt: &mut [Value], span: &Span) -> IntrinsicsResult {
    let (arg, splitter) = get_args!(ipt, span, "split", { Str => String }, { Str => String })?;
    Ok(Value::array(
        arg.split(&splitter).map(|s| Value::Str(s.into())).collect(),
    ))
}

fn join_str(_intp: &mut Interpretter, ipt: &mut [Value], span: &Span) -> IntrinsicsResult {
    let (arg, joiner) =
        get_args!(ipt, span, "join", { Array => Rc<RefCell<Vec<Value>>> }, { Str => String })?;
    let vals = arg.take().into_iter().collect::<Vec<_>>();
    let strings = vals.iter().map(ToString::to_string).collect::<Vec<_>>();
    Ok(Value::Str(strings.join(&joiner)))
}

fn collect(_intp: &mut Interpretter, ipt: &mut [Value], span: &Span) -> IntrinsicsResult {
    let arg = get_args!(ipt, span, "join", { Iterable => Iterable})?;
    Ok(Value::array(arg.collect()))
}

fn push(_intp: &mut Interpretter, ipt: &mut [Value], span: &Span) -> IntrinsicsResult {
    expect_args(ipt, span, 2, "push")?;
    let val = ipt[1].clone_or_take();
    match &mut ipt[0] {
        Value::Array(a) => {
            a.borrow_mut().push(val);
            Ok(Value::None)
        }
        a => Err(FlowControl::Error(Error::CannotPush(
            span.clone(),
            "push",
            a.clone_or_take(),
        ))),
    }
}

fn str_replace(_intp: &mut Interpretter, ipt: &mut [Value], span: &Span) -> IntrinsicsResult {
    let (haystack, needle, replacement) =
        get_args!(ipt, span, "replace", { Str => String }, { Str => String }, { Str => String })?;
    Ok(Value::Str(haystack.replace(&needle, &replacement)))
}

#[allow(clippy::cast_possible_wrap)]
fn len(_intp: &mut Interpretter, ipt: &mut [Value], span: &Span) -> IntrinsicsResult {
    expect_args(ipt, span, 1, "len")?;
    match &ipt[0] {
        Value::Array(a) => Ok(Value::Integer(a.borrow().len() as i64)),
        Value::Tuple(a) => Ok(Value::Integer(a.len() as i64)),
        Value::Str(a) => Ok(Value::Integer(a.len() as i64)),
        _ => Err(FlowControl::Error(Error::CannotTakeLength(
            span.clone(),
            "len",
            ipt[0].clone_or_take(),
        ))),
    }
}

fn add(_intp: &mut Interpretter, ipt: &mut [Value], span: &Span) -> IntrinsicsResult {
    expect_args(ipt, span, 2, "+")?;
    match (ipt[0].try_clone(), ipt[1].try_clone()) {
        (Some(v1), Some(v2)) => (v1 + v2).map_err(|_| {
            FlowControl::Error(Error::CannotAdd(
                span.clone(),
                "+",
                ipt[0].clone_or_take(),
                ipt[1].clone_or_take(),
            ))
        }),
        _ => Err(FlowControl::Error(Error::CannotAdd(
            span.clone(),
            "+",
            ipt[0].clone_or_take(),
            ipt[1].clone_or_take(),
        ))),
    }
}

fn reduce(intp: &mut Interpretter, ipt: &mut [Value], span: &Span) -> IntrinsicsResult {
    expect_args(ipt, span, 2, "reduce")?;
    let iterable = ipt[0].clone_or_take();
    let mut function = ipt[1].clone_or_take();
    match iterable {
        Value::Array(a) => {
            let mut iterable = a.borrow_mut();
            let Some(mut base) = iterable.first_mut().map(Value::clone_or_take) else {
                return Err(FlowControl::Error(Error::EmptyIterable(
                    span.clone(),
                    "reduce",
                )));
            };
            for val in &mut iterable[1..] {
                base = intp.evaluate_call(
                    &mut function,
                    vec![base.clone_or_take(), val.clone_or_take()],
                    span,
                )?;
            }
            Ok(base.clone_or_take())
        }
        _ => Err(FlowControl::Error(Error::CannotIterate(
            span.clone(),
            "reduce",
            iterable,
        ))),
    }
}
