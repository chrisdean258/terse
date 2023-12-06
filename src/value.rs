use crate::expression::UntypedExpression;
use std::{
    fmt::{Debug, Display, Formatter},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub enum Value {
    None,
    Integer(i64),
    Float(f64),
    Str(String),
    Tuple(Vec<Value>),
    Bool(bool),
    Array(Vec<Value>),
    Char(char),
    ExternalFunc(fn(&[Value]) -> Value),
    Lazy(fn() -> Value),
    Lambda(Rc<UntypedExpression>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Str(s) => write!(f, "{s}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::Tuple(tup) => {
                let mut first = true;
                for value in tup.iter() {
                    if first {
                        write!(f, "(")?;
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?
                }
                write!(f, ")")
            }
            Self::Array(arr) => {
                let mut first = true;
                for value in arr.iter() {
                    if first {
                        write!(f, "[")?;
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value:?}")?
                }
                write!(f, "]")
            }
            Self::ExternalFunc(c) => write!(f, "{c:?}"),
            Self::Lazy(v) => write!(f, "{v:?}"),
            Self::Lambda(l) => write!(f, "{l}"),
        }
    }
}
