use std::fmt::{Display, Formatter};
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
    Callable(fn(Vec<Value>) -> Value),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Str(s) => write!(f, "{s:?}"),
            Self::Char(c) => write!(f, "{c:?}"),
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
                    write!(f, "{value}")?
                }
                write!(f, "]")
            }
            Self::Callable(c) => write!(f, "{c:?}"),
        }
    }
}
