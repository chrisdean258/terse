use crate::expression::UntypedExpr;
use std::{
    cell::RefCell,
    fmt::{Debug, Display, Formatter},
    rc::Rc,
};

#[derive(Clone)]
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
    Lambda(Rc<UntypedExpr>),
    Iterable(Iterable),
}

#[derive(Clone)]
pub struct Iterable {
    iterable: Rc<RefCell<dyn Iterator<Item = Value>>>,
}

impl Iterator for Iterable {
    type Item = Value;
    fn next(&mut self) -> Option<Self::Item> {
        self.iterable.borrow_mut().next()
    }
}

impl Iterable {
    pub fn new(iterable: impl Iterator<Item = Value> + 'static) -> Self {
        Self {
            iterable: Rc::new(RefCell::new(iterable)),
        }
    }
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
            Self::Lambda(l) => write!(f, "{l}"),
            Self::Iterable(_) => write!(f, "[...]"),
        }
    }
}

impl Debug for Value {
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
            Self::Lambda(l) => write!(f, "{l}"),
            Self::Iterable(_) => write!(f, "[...]"),
        }
    }
}
