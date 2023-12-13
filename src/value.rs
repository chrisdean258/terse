use crate::{expression::UntypedExpr, interpretter::Interpretter, intrinsics::Error};
use std::{
    cell::RefCell,
    fmt::{Debug, Display, Formatter},
    rc::Rc,
};

pub enum Value {
    None,
    Integer(i64),
    Float(f64),
    Str(String),
    Tuple(Vec<Value>),
    Bool(bool),
    Array(Rc<RefCell<Vec<Value>>>),
    Char(char),
    ExternalFunc(fn(&mut Interpretter, &mut [Value]) -> Result<Value, Error>),
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

impl Value {
    pub fn array(vals: Vec<Self>) -> Self {
        Self::Array(Rc::new(RefCell::new(vals)))
    }

    pub fn try_clone(&self) -> Option<Self> {
        match self {
            Self::Integer(i) => Some(Self::Integer(*i)),
            Self::Float(f) => Some(Self::Float(*f)),
            Self::Bool(b) => Some(Self::Bool(*b)),
            Self::Char(c) => Some(Self::Char(*c)),
            Self::None => Some(Self::None),
            Self::ExternalFunc(e) => Some(Self::ExternalFunc(*e)),
            _ => None,
        }
    }

    pub fn clone_or_take(&mut self) -> Self {
        let mut cln = self.try_clone().unwrap_or(Self::None);
        std::mem::swap(&mut cln, self);
        cln
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
                write!(f, "(")?;
                for value in tup {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{value:?}")?;
                }
                write!(f, ")")
            }
            Self::Array(arr) => {
                let mut first = true;
                write!(f, "[")?;
                for value in arr.borrow().iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{value:?}")?;
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
                write!(f, "(")?;
                for value in tup {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{value:?}")?;
                }
                write!(f, ")")
            }
            Self::Array(arr) => {
                let mut first = true;
                write!(f, "[")?;
                for value in arr.borrow().iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{value:?}")?;
                }
                write!(f, "]")
            }
            Self::ExternalFunc(c) => write!(f, "{c:?}"),
            Self::Lambda(l) => write!(f, "{l}"),
            Self::Iterable(_) => write!(f, "[...]"),
        }
    }
}
