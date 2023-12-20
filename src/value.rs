use crate::{expression::UntypedExpr, interpretter::Interpretter, intrinsics::Error};
use std::{
    cell::RefCell,
    fmt::{Debug, Display, Formatter},
    rc::Rc,
};

pub enum Value {
    None,
    Moved,
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
            Self::None => Some(Self::None),
            Self::Integer(i) => Some(Self::Integer(*i)),
            Self::Float(f) => Some(Self::Float(*f)),
            Self::Bool(b) => Some(Self::Bool(*b)),
            Self::Char(c) => Some(Self::Char(*c)),
            Self::Lambda(l) => Some(Self::Lambda(l.clone())),
            Self::ExternalFunc(e) => Some(Self::ExternalFunc(*e)),
            Self::Tuple(t) => {
                let mut v = Vec::new();
                for item in t {
                    v.push(item.try_clone()?);
                }
                Some(Self::Tuple(v))
            }
            _ => None,
        }
    }

    pub fn clone_or_take(&mut self) -> Self {
        let mut cln = self.try_clone().unwrap_or(Self::Moved);
        std::mem::swap(&mut cln, self);
        cln
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Moved => write!(f, "moved!"),
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
            Self::Moved => write!(f, "moved!"),
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

impl PartialEq for Value {
    #[allow(clippy::cognitive_complexity)]
    #[allow(clippy::cast_precision_loss)]
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::None => matches!(other, Self::None),
            Self::Integer(a) => {
                matches!(other, Self::Integer(b) if a == b)
                    || matches!(other, Self::Float(b) if *a as f64 == *b)
                    || matches!(other, Self::Char(b) if *a == *b as i64)
            }
            Self::Float(a) => {
                matches!(other, Self::Integer(b) if *a == *b as f64)
                    || matches!(other, Self::Float(b) if a == b)
            }
            Self::Str(a) => matches!(other, Self::Str(b) if a == b),
            Self::Tuple(a) => matches!(other, Self::Tuple(b) if a == b),
            Self::Bool(a) => matches!(other, Self::Bool(b) if a == b),
            Self::Char(a) => {
                matches!(other, Self::Integer(b) if *a as i64 == *b)
                    || matches!(other, Self::Char(b) if a == b)
            }
            Self::ExternalFunc(a) => matches!(other, Self::ExternalFunc(b) if a == b),
            Self::Lambda(a) => matches!(other, Self::Lambda(b) if Rc::ptr_eq(a,b)),
            Self::Iterable(_) | Self::Array(_) | Self::Moved => false,
        }
    }
}

macro_rules! cmp {

    ($self:ident, $other:ident => { $($first:ident$(($t1:ty))? <=> $second:ident$(($t2:ty))?),* $(,)?  }) => {
        match ($self, $other) {
            $((Self::$first(a), Self::$second(b)) if *a $(as $t1)? < *b $(as $t2)? => Some(std::cmp::Ordering::Less),)*
            $((Self::$first(a), Self::$second(b)) if *a $(as $t1)? > *b $(as $t2)? => Some(std::cmp::Ordering::Greater),)*
            $((Self::$second(a), Self::$first(b)) if *a $(as $t2)? < *b $(as $t1)? => Some(std::cmp::Ordering::Less),)*
            $((Self::$second(a), Self::$first(b)) if *a $(as $t2)? > *b $(as $t1)? => Some(std::cmp::Ordering::Greater),)*
            _ => None
        }
    };
}

impl PartialOrd for Value {
    #[allow(clippy::cast_precision_loss)]
    #[allow(clippy::cognitive_complexity)]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.eq(other) {
            return Some(std::cmp::Ordering::Equal);
        }
        cmp!(self, other => {
            Integer <=> Integer,
            Float <=> Float ,
            Float <=> Integer(f64),
            Integer <=> Char(i64),
            Str <=> Str,
            Tuple <=> Tuple,
        })
    }
}

macro_rules! binop {
    ($trait_name:ident, $funcname:ident => { $($first:ident($($t1:ty)?) $binop:tt  $(($r:tt))?$second:ident($($t2:ty)?) => $out:ident$(($t3:ty))?),* $(,)?  }, $($rest:tt)*) => {
        impl std::ops::$trait_name for Value {
            type Output = Result<Self, (Self, Self)>;
            fn $funcname(self, other: Self) -> Self::Output {
                match (self, other) {
                    $((Self::$first(a), Self::$second(b)) => Ok(Value::$out((a $(as $t1)? $binop $($r)?b $(as $t2)?) $(as $t3)?)),)*
                    $($rest)*
                    // $((Self::$second(a), Self::$second(b)) => Some(Value::$out(a $(as $t2)? $binop b $(as $t1)?)),)*
                    (a,b) => Err((a, b))
                }
            }
        }
    };
}

macro_rules! unop {
    ($trait_name:ident, $funcname:ident => { $($unop:tt $first:ident),* $(,)?  }) => {
        impl std::ops::$trait_name for Value {
            type Output = Result<Self, Self>;
            fn $funcname(self) -> Self::Output {
                match (self) {
                    $(Self::$first(a) => Ok(Value::$first($unop a)),)*
                    a => Err(a)
                }
            }
        }
    };
}

binop!(Add, add => {
    Integer() + Integer() => Integer,
    Integer(f64) + Float() => Float,
    Float() + Integer(f64) => Float,
    Char(u8) + Integer(u8) => Char(char),
    Integer(u8) + Char(u8) => Char(char),
    Bool(i64) + Bool(i64) => Integer,
    Bool(i64) + Integer() => Integer,
    Integer() + Bool(i64) => Integer,
    Str() + (&)Str() => Str,
},
(Value::Array(a), Value::Array(b)) => {
    a.borrow_mut().append(&mut b.borrow_mut());
    Ok(Value::Array(a))
}
);

binop!(Sub, sub => {
    Integer() - Integer() => Integer,
    Integer(f64) - Float() => Float,
    Float() - Integer(f64) => Float,
    Float() - Float() => Float,
    Char(u8) - Integer(u8) => Char(char),
    Char(u8) - Char(u8) => Integer(i64),
    Integer(u8) - Char(u8) => Char(char),
    Bool(i64) - Bool(i64) => Integer,
    Bool(i64) - Integer() => Integer,
    Integer() - Bool(i64) => Integer,
},);

binop!(BitAnd, bitand => {
    Integer() & Integer() => Integer,
    Bool(i64) & Bool(i64) => Integer,
},);

binop!(BitOr, bitor => {
    Integer() | Integer() => Integer,
    Bool(i64) | Bool(i64) => Integer,
},);

binop!(BitXor, bitxor => {
    Integer() ^ Integer() => Integer,
    Bool(i64) ^ Bool(i64) => Integer,
},);

binop!(Shl, shl => {
    Integer() << Integer() => Integer,
},);

binop!(Shr, shr => {
    Integer() >> Integer() => Integer,
},);

binop!(Div, div => {
    Integer(f64) / Integer(f64) => Float,
    Integer(f64) / Float() => Float,
    Float() / Integer(f64) => Float,
},);

binop!(Mul, mul => {
    Integer() * Integer() => Integer,
    Bool(i64) * Integer() => Integer,
    Integer() * Bool(i64) => Integer,
    Integer(f64) * Float() => Float,
    Float() * Integer(f64) => Float,
},);

binop!(Rem, rem => {
    Integer() % Integer() => Integer,
},);

unop!(Not, not => {
    ! Integer,
    ! Bool
});

unop!(Neg, neg => {
    - Integer,
    - Float
});

// Index
// IndexMut
