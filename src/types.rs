#![allow(dead_code)]

#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
pub struct FunctionID {
    id: usize,
}

impl FunctionID {
    pub const fn new() -> Self {
        Self { id: 0 }
    }

    pub const fn next(&self) -> Self {
        Self { id: self.id + 1 }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Type {
    Unreachable,
    Null,
    Integer,
    Float,
    Bool,
    Char,
    Str,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Function(usize),
}

#[derive(Debug, Clone, Copy)]
pub enum DeclarationKind {
    Let,
    Var,
}

#[allow(dead_code)]
#[allow(non_upper_case_globals)]
impl Type {}

impl std::ops::Add for &Type {
    type Output = Option<Type>;
    fn add(self, other: Self) -> Self::Output {
        Some(match (self, other) {
            (Type::Unreachable, _) => Type::Unreachable,
            (_, Type::Unreachable) => Type::Unreachable,
            (Type::Integer, Type::Integer) => Type::Integer,
            (Type::Integer, Type::Float) => Type::Float,
            (Type::Float, Type::Integer) => Type::Float,
            (Type::Float, Type::Float) => Type::Float,

            (Type::Char, Type::Char) => Type::Char,
            (Type::Str, Type::Str) => Type::Str,
            (Type::Array(t), t2) => (t.as_ref() + t2)?,
            (Type::Tuple(t), Type::Tuple(o)) => {
                let mut t = t.clone();
                t.append(&mut o.clone());
                Type::Tuple(t)
            }
            _ => return None,
        })
    }
}

impl std::ops::Sub for &Type {
    type Output = Option<Type>;
    fn sub(self, other: Self) -> Self::Output {
        Some(match (self, other) {
            (Type::Unreachable, _) => Type::Unreachable,
            (_, Type::Unreachable) => Type::Unreachable,
            (Type::Integer, Type::Integer) => Type::Integer,
            (Type::Integer, Type::Float) => Type::Float,
            (Type::Float, Type::Integer) => Type::Float,
            (Type::Float, Type::Float) => Type::Float,
            (Type::Char, Type::Char) => Type::Char,
            (Type::Array(t), t2) => (t.as_ref() + t2)?,
            _ => return None,
        })
    }
}

impl std::ops::Mul for &Type {
    type Output = Option<Type>;
    fn mul(self, other: Self) -> Self::Output {
        Some(match (self, other) {
            (Type::Unreachable, _) => Type::Unreachable,
            (_, Type::Unreachable) => Type::Unreachable,
            (Type::Integer, Type::Integer) => Type::Integer,
            (Type::Integer, Type::Float) => Type::Float,
            (Type::Float, Type::Integer) => Type::Float,
            (Type::Float, Type::Float) => Type::Float,
            _ => return None,
        })
    }
}

impl std::ops::Rem for &Type {
    type Output = Option<Type>;
    fn rem(self, other: Self) -> Self::Output {
        Some(match (self, other) {
            (Type::Unreachable, _) => Type::Unreachable,
            (_, Type::Unreachable) => Type::Unreachable,
            (Type::Integer, Type::Integer) => Type::Integer,
            _ => return None,
        })
    }
}

impl std::ops::Div for &Type {
    type Output = Option<Type>;
    fn div(self, other: Self) -> Self::Output {
        Some(match (self, other) {
            (Type::Unreachable, _) => Type::Unreachable,
            (_, Type::Unreachable) => Type::Unreachable,
            (Type::Integer, Type::Integer) => Type::Float,
            (Type::Integer, Type::Float) => Type::Float,
            (Type::Float, Type::Integer) => Type::Float,
            (Type::Float, Type::Float) => Type::Float,
            _ => return None,
        })
    }
}
