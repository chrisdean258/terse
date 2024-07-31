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

#[derive(Debug, Clone)]
pub struct Range<T> {
    minimum: Option<T>,
    maximum: Option<T>,
}

impl<T: Copy> Range<T> {
    pub const fn from_value(value: T) -> Self {
        Self {
            minimum: Some(value),
            maximum: Some(value),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Type {
    Unreachable,
    Null,
    Integer(Range<i64>),
    Float(Range<f64>),
    Bool(Range<bool>),
    Char(Range<char>),
    Str(Option<String>),
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
