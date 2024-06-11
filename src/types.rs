pub struct Type {}

pub const INTEGER: Type = Type {};

#[derive(Debug, Clone, Copy)]
pub enum DeclarationKind {
    Let,
    Var,
}

pub struct TypeSpec<T> {
    pub type_: Type,
    pub value: Option<T>,
}
