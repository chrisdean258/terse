#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub struct Type {
    idx: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum DeclarationKind {
    Let,
    Var,
}

#[allow(non_upper_case_globals)]
impl Type {
    pub const Null: Self = Self { idx: 0 };
    pub const Integer: Self = Self { idx: 1 };
    pub const Float: Self = Self { idx: 2 };
    pub const Bool: Self = Self { idx: 3 };
    pub const Char: Self = Self { idx: 4 };
    pub const Str: Self = Self { idx: 5 };
}
