use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub value: TokenKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Integer(i64),
    Float(f64),
    String(String),
    SingleEquals,
    DoubleEquals,
    FatArrow,
    LessThan,
    LessThanOrEqual,
    BitShiftLeft,
    BitShiftLeftEquals,
}
