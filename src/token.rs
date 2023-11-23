use crate::textlocator::Span;

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub value: TokenKind,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Number(i64),
    String(String),
}
