use crate::span::Span;

#[derive(Clone, Debug)]
pub struct UntypedExpression {
    pub span: Span,
    pub value: UntypedExpressionKind,
}

#[derive(Clone, Debug)]
pub enum UntypedExpressionKind {
    Integer(i64),
    Float(f64),
    String(String),
}
