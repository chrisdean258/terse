use crate::span::Span;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct UntypedExpression {
    pub span: Span,
    pub value: UntypedExpressionKind,
}

type SubExpr = Box<UntypedExpression>;

#[derive(Clone, Debug)]
pub enum UntypedExpressionKind {
    Integer(i64),
    Float(f64),
    Str(String),
    BinOp {
        left: SubExpr,
        op: BinOpKind,
        right: SubExpr,
    },
}

#[derive(Clone, Debug, Copy)]
pub enum BinOpKind {
    Add,
    Subtract,
    Divide,
    IntegerDivide,
    Mod,
    Multiply,
    BoolOr,
    BoolAnd,
    BoolXor,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    CmpEquals,
    CmpNotEquals,
    BitShiftRight,
    BitShiftLeft,
}

impl Display for BinOpKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "+",
                Self::Subtract => "-",
                Self::Divide => "/",
                Self::IntegerDivide => "//",
                Self::Mod => "%",
                Self::Multiply => "*",
                Self::BoolOr => "||",
                Self::BoolAnd => "&&",
                Self::BoolXor => "^^",
                Self::BitwiseOr => "|",
                Self::BitwiseAnd => "&",
                Self::BitwiseXor => "^",
                Self::GreaterThan => ">",
                Self::GreaterThanOrEqual => ">=",
                Self::LessThan => "<",
                Self::LessThanOrEqual => "<=",
                Self::CmpEquals => "==",
                Self::CmpNotEquals => "!=",
                Self::BitShiftRight => ">>",
                Self::BitShiftLeft => "<<",
            }
        )
    }
}

impl Display for UntypedExpressionKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Str(s) => write!(f, "{s:?}"),
            Self::BinOp { left, op, right } => write!(f, "({left} {op} {right})"),
        }
    }
}

impl Display for UntypedExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}