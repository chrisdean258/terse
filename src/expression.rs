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
    Bool(bool),
    BinOp {
        left: SubExpr,
        op: BinOpKind,
        right: SubExpr,
    },
    FlatBinOp {
        first: SubExpr,
        rest: Vec<(FlatBinOpKind, SubExpr)>,
    },
    ShortCircuitBinOp {
        left: SubExpr,
        op: ShortCircuitBinOpKind,
        right: SubExpr,
    },
    ParenExpr(SubExpr),
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Subtract,
    Divide,
    IntegerDivide,
    Mod,
    Multiply,
    BoolXor,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    BitShiftRight,
    BitShiftLeft,
    InvertedCall,
    Pipe,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum FlatBinOpKind {
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    CmpEquals,
    CmpNotEquals,
    MakeTuple,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum ShortCircuitBinOpKind {
    BoolOr,
    BoolAnd,
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
                Self::BoolXor => "^^",
                Self::BitwiseOr => "|",
                Self::BitwiseAnd => "&",
                Self::BitwiseXor => "^",
                Self::BitShiftRight => ">>",
                Self::BitShiftLeft => "<<",
                Self::Pipe => "|>",
                Self::InvertedCall => "->",
            }
        )
    }
}

impl Display for FlatBinOpKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::GreaterThan => ">",
                Self::GreaterThanOrEqual => ">=",
                Self::LessThan => "<",
                Self::LessThanOrEqual => "<=",
                Self::CmpEquals => "==",
                Self::CmpNotEquals => "!=",
                Self::MakeTuple => ",",
            }
        )
    }
}

impl Display for ShortCircuitBinOpKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::BoolOr => "||",
                Self::BoolAnd => "&&",
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
            Self::Bool(b) => write!(f, "{b}"),
            Self::BinOp { left, op, right } => write!(f, "({left} {op} {right})"),
            Self::ShortCircuitBinOp { left, op, right } => write!(f, "({left} {op} {right})"),
            Self::ParenExpr(e) => write!(f, "({e})"),
            Self::FlatBinOp { first, rest } => {
                write!(f, "({first}")?;
                for (op, expr) in rest.iter() {
                    let spc = if *op == FlatBinOpKind::MakeTuple {
                        ""
                    } else {
                        " "
                    };
                    write!(f, "{spc}{op} {expr}")?;
                }
                Ok(())
            }
        }
    }
}

impl Display for UntypedExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
