use crate::span::Span;
use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

#[derive(Debug)]
pub struct UntypedExpression {
    pub span: Span,
    pub value: UntypedExpressionKind,
}

#[derive(Debug)]
pub struct UntypedLValue {
    pub span: Span,
    pub value: LValueKind,
}

impl UntypedExpression {
    pub fn to_lval(self) -> Result<UntypedLValue, Self> {
        let UntypedExpressionKind::LValue(value) = self.value else {
            return Err(self);
        };
        Ok(UntypedLValue {
            value,
            span: self.span,
        })
    }
}

// impl UntypedLValue {
// pub fn to_untyped_expression(self) -> UntypedExpression {
// UntypedExpression {
// value: UntypedExpressionKind::LValue(self.value),
// span: self.span,
// }
// }
// }

type SubExpr = Box<UntypedExpression>;

#[derive(Debug)]
pub enum UntypedExpressionKind {
    RValue(RValueKind),
    LValue(LValueKind),
}

#[derive(Debug)]
pub enum LValueKind {
    Variable(String),
    BracketExpr { left: SubExpr, subscript: SubExpr },
}

#[derive(Debug)]
pub enum RValueKind {
    Integer(i64),
    Float(f64),
    Str(String),
    Char(char),
    Bool(bool),
    Assignment {
        left: UntypedLValue,
        op: AssignmentKind,
        right: SubExpr,
    },
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
    For {
        item: UntypedLValue,
        items: SubExpr,
        body: SubExpr,
    },
    If {
        condition: SubExpr,
        body: SubExpr,
    },
    Call {
        callable: SubExpr,
        args: SubExpr,
    },
    Block(Vec<UntypedExpression>),
    ParenExpr(SubExpr),
    BracketExpr(SubExpr),
    LambdaArg(usize),
    Lambda(Rc<UntypedExpression>),
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
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum AssignmentKind {
    Equals,
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
    InvertedCall,
    Pipe,
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
                Self::Pipe => "|>",
                Self::InvertedCall => "->",
            }
        )
    }
}

impl Display for RValueKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Str(s) => write!(f, "{s:?}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::BinOp { left, op, right } => write!(f, "({left} {op} {right})"),
            Self::Assignment {
                left,
                op: _op,
                right,
            } => write!(f, "{left} = {right}"),
            Self::ShortCircuitBinOp { left, op, right } => write!(f, "({left} {op} {right})"),
            Self::ParenExpr(e) => write!(f, "({e})"),
            Self::BracketExpr(e) => write!(f, "[{e}]"),
            Self::FlatBinOp { first, rest } => {
                write!(f, "{first}")?;
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
            Self::For { item, items, body } => {
                writeln!(f, "for {item} in {items}\n{body}")
            }
            Self::If { condition, body } => {
                writeln!(f, "if {condition}\n{body}")
            }
            Self::Block(exprs) => {
                writeln!(f, "{{")?;
                for expr in exprs {
                    writeln!(f, "    {expr}")?;
                }
                writeln!(f, "}}")
            }
            Self::Call { callable, args } => {
                write!(f, "{callable}{args}")
            }
            Self::LambdaArg(i) => {
                write!(f, "\\{i}")
            }
            Self::Lambda(e) => {
                write!(f, "\\({e})")
            }
        }
    }
}

impl Display for UntypedLValue {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Display for LValueKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Variable(i) => write!(f, "{i}"),
            Self::BracketExpr { left, subscript } => write!(f, "{left}{subscript}"),
        }
    }
}

impl Display for UntypedExpressionKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::RValue(r) => write!(f, "{r}"),
            Self::LValue(l) => write!(f, "{l}"),
        }
    }
}

impl Display for UntypedExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
