use crate::span::Span;
use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

#[derive(Debug)]
pub struct UntypedExpr {
    pub span: Span,
    pub value: UntypedExprKind,
}

#[derive(Debug)]
pub struct UntypedLValue {
    pub span: Span,
    pub value: LValueKind,
}

impl UntypedExpr {
    pub fn into_lval(self) -> Result<UntypedLValue, Self> {
        let UntypedExprKind::LValue(value) = self.value else {
            return Err(self);
        };
        Ok(UntypedLValue {
            value,
            span: self.span,
        })
    }
}

type SubExpr = Box<UntypedExpr>;

#[derive(Debug)]
pub enum UntypedExprKind {
    RValue(RValueKind),
    LValue(LValueKind),
}

#[derive(Debug)]
pub enum LValueKind {
    Variable(String),
    BracketExpr { left: SubExpr, subscript: SubExpr },
    Tuple(Vec<UntypedExpr>),
}

#[derive(Debug, Clone, Copy)]
pub enum DeclarationKind {
    Let,
    Var,
    Moved,
}

#[derive(Debug)]
pub enum DeclIds {
    One(String),
    Many(Vec<DeclIds>),
}

#[derive(Debug)]
pub enum RValueKind {
    Declaration {
        kind: DeclarationKind,
        names: DeclIds,
        value: SubExpr,
    },
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
        args: Vec<UntypedExpr>,
    },
    Block(Vec<UntypedExpr>),
    LambdaArg(usize),
    Lambda(Rc<UntypedExpr>),
    Array(Vec<UntypedExpr>),
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
    PlusEquals,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum FlatBinOpKind {
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    CmpEquals,
    CmpNotEquals,
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
            Self::FlatBinOp { first, rest } => {
                write!(f, "{first}")?;
                for (op, expr) in rest {
                    write!(f, " {op} {expr}")?;
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
                write!(f, "{callable}(")?;
                let mut first = true;
                for arg in args {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            Self::LambdaArg(i) => {
                write!(f, "\\{i}")
            }
            Self::Lambda(e) => {
                write!(f, "\\({e})")
            }
            Self::Array(vals) => {
                let mut first = true;
                for val in vals {
                    write!(f, "{}{val}", if first { "[" } else { ", " })?;
                    first = false;
                }
                write!(f, "]")
            }
            Self::Declaration { kind, names, value } => write!(f, "{kind} {names} = {value}"),
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
            Self::Tuple(vals) => {
                let mut first = true;
                for val in vals {
                    write!(f, "{}{val}", if first { "(" } else { ", " })?;
                    first = false;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for UntypedExprKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::RValue(r) => write!(f, "{r}"),
            Self::LValue(l) => write!(f, "{l}"),
        }
    }
}

impl Display for UntypedExpr {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl Display for DeclarationKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Let => write!(f, "let"),
            Self::Var => write!(f, "var"),
            Self::Moved => write!(f, "moved!"),
        }
    }
}

impl Display for DeclIds {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::One(s) => write!(f, "{s}"),
            Self::Many(ids) => {
                let mut first = true;
                for id in ids {
                    write!(f, "{}{id}", if first { "(" } else { ", " })?;
                    first = false;
                }
                write!(f, ")")
            }
        }
    }
}
