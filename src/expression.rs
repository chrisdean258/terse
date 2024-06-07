use crate::{span::Span, types::Type, value::Value};
use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

#[derive(Debug)]
pub struct Ast<T> {
    pub exprs: Vec<Expr<T>>,
}

impl<T> std::fmt::Display for Ast<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for e in &self.exprs {
            writeln!(f, "{e}")?;
        }
        Ok(())
    }
}

pub struct TypeSpec {
    type_: Type,
    value: Option<Value>,
}

#[derive(Debug, Clone)]
pub struct Expr<T> {
    pub span: Span,
    pub value: ExprKind<T>,
    pub typespec: T,
}

#[derive(Debug, Clone)]
pub struct LValue<T> {
    pub span: Span,
    pub value: LValueKind<T>,
}

impl<T> Expr<T> {
    pub fn into_lval(self) -> Result<LValue<T>, Self> {
        let ExprKind::LValue(value) = self.value else {
            return Err(self);
        };
        Ok(LValue {
            value,
            span: self.span,
        })
    }
}

type SubExpr<T> = Box<Expr<T>>;
pub type UntypedExprKind = ExprKind<()>;
pub type UntypedExpr = Expr<()>;
pub type UntypedLValue = LValue<()>;
pub type UntypedLValueKind = LValueKind<()>;
pub type UntypedRValueKind = RValueKind<()>;
pub type UntypedAst = Ast<()>;

pub type TypedExprKind = ExprKind<TypeSpec>;
pub type TypedExpr = Expr<TypeSpec>;
pub type TypedLValue = LValue<TypeSpec>;
pub type TypedLValueKind = LValueKind<TypeSpec>;
pub type TypedRValueKind = RValueKind<TypeSpec>;
pub type TypedAst = Ast<TypeSpec>;

#[derive(Debug, Clone)]
pub enum ExprKind<T> {
    RValue(RValueKind<T>),
    LValue(LValueKind<T>),
}

#[derive(Debug, Clone)]
pub enum LValueKind<T> {
    Variable(String),
    BracketExpr {
        left: SubExpr<T>,
        subscript: SubExpr<T>,
    },
    Tuple(Vec<Expr<T>>),
}

#[derive(Debug, Clone, Copy)]
pub enum DeclarationKind {
    Let,
    Var,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    One(String),
    Many(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub enum RValueKind<T> {
    Declaration {
        kind: DeclarationKind,
        names: Pattern,
        value: SubExpr<T>,
    },
    Integer(i64),
    Float(f64),
    Str(String),
    Char(char),
    Bool(bool),
    Assignment {
        left: LValue<T>,
        right: SubExpr<T>,
    },
    BinOp {
        left: SubExpr<T>,
        op: BinOpKind,
        right: SubExpr<T>,
    },
    FlatBinOp {
        first: SubExpr<T>,
        rest: Vec<(FlatBinOpKind, SubExpr<T>)>,
    },
    ShortCircuitBinOp {
        left: SubExpr<T>,
        op: ShortCircuitBinOpKind,
        right: SubExpr<T>,
    },
    For {
        item: LValue<T>,
        items: SubExpr<T>,
        body: SubExpr<T>,
    },
    If {
        condition: SubExpr<T>,
        body: SubExpr<T>,
        else_: Option<SubExpr<T>>,
    },
    While {
        condition: SubExpr<T>,
        body: SubExpr<T>,
    },
    Call {
        callable: SubExpr<T>,
        args: Vec<Expr<T>>,
    },
    PreIncr(LValue<T>),
    PreDecr(LValue<T>),
    PostIncr(LValue<T>),
    PostDecr(LValue<T>),
    Negate(SubExpr<T>),
    Block(Vec<Expr<T>>),
    LambdaArg(usize),
    Lambda(Rc<Expr<T>>),
    Array(Vec<Expr<T>>),
    Break(Option<SubExpr<T>>),
    Continue,
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

impl<T> Display for RValueKind<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Str(s) => write!(f, "{s:?}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::BinOp { left, op, right } => write!(f, "({left} {op} {right})"),
            Self::Assignment { left, right } => write!(f, "{left} = {right}"),
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
            Self::If {
                condition,
                body,
                else_: None,
            } => {
                writeln!(f, "if {condition}\n{body}")
            }
            Self::If {
                condition,
                body,
                else_: Some(e),
            } => {
                writeln!(f, "if {condition}\n{body}{e}")
            }
            Self::While { condition, body } => {
                writeln!(f, "while {condition}\n{body}")
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
            Self::Break(s) => {
                write!(f, "break")?;
                if let Some(subexpr) = s {
                    write!(f, " {subexpr}")?;
                };
                Ok(())
            }
            Self::Continue => {
                write!(f, "continue")
            }
            Self::PreIncr(e) => write!(f, "++{e}"),
            Self::PreDecr(e) => write!(f, "--{e}"),
            Self::PostIncr(e) => write!(f, "{e}++"),
            Self::PostDecr(e) => write!(f, "{e}--"),
            Self::Negate(e) => write!(f, "-{e}"),
        }
    }
}

impl<T> Display for LValue<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<T> Display for LValueKind<T> {
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

impl<T> Display for ExprKind<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::RValue(r) => write!(f, "{r}"),
            Self::LValue(l) => write!(f, "{l}"),
        }
    }
}

impl<T> Display for Expr<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl Display for DeclarationKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Let => write!(f, "let"),
            Self::Var => write!(f, "var"),
        }
    }
}

impl Display for Pattern {
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
