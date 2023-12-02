use crate::{
    expression::{
        AssignmentKind, BinOpKind, FlatBinOpKind, LValueKind, RValueKind, ShortCircuitBinOpKind,
        UntypedExpression, UntypedExpressionKind, UntypedLValue,
    },
    nonempty_vec::NonEmptyVec,
    parser::Ast,
    span::Span,
    value::Value,
};
use std::collections::HashMap;
use thiserror::Error;

pub struct Interpretter {
    scopes: ScopeTable,
}

pub struct ScopeTable {
    scopes: NonEmptyVec<HashMap<String, Value>>,
}

impl ScopeTable {
    pub fn new(presets: HashMap<String, Value>) -> Self {
        Self {
            scopes: NonEmptyVec::new(presets),
        }
    }
    pub fn insert(&mut self, key: &str, val: Value) {
        match self.get_mut(key) {
            Some(v) => {
                *v = val;
            }
            None => {
                self.scopes.last_mut().insert(key.into(), val);
            }
        }
    }

    pub fn get(&mut self, key: &str) -> Option<&Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(v) = scope.get_mut(key) {
                if let Value::Lazy(l) = v {
                    *v = l();
                }
                return Some(v);
            }
        }
        None
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(v) = scope.get_mut(key) {
                return Some(v);
            }
        }
        None
    }
}

#[derive(Error, Debug, Clone)]
pub enum InterpretterError {
    #[error("{0}: Cannot evaluate `{1} {2}`")]
    ShortCircuitBinOpErrorOne(Span, Value, ShortCircuitBinOpKind),
    #[error("{0}: Cannot evaluate `{1} {2} {3}`")]
    ShortCircuitBinOpErrorTwo(Span, Value, ShortCircuitBinOpKind, Value),
    #[error("{0}: Cannot evaluate `{1} {2} {3}`")]
    FlatBinOpError(Span, Value, FlatBinOpKind, Value),
    #[error("{0}: Cannot evaluate `{1} {2} {3}`")]
    BinOpError(Span, Value, BinOpKind, Value),
    #[error("{0}: Divide by zero")]
    DivideBy0(Span),
    #[error("{0}: Mod by zero")]
    ModBy0(Span),
    #[error("{0}: Cannot iterate over `{1}`")]
    CannotIterateOver(Span, Value),
    #[error("{0}: No variable with the name `{1}` exists")]
    NoSuchVariable(Span, String),
    #[error("{0}: Non boolean value `{1}` in if condition")]
    NonBoolIfCondition(Span, Value),
    #[error("{0}: `{1}` is not callable")]
    NotCallable(Span, Value),
}

#[derive(Debug, Clone)]
enum FlowControl {
    Error(InterpretterError),
}

type InterpretterResult = Result<Value, FlowControl>;

impl Interpretter {
    // pub fn new() -> Self {
    // Self::with_vars(HashMap::new())
    // }

    pub fn with_vars(presets: HashMap<String, Value>) -> Self {
        Self {
            scopes: ScopeTable::new(presets),
        }
    }

    pub fn interpret(&mut self, ast: &Ast) -> Result<Value, InterpretterError> {
        let mut val = Value::None;
        for expr in ast.exprs.iter() {
            val = self.expr(expr).map_err(|e| {
                let FlowControl::Error(ie) = e;
                ie
            })?;
        }
        Ok(val)
    }

    fn expr(&mut self, expr: &UntypedExpression) -> InterpretterResult {
        match &expr.value {
            UntypedExpressionKind::RValue(r) => match r {
                RValueKind::Integer(i) => Ok(Value::Integer(*i)),
                RValueKind::Float(f) => Ok(Value::Float(*f)),
                RValueKind::Bool(b) => Ok(Value::Bool(*b)),
                RValueKind::Str(s) => Ok(Value::Str(s.clone())),
                RValueKind::Char(c) => Ok(Value::Char(*c)),
                RValueKind::BinOp { left, op, right } => self.binop(&expr.span, left, op, right),
                RValueKind::FlatBinOp { first, rest } => self.flat_binop(&expr.span, first, rest),
                RValueKind::ShortCircuitBinOp { left, op, right } => {
                    self.short_circuit_binop(&expr.span, left, op, right)
                }
                RValueKind::ParenExpr(expr) => self.expr(expr.as_ref()),
                RValueKind::Assignment { left, op, right } => self.assignment(left, op, right),
                RValueKind::For { item, items, body } => self.for_(item, items, body),
                RValueKind::If { condition, body } => self.if_(condition, body),
                RValueKind::Block(exprs) => self.block(exprs),
                RValueKind::Call { callable, args } => self.call(callable, args),
            },
            UntypedExpressionKind::LValue(l) => match l {
                LValueKind::Variable(s) => self.variable(&expr.span, s),
            },
        }
    }

    fn binop(
        &mut self,
        span: &Span,
        left: &UntypedExpression,
        op: &BinOpKind,
        right: &UntypedExpression,
    ) -> InterpretterResult {
        use BinOpKind::*;
        use InterpretterError::*;
        let left_val = self.expr(left)?;
        let right_val = self.expr(right)?;
        let val = match (left_val, op, right_val) {
            (Value::Integer(l), Add, Value::Integer(r)) => Value::Integer(l + r),
            (Value::Integer(l), Subtract, Value::Integer(r)) => Value::Integer(l - r),
            (Value::Integer(l), Multiply, Value::Integer(r)) => Value::Integer(l * r),
            (Value::Integer(_), Mod, Value::Integer(0)) => {
                return Err(FlowControl::Error(ModBy0(span.clone())))
            }
            (Value::Integer(l), Mod, Value::Integer(r)) => Value::Integer(l % r),
            (Value::Integer(_), Divide, Value::Integer(0)) => {
                return Err(FlowControl::Error(DivideBy0(span.clone())))
            }
            (Value::Integer(l), Divide, Value::Integer(r)) => Value::Float(l as f64 / r as f64),
            (Value::Integer(_), IntegerDivide, Value::Integer(0)) => {
                return Err(FlowControl::Error(DivideBy0(span.clone())))
            }
            (Value::Integer(l), IntegerDivide, Value::Integer(r)) => Value::Integer(l / r),

            (Value::Char(l), Subtract, Value::Char(r)) => Value::Integer(l as i64 - r as i64),

            (Value::Float(l), Add, Value::Float(r)) => Value::Float(l + r),
            (Value::Float(l), Multiply, Value::Float(r)) => Value::Float(l * r),
            (Value::Float(l), Subtract, Value::Float(r)) => Value::Float(l - r),

            (Value::Str(l), Add, Value::Str(r)) => Value::Str(l + &r),
            (Value::Tuple(l), Add, Value::Tuple(r)) => {
                let mut t = l.clone();
                t.append(&mut r.clone());
                Value::Tuple(t)
            }
            (l, op, r) => return Err(FlowControl::Error(BinOpError(span.clone(), l, *op, r))),
        };
        Ok(val)
    }

    fn short_circuit_binop(
        &mut self,
        span: &Span,
        left: &UntypedExpression,
        op: &ShortCircuitBinOpKind,
        right: &UntypedExpression,
    ) -> InterpretterResult {
        use ShortCircuitBinOpKind::*;
        let left_val = self.expr(left)?;
        let val = match (&left_val, op) {
            (Value::Bool(false), BoolAnd) => Value::Bool(false),
            (Value::Bool(true), BoolOr) => Value::Bool(true),
            (Value::Bool(true), BoolAnd) | (Value::Bool(false), BoolOr) => {
                match self.expr(right)? {
                    Value::Bool(b) => Value::Bool(b),
                    v => {
                        return Err(FlowControl::Error(
                            InterpretterError::ShortCircuitBinOpErrorTwo(
                                span.clone(),
                                left_val,
                                *op,
                                v,
                            ),
                        ))
                    }
                }
            }
            (_, op) => {
                return Err(FlowControl::Error(
                    InterpretterError::ShortCircuitBinOpErrorOne(span.clone(), left_val, *op),
                ))
            }
        };
        Ok(val)
    }

    fn flat_binop(
        &mut self,
        span: &Span,
        first: &UntypedExpression,
        rest: &[(FlatBinOpKind, Box<UntypedExpression>)],
    ) -> InterpretterResult {
        let mut result = self.expr(first)?;
        for (op, expr) in rest.iter() {
            let right_val = self.expr(expr)?;
            result = match (result, op, right_val) {
                (Value::Char(l), FlatBinOpKind::LessThanOrEqual, Value::Char(r)) => {
                    if rest.len() > 1 {
                        panic!("Chained comparisons not implmented yet")
                    }
                    Value::Bool(l <= r)
                }
                (Value::Char(l), FlatBinOpKind::LessThan, Value::Char(r)) => {
                    if rest.len() > 1 {
                        panic!("Chained comparisons not implmented yet")
                    }
                    Value::Bool(l < r)
                }
                (Value::Integer(l), FlatBinOpKind::CmpEquals, Value::Integer(r)) => {
                    if rest.len() > 1 {
                        panic!("Chained comparisons not implmented yet")
                    }
                    Value::Bool(l == r)
                }
                (Value::Tuple(mut v), FlatBinOpKind::MakeTuple, r) => {
                    v.push(r);
                    Value::Tuple(v)
                }
                (l, FlatBinOpKind::MakeTuple, r) => Value::Tuple(vec![l, r]),
                (l, op, r) => {
                    return Err(FlowControl::Error(InterpretterError::FlatBinOpError(
                        span.clone(),
                        l,
                        *op,
                        r,
                    )))
                }
            }
        }
        Ok(result)
    }

    fn assignment(
        &mut self,
        left: &UntypedLValue,
        _op: &AssignmentKind,
        right: &UntypedExpression,
    ) -> InterpretterResult {
        let right = self.expr(right)?;
        match left.value {
            LValueKind::Variable(ref s) => self.scopes.insert(s, right.clone()),
        };
        Ok(right)
    }

    fn for_(
        &mut self,
        item: &UntypedLValue,
        items: &UntypedExpression,
        body: &UntypedExpression,
    ) -> InterpretterResult {
        let right = self.expr(items)?;
        let items_vec = match right {
            Value::Tuple(v) => v,
            Value::Array(v) => v,
            Value::Str(s) => s.chars().map(Value::Char).collect(),
            _ => {
                return Err(FlowControl::Error(InterpretterError::CannotIterateOver(
                    items.span.clone(),
                    right,
                )))
            }
        };
        for val in items_vec.iter() {
            match item.value {
                LValueKind::Variable(ref s) => self.scopes.insert(s, val.clone()),
            };
            self.expr(body)?;
        }
        Ok(Value::None)
    }

    fn if_(
        &mut self,
        condition: &UntypedExpression,
        body: &UntypedExpression,
    ) -> InterpretterResult {
        let condition_val = self.expr(condition)?;
        match condition_val {
            Value::Bool(true) => {
                self.expr(body)?;
            }
            Value::Bool(false) => {}
            _ => {
                return Err(FlowControl::Error(InterpretterError::NonBoolIfCondition(
                    condition.span.clone(),
                    condition_val,
                )))
            }
        }
        Ok(Value::None)
    }

    fn variable(&mut self, span: &Span, name: &str) -> InterpretterResult {
        self.scopes.get(name).cloned().ok_or_else(|| {
            FlowControl::Error(InterpretterError::NoSuchVariable(
                span.clone(),
                name.to_owned(),
            ))
        })
    }

    fn block(&mut self, exprs: &[UntypedExpression]) -> InterpretterResult {
        let mut val = Value::None;
        for e in exprs {
            val = self.expr(e)?;
        }
        Ok(val)
    }

    fn call(
        &mut self,
        callable: &UntypedExpression,
        args: &UntypedExpression,
    ) -> InterpretterResult {
        let callable_val = self.expr(callable)?;
        let arg = self.expr(args)?;
        let Value::Callable(c) = callable_val else {
            return Err(FlowControl::Error(InterpretterError::NotCallable(
                callable.span.clone(),
                callable_val,
            )));
        };
        let args = match arg {
            Value::Tuple(v) => v,
            _ => vec![arg],
        };
        Ok(c(args))
    }
}
