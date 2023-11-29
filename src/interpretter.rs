use crate::{
    expression::{
        BinOpKind, FlatBinOpKind, ShortCircuitBinOpKind, UntypedExpression, UntypedExpressionKind,
    },
    parser::Ast,
    span::Span,
    value::Value,
};
use thiserror::Error;

pub struct Interpretter {}

#[derive(Debug, Clone)]
enum FlowControl {
    Value(Value),
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
}

impl Interpretter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(&mut self, ast: &Ast) -> Result<Value, InterpretterError> {
        let mut val = Value::None;
        for expr in ast.exprs.iter() {
            let FlowControl::Value(v) = self.expr(expr)?;
            val = v;
        }
        Ok(val)
    }

    fn expr(&mut self, expr: &UntypedExpression) -> Result<FlowControl, InterpretterError> {
        match &expr.value {
            UntypedExpressionKind::Integer(i) => Ok(FlowControl::Value(Value::Integer(*i))),
            UntypedExpressionKind::Float(f) => Ok(FlowControl::Value(Value::Float(*f))),
            UntypedExpressionKind::Bool(b) => Ok(FlowControl::Value(Value::Bool(*b))),
            UntypedExpressionKind::Str(s) => Ok(FlowControl::Value(Value::Str(s.clone()))),
            UntypedExpressionKind::BinOp { left, op, right } => {
                self.binop(&expr.span, left, op, right)
            }
            UntypedExpressionKind::FlatBinOp { first, rest } => {
                self.flat_binop(&expr.span, first, rest)
            }
            UntypedExpressionKind::ShortCircuitBinOp { left, op, right } => {
                self.short_circuit_binop(&expr.span, left, op, right)
            }
            _ => todo!(),
        }
    }

    fn binop(
        &mut self,
        span: &Span,
        left: &UntypedExpression,
        op: &BinOpKind,
        right: &UntypedExpression,
    ) -> Result<FlowControl, InterpretterError> {
        use BinOpKind::*;
        let FlowControl::Value(left_val) = self.expr(left)?;
        let FlowControl::Value(right_val) = self.expr(right)?;
        let val = match (left_val, op, right_val) {
            (Value::Integer(l), Add, Value::Integer(r)) => Value::Integer(l + r),
            (Value::Integer(l), Subtract, Value::Integer(r)) => Value::Integer(l - r),
            (Value::Integer(l), Multiply, Value::Integer(r)) => Value::Integer(l * r),
            (Value::Integer(_), Mod, Value::Integer(0)) => {
                return Err(InterpretterError::ModBy0(span.clone()))
            }
            (Value::Integer(l), Mod, Value::Integer(r)) => Value::Integer(l % r),
            (Value::Integer(_), Divide, Value::Integer(0)) => {
                return Err(InterpretterError::DivideBy0(span.clone()))
            }
            (Value::Integer(l), Divide, Value::Integer(r)) => Value::Float(l as f64 / r as f64),
            (Value::Integer(_), IntegerDivide, Value::Integer(0)) => {
                return Err(InterpretterError::DivideBy0(span.clone()))
            }
            (Value::Integer(l), IntegerDivide, Value::Integer(r)) => Value::Integer(l / r),

            (Value::Float(l), Add, Value::Float(r)) => Value::Float(l + r),
            (Value::Float(l), Multiply, Value::Float(r)) => Value::Float(l * r),
            (Value::Float(l), Subtract, Value::Float(r)) => Value::Float(l - r),

            (Value::Str(l), Add, Value::Str(r)) => Value::Str(l + &r),
            (Value::Tuple(l), Add, Value::Tuple(r)) => {
                let mut t = l.clone();
                t.append(&mut r.clone());
                Value::Tuple(t)
            }
            (l, op, r) => return Err(InterpretterError::BinOpError(span.clone(), l, *op, r)),
        };
        Ok(FlowControl::Value(val))
    }

    fn short_circuit_binop(
        &mut self,
        span: &Span,
        left: &UntypedExpression,
        op: &ShortCircuitBinOpKind,
        right: &UntypedExpression,
    ) -> Result<FlowControl, InterpretterError> {
        use ShortCircuitBinOpKind::*;
        let FlowControl::Value(left_val) = self.expr(left)?;
        let val = match (&left_val, op) {
            (Value::Bool(false), BoolAnd) => Value::Bool(false),
            (Value::Bool(true), BoolOr) => Value::Bool(true),
            (Value::Bool(true), BoolAnd) | (Value::Bool(false), BoolOr) => {
                match self.expr(right)? {
                    FlowControl::Value(Value::Bool(b)) => Value::Bool(b),
                    FlowControl::Value(v) => {
                        return Err(InterpretterError::ShortCircuitBinOpErrorTwo(
                            span.clone(),
                            left_val,
                            *op,
                            v,
                        ))
                    }
                }
            }
            (_, op) => {
                return Err(InterpretterError::ShortCircuitBinOpErrorOne(
                    span.clone(),
                    left_val,
                    *op,
                ))
            }
        };
        Ok(FlowControl::Value(val))
    }

    fn flat_binop(
        &mut self,
        span: &Span,
        first: &UntypedExpression,
        rest: &[(FlatBinOpKind, Box<UntypedExpression>)],
    ) -> Result<FlowControl, InterpretterError> {
        let FlowControl::Value(mut result) = self.expr(first)?;
        for (op, expr) in rest.iter() {
            let FlowControl::Value(right_val) = self.expr(expr)?;
            result = match (result, op, right_val) {
                (Value::Tuple(mut v), FlatBinOpKind::MakeTuple, r) => {
                    v.push(r);
                    Value::Tuple(v)
                }
                (l, FlatBinOpKind::MakeTuple, r) => Value::Tuple(vec![l, r]),
                (l, op, r) => {
                    return Err(InterpretterError::FlatBinOpError(span.clone(), l, *op, r))
                }
            }
        }
        Ok(FlowControl::Value(result))
    }
}
