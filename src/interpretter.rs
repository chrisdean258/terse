use crate::{
    expression::{BinOpKind, UntypedExpression, UntypedExpressionKind},
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
    #[error("Error in interpretter")]
    Error(Span),
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
            UntypedExpressionKind::Str(s) => Ok(FlowControl::Value(Value::Str(s.clone()))),
            UntypedExpressionKind::BinOp { left, op, right } => {
                self.binop(&expr.span, left, op, right)
            }
            UntypedExpressionKind::FlatBinOp { first, rest } => {
                self.flat_binop(&expr.span, first, rest)
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
        let FlowControl::Value(left_val) = self.expr(left)?;
        let FlowControl::Value(right_val) = self.expr(right)?;
        match (left_val, op, right_val) {
            (Value::Integer(l), BinOpKind::Add, Value::Integer(r)) => {
                Ok(FlowControl::Value(Value::Integer(l + r)))
            }
            (_, BinOpKind::Add, _) => Err(InterpretterError::Error(span.clone())),
            _ => todo!(),
        }
    }

    fn flat_binop(
        &mut self,
        span: &Span,
        first: &UntypedExpression,
        rest: &Vec<(BinOpKind, Box<UntypedExpression>)>,
    ) -> Result<FlowControl, InterpretterError> {
        let FlowControl::Value(mut result) = self.expr(first)?;
        for (op, expr) in rest.iter() {
            let FlowControl::Value(right_val) = self.expr(expr)?;
            result = match (result, op, right_val) {
                (Value::Integer(l), BinOpKind::Add, Value::Integer(r)) => Value::Integer(l + r),
                (Value::Tuple(mut v), BinOpKind::MakeTuple, r) => {
                    v.push(r);
                    Value::Tuple(v)
                }
                (l, BinOpKind::MakeTuple, r) => Value::Tuple(vec![l, r]),
                (_, BinOpKind::Add, _) => return Err(InterpretterError::Error(span.clone())),
                _ => todo!(),
            }
        }
        return Ok(FlowControl::Value(result));
    }
}
