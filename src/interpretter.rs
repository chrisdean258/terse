use crate::{
    expression::{
        BinOpKind, DeclIds, DeclarationKind, FlatBinOpKind, LValueKind, RValueKind,
        ShortCircuitBinOpKind, UntypedExpr, UntypedExprKind, UntypedLValue,
    },
    intrinsics::{intrinsics, Error as IntrinsicsError},
    parser::Ast,
    span::Span,
    value::Value,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use thiserror::Error;

pub struct Interpretter {
    scopes: ScopeTable,
    lambda_args: Vec<Vec<Value>>,
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("{0}: Cannot evaluate `{1} {2}`")]
    ShortCircuitBinOpErrorOne(Span, Value, ShortCircuitBinOpKind),
    #[error("{0}: Cannot evaluate `{1} {2} {3}`")]
    ShortCircuitBinOpErrorTwo(Span, Value, ShortCircuitBinOpKind, Value),
    // #[error("{0}: Cannot evaluate `{1} {2} {3}`")]
    // FlatBinOp(Span, Value, FlatBinOpKind, Value),
    #[error("{0}: Cannot evaluate `{1} {2} {3}`")]
    BinOp(Span, Value, BinOpKind, Value),
    #[error("{0}: Divide by zero")]
    DivideBy0(Span),
    #[error("{0}: Mod by zero")]
    ModBy0(Span),
    #[error("{0}: Cannot iterate over `{1}`")]
    CannotIterateOver(Span, Value),
    #[error("{0}: No variable with the name `{1}` exists")]
    NoSuchVariable(Span, String),
    #[error("{0}: Non boolean value `{1}` in flow control condition")]
    NonBoolCondition(Span, Value),
    #[error("{0}: `{1}` is not callable")]
    NotCallable(Span, Value),
    #[error("{0}: Reference to `\\{1}` in lambda only supplied with {2} arguments")]
    NotEnoughArguments(Span, usize, usize),
    #[error("{0}: Tried to index `{1}` with `{2}` but `{1}` is only of length {3}")]
    IndexOutOfBound(Span, Value, Value, usize),
    #[error("{0}: Cannot index {1}")]
    CannotIndex(Span, Value),
    #[error("{0}: Cannot assign `{2}` into {1} values")]
    AssignmentLengthMismatch(Span, usize, Value),
    //TODO: make this error message better
    #[error("{0}: Cannot assign into expression")]
    CannotAssign(Span),
    #[error("{0}")]
    BadConversion(#[from] std::num::TryFromIntError),
    #[error("{0}: `{1}` was declared as a constant with the `let` keyword")]
    VariableIsConstant(Span, String),
    #[error("{0}")]
    TypeErrorInIntrinsic(#[from] IntrinsicsError),
    #[error("{0}: `{1}` was moved")]
    VariableWasMoved(Span, String),
    #[error("Illegal flow control. Break or continue not in a loop")]
    IllegalFlowControl,
}

pub struct ScopeTable {
    scopes: Vec<HashMap<String, (Value, DeclarationKind)>>,
}

#[derive(Debug)]
enum FlowControl {
    Error(Error),
    Break(Value),
    Continue,
}

impl From<Error> for FlowControl {
    fn from(err: Error) -> Self {
        Self::Error(err)
    }
}

impl From<IntrinsicsError> for FlowControl {
    fn from(err: IntrinsicsError) -> Self {
        Self::Error(Error::TypeErrorInIntrinsic(err))
    }
}

type InterpretterResult = Result<Value, FlowControl>;

enum GetMutError {
    NoSuchKey,
    IsLetVar,
}
impl ScopeTable {
    fn new(presets: HashMap<String, (Value, DeclarationKind)>) -> Self {
        Self {
            scopes: vec![presets],
        }
    }
    fn insert(&mut self, key: String, val: Value, kind: DeclarationKind) {
        self.scopes
            .last_mut()
            .expect("scopes is empty")
            .insert(key, (val, kind));
    }

    fn get(&mut self, key: &str) -> Option<&mut Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((v, _)) = scope.get_mut(key) {
                return Some(v);
            }
        }
        None
    }

    fn get_mut(&mut self, key: &str) -> Result<&mut Value, GetMutError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((v, k)) = scope.get_mut(key) {
                return match k {
                    DeclarationKind::Var => Ok(v),
                    DeclarationKind::Let => Err(GetMutError::IsLetVar),
                };
            }
        }
        Err(GetMutError::NoSuchKey)
    }

    fn clone_or_take(&mut self, key: &str) -> Option<Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((v, _)) = scope.get_mut(key) {
                // we can take out of `let` or `var` decls
                return Some(v.clone_or_take());
            }
        }
        None
    }

    pub fn open(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn close(&mut self) {
        self.scopes.pop();
        assert!(!self.scopes.is_empty(), "popped last scope");
    }
}

impl Interpretter {
    pub fn new() -> Self {
        Self::with_vars(intrinsics())
    }

    pub fn with_vars(presets: HashMap<String, Value>) -> Self {
        let presets = presets
            .into_iter()
            .map(|(k, v)| (k, (v, DeclarationKind::Let)))
            .collect::<HashMap<_, _>>();
        Self {
            scopes: ScopeTable::new(presets),
            lambda_args: Vec::new(),
        }
    }

    pub fn interpret(&mut self, ast: &Ast) -> Result<Value, Error> {
        let mut val = Value::None;
        for expr in &ast.exprs {
            val = match self.expr(expr) {
                Err(FlowControl::Error(ie)) => Err(ie)?,
                Err(FlowControl::Break(_e)) => Err(Error::IllegalFlowControl)?,
                Err(FlowControl::Continue) => Err(Error::IllegalFlowControl)?,
                Ok(v) => v,
            }
        }
        Ok(val)
    }

    fn exprs(&mut self, exprs: &[UntypedExpr]) -> Result<Vec<Value>, FlowControl> {
        exprs.iter().map(|e| self.expr(e)).collect()
    }

    fn expr(&mut self, expr: &UntypedExpr) -> InterpretterResult {
        match &expr.value {
            UntypedExprKind::RValue(r) => match r {
                RValueKind::Integer(i) => Ok(Value::Integer(*i)),
                RValueKind::Float(f) => Ok(Value::Float(*f)),
                RValueKind::Bool(b) => Ok(Value::Bool(*b)),
                RValueKind::Str(s) => Ok(Value::Str(s.clone())),
                RValueKind::Char(c) => Ok(Value::Char(*c)),
                RValueKind::BinOp { left, op, right } => self.binop(&expr.span, left, *op, right),
                RValueKind::FlatBinOp { first, rest } => self.flat_binop(&expr.span, first, rest),
                RValueKind::ShortCircuitBinOp { left, op, right } => {
                    self.short_circuit_binop(&expr.span, left, *op, right)
                }
                RValueKind::Assignment { left, right } => self.assignment(left, right, &expr.span),
                RValueKind::For { item, items, body } => self.for_(item, items, body),
                RValueKind::If {
                    condition,
                    body,
                    else_,
                } => self.if_(condition, body, else_.as_ref().map(AsRef::as_ref)),
                RValueKind::While { condition, body } => self.while_(condition, body),
                RValueKind::Block(exprs) => self.block(exprs),
                RValueKind::Call { callable, args } => self.call(callable, args, &expr.span),
                RValueKind::Lambda(subexpr) => Ok(Value::Lambda(subexpr.clone())),
                RValueKind::LambdaArg(i) => self.lambda_arg(*i, &expr.span),
                RValueKind::Array(a) => self.array(a),
                RValueKind::Declaration { kind, names, value } => {
                    let v = self.expr(value)?;
                    self.declaration(*kind, names, v, &expr.span)
                }
                RValueKind::Break(expr) => self.break_(expr.as_ref().map(AsRef::as_ref)),
                RValueKind::Continue => Err(FlowControl::Continue),
            },
            UntypedExprKind::LValue(l) => self.lval(l, &expr.span),
        }
    }

    fn lval(&mut self, expr: &LValueKind, span: &Span) -> InterpretterResult {
        match expr {
            LValueKind::Variable(s) => self.variable(span, s),
            LValueKind::BracketExpr { left, subscript } => self.bracket(span, left, subscript),
            LValueKind::Tuple(t) => self.tuple(t),
        }
    }

    fn binop(
        &mut self,
        span: &Span,
        left: &UntypedExpr,
        op: BinOpKind,
        right: &UntypedExpr,
    ) -> InterpretterResult {
        use BinOpKind::{Add, Divide, IntegerDivide, Mod, Multiply, Subtract};
        use Error::{BinOp, DivideBy0, ModBy0};
        let left_val = self.expr(left)?;
        let right_val = self.expr(right)?;
        match (left_val, op, right_val) {
            (a, Add, b) => {
                (a + b).map_err(|(a, b)| FlowControl::Error(BinOp(span.clone(), a, op, b)))
            }
            (a, Subtract, b) => {
                (a - b).map_err(|(a, b)| FlowControl::Error(BinOp(span.clone(), a, op, b)))
            }
            (a, Multiply, b) => {
                (a * b).map_err(|(a, b)| FlowControl::Error(BinOp(span.clone(), a, op, b)))
            }
            (_, Mod, Value::Integer(0)) => Err(FlowControl::Error(ModBy0(span.clone()))),
            (_, Divide | IntegerDivide, Value::Integer(0)) => {
                Err(FlowControl::Error(DivideBy0(span.clone())))
            }
            (a, Mod, b) => {
                (a % b).map_err(|(a, b)| FlowControl::Error(BinOp(span.clone(), a, op, b)))
            }
            (a, Divide, b) => {
                (a / b).map_err(|(a, b)| FlowControl::Error(BinOp(span.clone(), a, op, b)))
            }
            // (Value::Integer(_), Mod, Value::Integer(0)) => {
            // return Err(FlowControl::Error(ModBy0(span.clone())))
            // }
            // (Value::Integer(l), Mod, Value::Integer(r)) => Value::Integer(l % r),
            // #[allow(clippy::cast_precision_loss)]
            // (Value::Integer(l), Divide, Value::Integer(r)) => Value::Float(l as f64 / r as f64),
            // (Value::Integer(l), IntegerDivide, Value::Integer(r)) => Value::Integer(l / r),

            // (Value::Char(l), Subtract, Value::Char(r)) => Value::Integer(l as i64 - r as i64),

            // (Value::Float(l), Add, Value::Float(r)) => Value::Float(l + r),
            // (Value::Float(l), Multiply, Value::Float(r)) => Value::Float(l * r),
            // (Value::Float(l), Subtract, Value::Float(r)) => Value::Float(l - r),

            // (Value::Str(l), Add, Value::Str(r)) => Value::Str(l + &r),
            // (Value::Tuple(l), Add, Value::Tuple(mut r)) => {
            // let mut t = l;
            // t.append(&mut r);
            // Value::Tuple(t)
            // }
            (l, op, r) => Err(FlowControl::Error(BinOp(span.clone(), l, op, r))),
        }
    }

    fn short_circuit_binop(
        &mut self,
        span: &Span,
        left: &UntypedExpr,
        op: ShortCircuitBinOpKind,
        right: &UntypedExpr,
    ) -> InterpretterResult {
        use ShortCircuitBinOpKind::{BoolAnd, BoolOr, InvertedCall, Pipe};
        let mut left_val = self.expr(left)?;
        let val = match (&mut left_val, op) {
            (Value::Bool(false), BoolAnd) => Value::Bool(false),
            (Value::Bool(true), BoolOr) => Value::Bool(true),
            (Value::Bool(true), BoolAnd) | (Value::Bool(false), BoolOr) => {
                match self.expr(right)? {
                    Value::Bool(b) => Value::Bool(b),
                    v => {
                        return Err(FlowControl::Error(Error::ShortCircuitBinOpErrorTwo(
                            span.clone(),
                            left_val,
                            op,
                            v,
                        )))
                    }
                }
            }
            (_a, InvertedCall) => self.invert_call_or_call(left_val, right, span)?,
            (Value::Array(a), Pipe) => self.pipe(a.take().into_iter(), right, span)?,
            (Value::Str(s), Pipe) => self.pipe(s.chars().map(Value::Char), right, span)?,
            (Value::Iterable(iter), Pipe) => self.pipe(iter.into_iter(), right, span)?,
            (_, op) => {
                return Err(FlowControl::Error(Error::ShortCircuitBinOpErrorOne(
                    span.clone(),
                    left_val,
                    op,
                )))
            }
        };
        Ok(val)
    }

    fn invert_call_or_call(
        &mut self,
        left: Value,
        right: &UntypedExpr,
        span: &Span,
    ) -> InterpretterResult {
        let mut args: Vec<Value> = vec![left];
        let b = if let UntypedExprKind::RValue(RValueKind::Call {
            callable,
            args: int_args,
        }) = &right.value
        {
            let callable = self.expr(callable)?;
            args.append(&mut self.exprs(int_args)?);
            callable
        } else {
            self.expr(right)?
        };
        self.evaluate_call(b, args, span)
    }

    fn flat_binop(
        &mut self,
        _span: &Span,
        first: &UntypedExpr,
        rest: &[(FlatBinOpKind, Box<UntypedExpr>)],
    ) -> InterpretterResult {
        let mut left_val = self.expr(first)?;
        let mut result = true;
        for (op, expr) in rest {
            let right_val = self.expr(expr)?;
            result = match op {
                FlatBinOpKind::CmpEquals => left_val == right_val,
                FlatBinOpKind::CmpNotEquals => left_val != right_val,
                FlatBinOpKind::GreaterThan => left_val > right_val,
                FlatBinOpKind::LessThan => left_val < right_val,
                FlatBinOpKind::GreaterThanOrEqual => left_val >= right_val,
                FlatBinOpKind::LessThanOrEqual => left_val <= right_val,
            };
            left_val = right_val;
        }
        Ok(Value::Bool(result))
    }

    fn assignment(
        &mut self,
        left: &UntypedLValue,
        right: &UntypedExpr,
        span: &Span,
    ) -> InterpretterResult {
        let right = self.expr(right)?;
        self.assignment_one(&left.value, right, span)
    }

    fn try_assignment(
        &mut self,
        left: &UntypedExpr,
        right: Value,
        span: &Span,
    ) -> InterpretterResult {
        match &left.value {
            UntypedExprKind::RValue(_) => {
                Err(FlowControl::Error(Error::CannotAssign(span.clone())))
            }
            UntypedExprKind::LValue(l) => self.assignment_one(l, right, span),
        }
    }

    fn assignment_one(
        &mut self,
        left: &LValueKind,
        right: Value,
        span: &Span,
    ) -> InterpretterResult {
        match (left, right) {
            (LValueKind::Variable(ref s), right) => match self.scopes.get_mut(s) {
                Ok(v) => *v = right,
                Err(GetMutError::NoSuchKey) => {
                    return Err(FlowControl::Error(Error::NoSuchVariable(
                        span.clone(),
                        s.clone(),
                    )))
                }
                Err(GetMutError::IsLetVar) => {
                    return Err(FlowControl::Error(Error::VariableIsConstant(
                        span.clone(),
                        s.clone(),
                    )))
                }
            },
            (LValueKind::Tuple(lvals), Value::Tuple(t)) => {
                if lvals.len() != t.len() {
                    return Err(FlowControl::Error(Error::AssignmentLengthMismatch(
                        span.clone(),
                        lvals.len(),
                        Value::Tuple(t),
                    )));
                }
                for (l, r) in lvals.iter().zip(t.into_iter()) {
                    self.try_assignment(l, r, span)?;
                }
            }

            (LValueKind::Tuple(lvals), Value::Array(t)) => {
                if lvals.len() != t.borrow().len() {
                    return Err(FlowControl::Error(Error::AssignmentLengthMismatch(
                        span.clone(),
                        lvals.len(),
                        Value::Array(t),
                    )));
                }
                for (l, r) in lvals.iter().zip(t.take().into_iter()) {
                    self.try_assignment(l, r, span)?;
                }
            }

            #[allow(clippy::manual_let_else)]
            (LValueKind::BracketExpr { left, subscript }, val) => {
                let left = match left.value {
                    UntypedExprKind::LValue(LValueKind::Variable(ref s)) => s,
                    _ => todo!(),
                };
                let idx = self.expr(subscript)?;
                let Some(left) = self.scopes.get(left) else {
                    return Err(Error::NoSuchVariable(span.clone(), left.clone()))?;
                };
                let Value::Integer(idx) = idx else {
                    todo!();
                };
                match left {
                    Value::Array(a) => {
                        a.borrow_mut()[usize::try_from(idx).map_err(Error::BadConversion)?] = val;
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        };
        Ok(Value::None)
    }

    fn for_(
        &mut self,
        item: &UntypedLValue,
        items: &UntypedExpr,
        body: &UntypedExpr,
    ) -> InterpretterResult {
        use DeclarationKind::Var;
        let right = self.expr(items)?;
        match right {
            Value::Tuple(v) => self.do_for(item, v.into_iter(), body),
            Value::Array(v) => self.do_for(item, v.take().into_iter(), body),
            Value::Str(s) => {
                let items = s.chars().map(Value::Char);
                self.do_for(item, items.into_iter(), body)
            }
            Value::Iterable(iterable) => {
                for val in iterable {
                    match item.value {
                        LValueKind::Variable(ref s) => {
                            self.scopes.insert(s.clone(), val, Var);
                        }
                        _ => todo!(),
                    };
                    self.expr(body)?;
                }
                Ok(Value::None)
            }
            _ => Err(FlowControl::Error(Error::CannotIterateOver(
                items.span.clone(),
                right,
            ))),
        }
    }

    fn do_for<T: Iterator<Item = Value>>(
        &mut self,
        item: &UntypedLValue,
        items: T,
        body: &UntypedExpr,
    ) -> InterpretterResult {
        use DeclarationKind::Var;
        for val in items {
            match item.value {
                LValueKind::Variable(ref s) => {
                    self.scopes.insert(s.clone(), val, Var);
                }
                _ => todo!(),
            };
            self.expr(body)?;
        }
        Ok(Value::None)
    }

    fn if_(
        &mut self,
        condition: &UntypedExpr,
        body: &UntypedExpr,
        else_: Option<&UntypedExpr>,
    ) -> InterpretterResult {
        let condition_val = self.expr(condition)?;
        match (&condition_val, else_) {
            (Value::Bool(true), _) => self.expr(body),
            (Value::Bool(false), Some(else_)) => self.expr(else_),
            (Value::Bool(false), None) => Ok(Value::None),
            _ => Err(FlowControl::Error(Error::NonBoolCondition(
                condition.span.clone(),
                condition_val,
            ))),
        }
    }

    fn while_(&mut self, condition: &UntypedExpr, body: &UntypedExpr) -> InterpretterResult {
        loop {
            let condition_val = self.expr(condition)?;
            match condition_val {
                Value::Bool(true) => match self.expr(body) {
                    Err(FlowControl::Break(v)) => return Ok(v),
                    Err(FlowControl::Continue) => {}
                    a => {
                        a?;
                    }
                },
                Value::Bool(false) => break,
                _ => {
                    return Err(FlowControl::Error(Error::NonBoolCondition(
                        condition.span.clone(),
                        condition_val,
                    )))
                }
            }
        }
        Ok(Value::None)
    }

    fn variable(&mut self, span: &Span, name: &str) -> InterpretterResult {
        match self.scopes.clone_or_take(name) {
            Some(Value::Moved) => Err(FlowControl::Error(Error::VariableWasMoved(
                span.clone(),
                name.to_owned(),
            ))),
            Some(v) => Ok(v),
            None => Err(FlowControl::Error(Error::NoSuchVariable(
                span.clone(),
                name.to_owned(),
            ))),
        }
    }

    fn block(&mut self, exprs: &[UntypedExpr]) -> InterpretterResult {
        self.scopes.open();
        let mut val = Value::None;
        for e in exprs {
            val = self.expr(e)?;
        }
        self.scopes.close();
        Ok(val)
    }

    fn evaluate_call(
        &mut self,
        callable: Value,
        mut args: Vec<Value>,
        span: &Span,
    ) -> InterpretterResult {
        match callable {
            Value::ExternalFunc(c) => Ok(c(self, &mut args)?),
            Value::Lambda(l) => {
                self.scopes.open();
                self.lambda_args.push(args);
                let result = self.expr(&l);
                self.lambda_args
                    .pop()
                    .expect("should have popped the args we pushed");
                self.scopes.close();
                result
            }
            _ => Err(FlowControl::Error(Error::NotCallable(
                span.clone(),
                callable,
            ))),
        }
    }

    fn call(
        &mut self,
        callable: &UntypedExpr,
        args: &[UntypedExpr],
        span: &Span,
    ) -> InterpretterResult {
        let callable_val = self.expr(callable)?;
        let args = args
            .iter()
            .map(|a| self.expr(a))
            .collect::<Result<Vec<_>, _>>()?;
        self.evaluate_call(callable_val, args, span)
    }

    fn lambda_arg(&mut self, num: usize, span: &Span) -> InterpretterResult {
        assert!(!self.lambda_args.is_empty());
        #[allow(clippy::unwrap_used)]
        let args = self.lambda_args.last_mut().unwrap();
        if num >= args.len() {
            return Err(FlowControl::Error(Error::NotEnoughArguments(
                span.clone(),
                num,
                self.lambda_args.len(),
            )));
        }
        Ok(args[num].clone_or_take())
    }

    fn pipe<T: Iterator<Item = Value>>(
        &mut self,
        iterable: T,
        callable: &UntypedExpr,
        span: &Span,
    ) -> InterpretterResult {
        let mut rtn = Vec::new();
        for val in iterable {
            let val = self.invert_call_or_call(val, callable, span)?;
            let Value::None = val else {
                rtn.push(val);
                continue;
            };
        }
        Ok(Value::Array(Rc::new(RefCell::new(rtn))))
    }

    fn bracket(
        &mut self,
        span: &Span,
        left: &UntypedExpr,
        subscript: &UntypedExpr,
    ) -> InterpretterResult {
        let mut left = self.expr(left)?;
        let subscript = self.expr(subscript)?;
        match (&mut left, &subscript) {
            (Value::Array(a), Value::Integer(i)) => {
                // This does not bring joy
                let b = usize::try_from(*i);
                let len = a.borrow().len();
                if *i < 0 || b.map_err(Error::BadConversion)? > len {
                    Err(Error::IndexOutOfBound(span.clone(), left, subscript, len))?
                } else {
                    Ok(a.borrow_mut()[b.map_err(Error::BadConversion)?].clone_or_take())
                }
            }
            (Value::Tuple(a), Value::Integer(i)) => {
                // This does not bring joy
                let b = usize::try_from(*i);
                if *i < 0 || b.map_err(Error::BadConversion)? > a.len() {
                    let l = a.len();
                    Err(Error::IndexOutOfBound(span.clone(), left, subscript, l))?
                } else {
                    Ok(a[b.map_err(Error::BadConversion)?].clone_or_take())
                }
            }
            (_, _) => Err(FlowControl::Error(Error::CannotIndex(span.clone(), left))),
        }
    }

    fn multiexpr_common(&mut self, values: &[UntypedExpr]) -> Result<Vec<Value>, FlowControl> {
        values.iter().map(|v| self.expr(v)).collect()
    }

    fn tuple(&mut self, values: &[UntypedExpr]) -> InterpretterResult {
        Ok(Value::Tuple(self.multiexpr_common(values)?))
    }

    fn array(&mut self, values: &[UntypedExpr]) -> InterpretterResult {
        Ok(Value::array(self.multiexpr_common(values)?))
    }

    fn declaration(
        &mut self,
        kind: DeclarationKind,
        names: &DeclIds,
        value: Value,
        span: &Span,
    ) -> InterpretterResult {
        match names {
            DeclIds::One(n) => self.scopes.insert(n.clone(), value, kind),
            DeclIds::Many(ns) => match value {
                Value::Tuple(a) => {
                    for (name, value) in ns.iter().zip(a) {
                        self.declaration(kind, name, value, span)?;
                    }
                }
                Value::Array(a) => {
                    for (name, value) in ns.iter().zip(a.borrow_mut().iter_mut()) {
                        self.declaration(kind, name, value.clone_or_take(), span)?;
                    }
                }
                v => {
                    return Err(FlowControl::Error(Error::AssignmentLengthMismatch(
                        span.clone(),
                        ns.len(),
                        v,
                    )))
                }
            },
        }
        Ok(Value::None)
    }

    fn break_(&mut self, subexpr: Option<&UntypedExpr>) -> InterpretterResult {
        Err(FlowControl::Break(if let Some(s) = subexpr {
            self.expr(s)?
        } else {
            Value::None
        }))
    }
}
