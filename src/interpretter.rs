use crate::{
    expression::{
        AssignmentKind, BinOpKind, DeclIds, DeclarationKind, FlatBinOpKind, LValueKind, RValueKind,
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

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("{0}: Cannot evaluate `{1} {2}`")]
    ShortCircuitBinOpErrorOne(Span, Value, ShortCircuitBinOpKind),
    #[error("{0}: Cannot evaluate `{1} {2} {3}`")]
    ShortCircuitBinOpErrorTwo(Span, Value, ShortCircuitBinOpKind, Value),
    #[error("{0}: Cannot evaluate `{1} {2} {3}`")]
    FlatBinOp(Span, Value, FlatBinOpKind, Value),
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
    #[error("{0}: Non boolean value `{1}` in if condition")]
    NonBoolIfCondition(Span, Value),
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
}

pub struct ScopeTable {
    scopes: Vec<HashMap<String, (Value, DeclarationKind)>>,
}

#[derive(Debug, Clone)]
enum FlowControl {
    Error(Error),
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
            val = self.expr(expr).map_err(|e| {
                let FlowControl::Error(ie) = e;
                ie
            })?;
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
                RValueKind::Assignment { left, op, right } => {
                    self.assignment(left, *op, right, &expr.span)
                }
                RValueKind::For { item, items, body } => self.for_(item, items, body),
                RValueKind::If { condition, body } => self.if_(condition, body),
                RValueKind::Block(exprs) => self.block(exprs),
                RValueKind::Call { callable, args } => self.call(callable, args, &expr.span),
                RValueKind::Lambda(subexpr) => Ok(Value::Lambda(subexpr.clone())),
                RValueKind::LambdaArg(i) => self.lambda_arg(*i, &expr.span),
                RValueKind::Array(a) => self.array(a),
                RValueKind::Declaration { kind, names, value } => {
                    let v = self.expr(value)?;
                    self.declaration(*kind, names, v, &expr.span)
                }
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
        let val = match (left_val, op, right_val) {
            (Value::Integer(l), Add, Value::Integer(r)) => Value::Integer(l + r),
            (Value::Integer(l), Subtract, Value::Integer(r)) => Value::Integer(l - r),
            (Value::Integer(l), Multiply, Value::Integer(r)) => Value::Integer(l * r),
            (Value::Integer(_), Mod, Value::Integer(0)) => {
                return Err(FlowControl::Error(ModBy0(span.clone())))
            }
            (Value::Integer(l), Mod, Value::Integer(r)) => Value::Integer(l % r),
            (Value::Integer(_), Divide | IntegerDivide, Value::Integer(0)) => {
                return Err(FlowControl::Error(DivideBy0(span.clone())))
            }
            #[allow(clippy::cast_precision_loss)]
            (Value::Integer(l), Divide, Value::Integer(r)) => Value::Float(l as f64 / r as f64),
            (Value::Integer(l), IntegerDivide, Value::Integer(r)) => Value::Integer(l / r),

            (Value::Char(l), Subtract, Value::Char(r)) => Value::Integer(l as i64 - r as i64),

            (Value::Float(l), Add, Value::Float(r)) => Value::Float(l + r),
            (Value::Float(l), Multiply, Value::Float(r)) => Value::Float(l * r),
            (Value::Float(l), Subtract, Value::Float(r)) => Value::Float(l - r),

            (Value::Str(l), Add, Value::Str(r)) => Value::Str(l + &r),
            (Value::Tuple(l), Add, Value::Tuple(mut r)) => {
                let mut t = l;
                t.append(&mut r);
                Value::Tuple(t)
            }
            (l, op, r) => return Err(FlowControl::Error(BinOp(span.clone(), l, op, r))),
        };
        Ok(val)
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
            (Value::Array(a), Pipe) => self.pipe(a.borrow().iter(), right, span)?,
            (Value::Str(s), Pipe) => {
                let items = s.chars().map(Value::Char).collect::<Vec<_>>();
                self.pipe(items.iter(), right, span)?
            }
            (Value::Iterable(iter), Pipe) => {
                let items = iter.collect::<Vec<_>>();
                self.pipe(items.iter(), right, span)?
            }
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
        self.evaluate_call(&b, args, span)
    }

    fn flat_binop(
        &mut self,
        span: &Span,
        first: &UntypedExpr,
        rest: &[(FlatBinOpKind, Box<UntypedExpr>)],
    ) -> InterpretterResult {
        let mut result = self.expr(first)?;
        for (op, expr) in rest {
            let right_val = self.expr(expr)?;
            result = match (result, op, right_val) {
                (Value::Str(l), FlatBinOpKind::CmpEquals, Value::Str(r)) => {
                    assert!(rest.len() < 2, "Chained comparisons not implmented yet");
                    Value::Bool(l == r)
                }
                (Value::Str(l), FlatBinOpKind::CmpNotEquals, Value::Str(r)) => {
                    assert!(rest.len() < 2, "Chained comparisons not implmented yet");
                    Value::Bool(l != r)
                }
                (Value::Char(l), FlatBinOpKind::LessThanOrEqual, Value::Char(r)) => {
                    assert!(rest.len() < 2, "Chained comparisons not implmented yet");
                    Value::Bool(l <= r)
                }
                (Value::Char(l), FlatBinOpKind::LessThan, Value::Char(r)) => {
                    assert!(rest.len() < 2, "Chained comparisons not implmented yet");
                    Value::Bool(l < r)
                }
                (Value::Integer(l), FlatBinOpKind::CmpEquals, Value::Integer(r)) => {
                    assert!(rest.len() < 2, "Chained comparisons not implmented yet");
                    Value::Bool(l == r)
                }
                (Value::Integer(l), FlatBinOpKind::GreaterThan, Value::Integer(r)) => {
                    assert!(rest.len() < 2, "Chained comparisons not implmented yet");
                    Value::Bool(l > r)
                }
                (Value::Integer(l), FlatBinOpKind::LessThan, Value::Integer(r)) => {
                    assert!(rest.len() < 2, "Chained comparisons not implmented yet");
                    Value::Bool(l < r)
                }
                (Value::Integer(l), FlatBinOpKind::LessThanOrEqual, Value::Integer(r)) => {
                    assert!(rest.len() < 2, "Chained comparisons not implmented yet");
                    Value::Bool(l <= r)
                }
                (l, op, r) => {
                    return Err(FlowControl::Error(Error::FlatBinOp(
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
        op: AssignmentKind,
        right: &UntypedExpr,
        span: &Span,
    ) -> InterpretterResult {
        let right = self.expr(right)?;
        self.assignment_one(&left.value, op, right, span)
    }

    fn try_assignment(
        &mut self,
        left: &UntypedExpr,
        op: AssignmentKind,
        right: Value,
        span: &Span,
    ) -> InterpretterResult {
        match &left.value {
            UntypedExprKind::RValue(_) => {
                Err(FlowControl::Error(Error::CannotAssign(span.clone())))
            }
            UntypedExprKind::LValue(l) => self.assignment_one(l, op, right, span),
        }
    }

    fn assignment_one(
        &mut self,
        left: &LValueKind,
        op: AssignmentKind,
        right: Value,
        span: &Span,
    ) -> InterpretterResult {
        match (left, right.clone()) {
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
                        right,
                    )));
                }
                for (l, r) in lvals.iter().zip(t) {
                    self.try_assignment(l, op, r, span)?;
                }
            }

            (LValueKind::Tuple(lvals), Value::Array(t)) => {
                if lvals.len() != t.borrow().len() {
                    return Err(FlowControl::Error(Error::AssignmentLengthMismatch(
                        span.clone(),
                        lvals.len(),
                        right,
                    )));
                }
                for (l, r) in lvals.iter().zip(t.borrow().iter()) {
                    self.try_assignment(l, op, r.clone(), span)?;
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
        Ok(right)
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
            Value::Tuple(v) => self.do_for(item, &mut v.iter(), body),
            Value::Array(v) => self.do_for(item, &mut v.borrow().iter(), body),
            Value::Str(s) => {
                let items = s.chars().map(Value::Char).collect::<Vec<_>>();
                self.do_for(item, &mut items.iter(), body)
            }
            Value::Iterable(iterable) => {
                for val in iterable {
                    match item.value {
                        LValueKind::Variable(ref s) => {
                            self.scopes.insert(s.clone(), val.clone(), Var);
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

    fn do_for<'a, T: Iterator<Item = &'a Value>>(
        &mut self,
        item: &UntypedLValue,
        items: &'a mut T,
        body: &UntypedExpr,
    ) -> InterpretterResult {
        use DeclarationKind::Var;
        for val in items {
            match item.value {
                LValueKind::Variable(ref s) => {
                    self.scopes.insert(s.clone(), val.clone(), Var);
                }
                _ => todo!(),
            };
            self.expr(body)?;
        }
        Ok(Value::None)
    }

    fn if_(&mut self, condition: &UntypedExpr, body: &UntypedExpr) -> InterpretterResult {
        let condition_val = self.expr(condition)?;
        match condition_val {
            Value::Bool(true) => {
                self.expr(body)?;
            }
            Value::Bool(false) => {}
            _ => {
                return Err(FlowControl::Error(Error::NonBoolIfCondition(
                    condition.span.clone(),
                    condition_val,
                )))
            }
        }
        Ok(Value::None)
    }

    fn variable(&mut self, span: &Span, name: &str) -> InterpretterResult {
        self.scopes
            .get(name)
            .cloned()
            .ok_or_else(|| FlowControl::Error(Error::NoSuchVariable(span.clone(), name.to_owned())))
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
        callable: &Value,
        mut args: Vec<Value>,
        span: &Span,
    ) -> InterpretterResult {
        match callable {
            Value::ExternalFunc(c) => Ok(c(&mut args)?),
            Value::Lambda(l) => {
                self.scopes.open();
                self.lambda_args.push(args);
                let result = self.expr(l);
                self.lambda_args
                    .pop()
                    .expect("should have popped the args we pushed");
                self.scopes.close();
                result
            }
            _ => Err(FlowControl::Error(Error::NotCallable(
                span.clone(),
                callable.clone(),
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
        self.evaluate_call(&callable_val, args, span)
    }

    fn lambda_arg(&mut self, num: usize, span: &Span) -> InterpretterResult {
        assert!(!self.lambda_args.is_empty());
        #[allow(clippy::unwrap_used)]
        let args = self.lambda_args.last().unwrap();
        if num >= args.len() {
            return Err(FlowControl::Error(Error::NotEnoughArguments(
                span.clone(),
                num,
                self.lambda_args.len(),
            )));
        }
        Ok(args[num].clone())
    }

    fn pipe<'a, T: Iterator<Item = &'a Value>>(
        &mut self,
        iterable: T,
        callable: &UntypedExpr,
        span: &Span,
    ) -> InterpretterResult {
        let mut rtn = Vec::new();
        for val in iterable {
            let val = self.invert_call_or_call(val.clone(), callable, span)?;
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
        let left = self.expr(left)?;
        let subscript = self.expr(subscript)?;
        match (&left, &subscript) {
            (Value::Array(a), Value::Integer(i)) => {
                // This does not bring joy
                let b = usize::try_from(*i).map_err(Error::BadConversion);
                let len = a.borrow().len();
                if *i < 0 || b.clone()? > len {
                    Err(Error::IndexOutOfBound(span.clone(), left, subscript, len))?
                } else {
                    Ok(a.borrow()[b?].clone())
                }
            }
            (Value::Tuple(a), Value::Integer(i)) => {
                // This does not bring joy
                let b = usize::try_from(*i).map_err(Error::BadConversion);
                if *i < 0 || b.clone()? > a.len() {
                    let l = a.len();
                    Err(Error::IndexOutOfBound(span.clone(), left, subscript, l))?
                } else {
                    Ok(a[b?].clone())
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
                    for (name, value) in ns.iter().zip(a.borrow().iter()) {
                        self.declaration(kind, name, value.clone(), span)?;
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
}
