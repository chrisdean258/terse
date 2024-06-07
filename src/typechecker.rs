#![allow(dead_code)]
use crate::expression::{TypedAst, TypedExpr, UntypedAst, UntypedExpr};

pub struct TypeChecker {}

pub enum Error {}

type TypeCheckerResult = Result<TypedExpr, Vec<Error>>;

impl TypeChecker {
    pub fn new() -> Self {
        Self {}
    }

    pub fn typecheck(&mut self, tree: UntypedAst) -> Result<TypedAst, Vec<Error>> {
        let mut exprs = Vec::new();
        let mut errs = Vec::new();
        for expr in tree.exprs {
            match self.expr(expr) {
                Ok(te) => exprs.push(te),
                Err(e) => errs.extend(e),
            }
        }
        if errs.is_empty() {
            Ok(TypedAst { exprs })
        } else {
            Err(errs)
        }
    }

    fn expr(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Vec<Error>> {
        drop(expr);
        todo!()
    }
}
