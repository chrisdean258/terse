use std::{
    cell::RefCell,
    cmp::{max, min},
    fmt::{Display, Formatter},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct Span {
    pub meta: Rc<ControlBlock>,
    pub from: usize,
    pub to: usize,
}

impl Span {
    pub fn to(&self, other: &Self) -> Self {
        let from = min(self.from, other.from);
        let to = max(self.to, other.to);
        let meta = self.meta.clone();
        Self { meta, from, to }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let (row, col) = self.meta.row_col(self.from);
        write!(f, "{}:{row},{col}", self.meta.label)?;
        if self.from != self.to {
            let (row, col) = self.meta.row_col(self.to);
            write!(f, "-{row},{col}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LocatedCharacter {
    pub value: char,
    pub span: Span,
}

#[derive(Debug)]
pub struct ControlBlock {
    pub label: String,
    pub newlines: RefCell<Vec<usize>>,
}

impl ControlBlock {
    fn row_col(&self, idx: usize) -> (usize, usize) {
        let n = self
            .newlines
            .borrow()
            .binary_search(&idx)
            .unwrap_or_else(|n| n);
        let start = if n == 0 {
            0
        } else {
            self.newlines.borrow()[n - 1] + 1
        };
        (n + 1, idx - start + 1)
    }
}
