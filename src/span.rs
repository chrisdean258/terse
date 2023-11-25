use std::{
    cell::RefCell,
    cmp::{max, min},
    fmt::{Display, Formatter},
    iter::Enumerate,
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct Span {
    meta: Rc<ControlBlock>,
    from: usize,
    to: usize,
}

impl Span {
    pub fn to(&self, other: &Self) -> Span {
        let from = min(self.from, other.from);
        let to = max(self.to, other.to);
        let meta = self.meta.clone();
        Span { from, to, meta }
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

#[derive(Debug)]
struct ControlBlock {
    label: String,
    newlines: RefCell<Vec<usize>>,
}

#[derive(Debug)]
pub struct TextLocator<I: Iterator<Item = char>> {
    characters: Enumerate<I>,
    meta: Rc<ControlBlock>,
}

impl<I: Iterator<Item = char>> Iterator for TextLocator<I> {
    type Item = (char, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let (idx, chr) = self.characters.next()?;
        if chr == '\n' {
            self.meta.newlines.borrow_mut().push(idx);
        }
        Some((
            chr,
            Span {
                from: idx,
                to: idx,
                meta: self.meta.clone(),
            },
        ))
    }
}

impl<I: Iterator<Item = char>> TextLocator<I> {
    pub fn new(label: String, text: I) -> Self {
        Self {
            characters: text.enumerate(),
            meta: Rc::new(ControlBlock {
                label,
                newlines: RefCell::new(Vec::new()),
            }),
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn placement_test(test_num: usize, text: &str, idx: usize, expt: (usize, usize)) {
        let mut chit = TextLocator::new(format!("Placement test {test_num}"), text.chars());
        for _ in &mut chit {}
        let res = chit.meta.row_col(idx);
        assert_eq!(res, expt);
    }

    #[test]
    fn placement() {
        placement_test(1, "testing\ntesting", 0, (1, 1));
        placement_test(2, "testing\ntesting", 7, (1, 8));
        placement_test(3, "testing\ntesting", 8, (2, 1));
        placement_test(4, "testing\ntesting", 14, (2, 7));
        placement_test(5, "\ntesting\ntesting", 0, (1, 1));
        placement_test(6, "\ntesting\ntesting", 1, (2, 1));
    }
}
