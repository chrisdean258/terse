use std::{
    fmt::{Display, Formatter},
    iter::Enumerate,
    str::Chars,
};

#[derive(Debug, Copy, Clone, PartialOrd, Ord, Eq, PartialEq)]
pub struct TextLocation {
    loc: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    from: TextLocation,
    to: TextLocation,
}

pub trait Spannable {
    fn to(self, other: impl Spannable) -> Span
    where
        Self: Sized,
    {
        let sf = self.first();
        let of = other.first();
        let sl = self.last();
        let ol = other.last();
        let from = if sf < of { sf } else { of };
        let to = if sl > ol { sl } else { ol };
        Span { from, to }
    }
    fn first(&self) -> TextLocation;
    fn last(&self) -> TextLocation;
    fn span(self) -> Span
    where
        Self: Sized,
    {
        Span {
            from: self.first(),
            to: self.last(),
        }
    }
}

impl Spannable for TextLocation {
    fn first(&self) -> TextLocation {
        *self
    }

    fn last(&self) -> TextLocation {
        *self
    }
}

impl Spannable for Span {
    fn first(&self) -> TextLocation {
        self.from
    }

    fn last(&self) -> TextLocation {
        self.to
    }
}

impl Display for TextLocation {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.loc)
    }
}

#[derive(Debug)]
pub struct TextLocator<I: Iterator<Item = char>> {
    characters: Enumerate<I>,
    newlines: Vec<usize>,
}

impl<I: Iterator<Item = char>> Iterator for TextLocator<I> {
    type Item = (char, TextLocation);

    fn next(&mut self) -> Option<Self::Item> {
        let (idx, chr) = self.characters.next()?;
        if chr == '\n' {
            self.newlines.push(idx);
        }
        Some((chr, TextLocation { loc: idx }))
    }
}

impl<'a> TextLocator<Chars<'a>> {
    pub fn from_text(text: &'a str) -> Self {
        Self::new(text.chars())
    }
}

impl<I: Iterator<Item = char>> TextLocator<I> {
    pub fn new(text: I) -> Self {
        Self {
            characters: text.enumerate(),
            newlines: Vec::new(),
        }
    }

    pub fn location_one_indexed(&self, idx: usize) -> (usize, usize) {
        let n = match self.newlines.binary_search(&idx) {
            Ok(n) => n,
            Err(n) => n,
        };
        let start = if n == 0 { 0 } else { self.newlines[n - 1] + 1 };
        (n + 1, idx - start + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn placement_test(text: &str, idx: usize, expt: (usize, usize)) {
        let mut chit = TextLocator::from_text(text);
        for _ in &mut chit {}
        let res = chit.location_one_indexed(idx);
        assert_eq!(res, expt);
    }

    #[test]
    fn placement() {
        placement_test("testing\ntesting", 0, (1, 1));
        placement_test("testing\ntesting", 7, (1, 8));
        placement_test("testing\ntesting", 8, (2, 1));
        placement_test("testing\ntesting", 14, (2, 7));
        placement_test("\ntesting\ntesting", 0, (1, 1));
        placement_test("\ntesting\ntesting", 1, (2, 1));
    }
}
