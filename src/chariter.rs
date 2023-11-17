#![allow(dead_code)]
use std::io::Read;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
enum CharIterError {
    #[error("Queried location for character {queried} but text length is only {length}")]
    OutOfRange { queried: usize, length: usize },
    #[error(
        "Queried location for character {queried} but only processed {processed} characters so far"
    )]
    NotYetProcessed { queried: usize, processed: usize },
}

#[derive(Debug)]
struct CharIter {
    characters: Vec<u8>,
    next_character_pos: usize,
    newlines: Vec<usize>,
}

impl Iterator for CharIter {
    type Item = (u8, usize);

    fn next(&mut self) -> Option<Self::Item> {
        match self.characters.get(self.next_character_pos) {
            Some(c) => {
                if *c == b'\n' {
                    self.newlines.push(self.next_character_pos);
                }
                self.next_character_pos += 1;
                Some((*c, self.next_character_pos - 1))
            }
            None => None,
        }
    }
}

impl CharIter {
    fn from_text(text: Vec<u8>) -> Self {
        Self {
            characters: text,
            next_character_pos: 0,
            newlines: Vec::new(),
        }
    }

    fn new(mut text_source: impl Read) -> std::io::Result<Self> {
        let mut text = Vec::new();
        text_source.read_to_end(&mut text)?;
        Ok(Self::from_text(text))
    }

    fn location_one_indexed(&self, idx: usize) -> Result<(usize, usize), CharIterError> {
        if idx >= self.characters.len() {
            return Err(CharIterError::OutOfRange {
                queried: idx,
                length: self.characters.len(),
            });
        } else if idx >= self.next_character_pos {
            return Err(CharIterError::NotYetProcessed {
                queried: idx,
                processed: self.next_character_pos,
            });
        }
        let n = match self.newlines.binary_search(&idx) {
            Ok(n) => n,
            Err(n) => n,
        };
        let start = if n == 0 { 0 } else { self.newlines[n - 1] + 1 };
        Ok((n + 1, idx - start + 1))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn placement_test(text: &[u8], idx: usize, expt: Result<(usize, usize), CharIterError>) {
        let mut chit = CharIter::from_text(text.to_vec());
        for _ in &mut chit {}
        let res = chit.location_one_indexed(idx);
        assert_eq!(res, expt);
    }

    #[test]
    fn placement1() {
        placement_test(b"testing\ntesting", 0, Ok((1, 1)));
    }
    #[test]
    fn placement2() {
        placement_test(b"testing\ntesting", 7, Ok((1, 8)));
    }
    #[test]
    fn placement3() {
        placement_test(b"testing\ntesting", 8, Ok((2, 1)));
    }
    #[test]
    fn placement4() {
        placement_test(b"testing\ntesting", 14, Ok((2, 7)));
    }
    #[test]
    fn placement5() {
        placement_test(b"\ntesting\ntesting", 0, Ok((1, 1)));
    }
    #[test]
    fn placement6() {
        placement_test(b"\ntesting\ntesting", 1, Ok((2, 1)));
    }
    #[test]
    fn out_of_range() {
        placement_test(
            b"testing\ntesting",
            15,
            Err(CharIterError::OutOfRange {
                queried: 15,
                length: 15,
            }),
        );
    }
    #[test]
    fn unprocessed() {
        let chit = CharIter::from_text(b"test".to_vec());
        let res = chit.location_one_indexed(0);
        assert_eq!(
            res,
            Err(CharIterError::NotYetProcessed {
                queried: 0,
                processed: 0,
            }),
        );
    }
}
