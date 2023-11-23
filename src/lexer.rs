use crate::{
    textlocator::{Spannable, TextLocation, TextLocator},
    token::{Token, TokenKind},
};
use std::iter::Peekable;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexError {
    #[error("{1}: unexpecteded character `{0}`")]
    UnexpectedChar(char, TextLocation),
    #[error("unexpected EOF while lexing string beginning at `{0}`")]
    UnexpectedEOFString(TextLocation),
}

pub struct Lexer<I: Iterator<Item = char>> {
    characters: Peekable<TextLocator<I>>,
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Result<Token, LexError>;
    fn next(&mut self) -> Option<Self::Item> {
        while self
            .characters
            .next_if(|(c, _)| c.is_whitespace())
            .is_some()
        {}
        let (ch, loc) = self.characters.next()?;
        match ch {
            '0'..='9' => Some(Ok(self.num(ch, loc))),
            '"' => Some(self.string(ch, loc)),
            _ => Some(Err(LexError::UnexpectedChar(ch, loc))),
        }
    }
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(characters: I) -> Self {
        Self {
            characters: TextLocator::new(characters).peekable(),
        }
    }
    fn num(&mut self, start: char, loc: TextLocation) -> Token {
        // This is OK as we only call num with characters between '0' and '9'
        let mut num: i64 = (start as u8 - b'0') as i64;
        let mut final_loc = loc;
        while let Some((digit, loc)) = self.characters.next_if(|(c, _)| '0' <= *c && *c <= '9') {
            final_loc = loc;
            num *= 10;
            num += (digit as u8 - b'0') as i64;
        }
        Token {
            span: loc.to(final_loc),
            value: TokenKind::Number(num),
        }
    }

    fn string(&mut self, _start: char, loc: TextLocation) -> Result<Token, LexError> {
        let mut output = String::new();
        while let Some((c, loc)) = self.characters.next_if(|(c, _)| *c != '"') {
            output.push(c);
        }
        let Some(('"', final_loc)) = self.characters.next() else {
            return Err(LexError::UnexpectedEOFString(loc));
        };

        Ok(Token {
            span: loc.to(final_loc),
            value: TokenKind::String(output),
        })
    }
}
