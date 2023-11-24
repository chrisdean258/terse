use crate::{
    span::{Span, TextLocator},
    token::{Token, TokenKind},
};
use std::iter::Peekable;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexError {
    #[error("{1}: unexpecteded character `{0}`")]
    Char(char, Span),
    #[error("unexpected EOF while lexing string beginning at `{0}`")]
    EOFString(Span),
}

pub struct Lexer<I: Iterator<Item = char>> {
    characters: Peekable<TextLocator<I>>,
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Result<Token, LexError>;
    fn next(&mut self) -> Option<Self::Item> {
        use TokenKind::*;
        while self
            .characters
            .next_if(|(c, _)| c.is_whitespace())
            .is_some()
        {}
        let (ch, loc) = self.characters.peek()?.clone();

        macro_rules! lex_tree {
            ($default:expr) => {
                Some(Ok(self.tag($default)))
            };
            ($pat_paramtern:pat => $($rest:tt)*) => {
                Some($pat_paramtern) => lex_tree!($($rest)*),
            };
            ($default:expr, {$($subpat_param:literal => $val:expr $(,{$($rest:tt)*})?),+}) => {{
                let _ = self.characters.next();
                match self.characters.peek().map(|(c, _l)| *c) {
                    $(Some($subpat_param) => lex_tree!($val $(,{$($rest)*})?),)+
                    _ => lex_tree!($default)
                }
            }};
        }
        let _qqqq = 0;
        match ch {
            '0'..='9' => Some(self.num()),
            '"' => Some(self.string()),
            '=' => lex_tree!(SingleEquals, {
                '=' => DoubleEquals,
                '>' => FatArrow
            }),
            '<' => lex_tree!(LessThan, {
                '=' => DoubleEquals,
                '<' => LessThanOrEqual, {
                    '=' => BitShiftLeftEquals
                }
            }),
            _ => Some(Err(LexError::Char(ch, loc.clone()))),
        }
    }
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(label: String, characters: I) -> Self {
        Self {
            characters: TextLocator::new(label, characters).peekable(),
        }
    }

    fn collect_while<T>(&mut self, mut cond: T) -> Option<(String, Span)>
    where
        T: FnMut(char) -> bool,
    {
        let mut output: String = String::new();
        let mut final_loc = None;
        let (first_char, start) = self.characters.next_if(|(c, _l)| cond(*c))?;
        output.push(first_char);
        while let Some((c, loc)) = self.characters.next_if(|(c, _l)| cond(*c)) {
            final_loc = Some(loc);
            output.push(c);
        }
        return Some((output, start.to(final_loc.as_ref().unwrap_or(&start))));
    }

    fn tag(&mut self, untagged: TokenKind) -> Token {
        let Some((_ch, loc)) = self.characters.next() else {
            unreachable!("Tried consuming and tagging on empty stream of characters");
        };
        Token {
            span: loc,
            value: untagged,
        }
    }

    fn num(&mut self) -> Result<Token, LexError> {
        let Some((nums, span)) = self.collect_while(|c| c.is_numeric()) else {
            unreachable!("Called lexer.num() without a numeral next in the lexer")
        };
        let Some(('.', loc)) = self.characters.next_if(|(c, _l)| *c == '.') else {
            return Ok(Token {
                span,
                value: TokenKind::Integer(
                    nums.parse()
                        .expect("numerical string failed to parse as int"),
                ),
            });
        };
        if let Some((nums2, span2)) = self.collect_while(|c| c.is_numeric()) {
            Ok(Token {
                span: span.to(&span2),
                value: TokenKind::Float(
                    format!("{nums}.{nums2}")
                        .parse()
                        .expect("numerical string failed to parse as int"),
                ),
            })
        } else {
            Ok(Token {
                span: span.to(&loc),
                value: TokenKind::Float(
                    nums.parse()
                        .expect("numerical string failed to parse as int"),
                ),
            })
        }
    }

    fn escape(string: &str) -> String {
        let mut output = String::with_capacity(string.len());
        let mut prev_slash = false;
        for c in string.chars() {
            if c == '\\' {
                prev_slash = true;
                continue;
            }
            let next = if prev_slash {
                match c {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '0' => '\0',
                    c => c,
                }
            } else {
                c
            };
            prev_slash = false;
            output.push(next);
        }
        output
    }

    fn string(&mut self) -> Result<Token, LexError> {
        let Some(('"', loc)) = self.characters.next_if(|(c, _l)| *c == '"') else {
            unreachable!("Called lexer.string() without a \" next in the lexer")
        };
        let mut prev_slash = false;
        let Some((chars, _)) = self.collect_while(|c| {
            let rtn = c != '"' || prev_slash;
            prev_slash = c == '\\';
            rtn
        }) else {
            return Err(LexError::EOFString(loc));
        };
        if let Some(('"', loc2)) = self.characters.next_if(|(c, _l)| *c == '"') {
            Ok(Token {
                span: loc.to(&loc2),
                value: TokenKind::String(Self::escape(&chars)),
            })
        } else {
            Err(LexError::EOFString(loc))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sequence() {
        let test = "\"this is as\" 1 \"ssfsd\"\"lsdkfjsd\" 1 12 12.12 12. 1";
        let mut l = Lexer::new("test".to_owned(), test.chars());

        macro_rules! test_lex {
            ($lexer:expr => $expt:pat) => {
                assert!(matches!($lexer.next().unwrap().unwrap().value, $expt))
            };
        }

        test_lex!(l => TokenKind::String(_));
        test_lex!(l => TokenKind::Integer(1));
        test_lex!(l => TokenKind::String(_));
        test_lex!(l => TokenKind::String(_));
        test_lex!(l => TokenKind::Integer(1));
        test_lex!(l => TokenKind::Integer(12));
        test_lex!(l => TokenKind::Float(_));
        test_lex!(l => TokenKind::Float(_));
        test_lex!(l => TokenKind::Integer(1));
        assert!(l.next().is_none())
    }

    #[test]
    fn string() {
        let test = "\"test\\n\"";
        let mut l = Lexer::new("test".to_owned(), test.chars());

        let TokenKind::String(s) = l.next().unwrap().unwrap().value else {
            panic!("Lexer did not yield String token");
        };

        assert_eq!(s, "test\n");
    }

    #[test]
    fn float() {
        let test = "1.2";
        let mut l = Lexer::new("test".to_owned(), test.chars());

        let TokenKind::Float(s) = l.next().unwrap().unwrap().value else {
            panic!("Lexer did not yield Float token");
        };

        assert_eq!(s, 1.2);
    }

    #[test]
    fn locations() {
        let test = "\"this is as\" 1 \n\"ssfsd\"\n\"lsdkfjsd\" 1 12 12.12 12. 1";
        let mut l = Lexer::new("test".to_owned(), test.chars());

        macro_rules! span {
            ($lexer:expr) => {
                $lexer.next().unwrap().unwrap().span
            };
        }

        assert_eq!(span!(l).to_string(), "test:1,1-1,12");
        assert_eq!(span!(l).to_string(), "test:1,14");
        assert_eq!(span!(l).to_string(), "test:2,1-2,7");
        assert_eq!(span!(l).to_string(), "test:3,1-3,10");
        assert_eq!(span!(l).to_string(), "test:3,12");
        assert_eq!(span!(l).to_string(), "test:3,14-3,15");
        assert_eq!(span!(l).to_string(), "test:3,17-3,21");
        assert_eq!(span!(l).to_string(), "test:3,23-3,25");
        assert_eq!(span!(l).to_string(), "test:3,27");
        assert!(l.next().is_none())
    }
}
