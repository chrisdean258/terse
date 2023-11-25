use crate::{
    span::{LocatedCharacter, Span, TextLocator},
    token::{Token, TokenKind},
};
use std::mem::replace;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum LexError {
    #[error("{1}: unexpecteded character `{0}`")]
    Char(char, Span),
    #[error("unexpected EOF while lexing string beginning at `{0}`")]
    EOFString(Span),
}

pub struct Lexer<I: Iterator<Item = char>> {
    characters: TextLocator<I>,
    cache: Option<LocatedCharacter>,
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Result<Token, LexError>;
    fn next(&mut self) -> Option<Self::Item> {
        use TokenKind::*;
        while self.next_if(|c| c.is_whitespace()).is_some() {}
        let lc = self.next_char()?;

        macro_rules! lex_tree {
            ($default:expr) => {
                Some(Ok(Token { value: $default, span: lc.span}))
            };
            ($pat_paramtern:pat => $($rest:tt)*) => {
                Some($pat_paramtern) => lex_tree!($($rest)*),
            };
            ($default:expr, {$($subpat_param:literal => $val:expr $(,{$($rest:tt)*})?),+ $(,)?}) => {{
                let nc = self.next_char();
                match nc.map(|c| c.value) {
                    $(Some($subpat_param) => lex_tree!($val $(,{$($rest)*})?),)+
                    _ => lex_tree!($default)
                }
            }};
        }
        match lc.value {
            '0'..='9' => Some(self.num(lc)),
            '"' => Some(self.string(lc)),
            '_' | 'a'..='z' | 'A'..='Z' => Some(self.identifier_or_keyword(lc)),
            '=' => lex_tree!(SingleEquals, {
                '=' => DoubleEquals,
                '>' => FatArrow,
            }),
            '<' => lex_tree!(LessThan, {
                '=' => LessThanOrEqual,
                '<' => BitShiftLeft, {
                    '=' => BitShiftLeftEquals,
                }
            }),
            '>' => lex_tree!(GreaterThan, {
                '=' => GreaterThanOrEqual,
                '>' => BitShiftRight, {
                    '=' => BitShiftRightEquals,
                }
            }),
            '+' => lex_tree!(Plus, {
                '+' => Increment,
                '=' => PlusEquals,
                '>' => CrossArrow,
            }),
            '-' => lex_tree!(Minus, {
                '-' => Decrement,
                '=' => MinusEquals,
                '>' => SkinnyArrow,
            }),
            '*' => lex_tree!(Asterik, {
                '=' => TimesEquals,
            }),
            '/' => lex_tree!(ForwardSlash, {
                '/' => DoubleForwardSlash, {
                    '=' => DoubleForwardSlashEquals,
                },
                '=' => ForwardSlashEquals,
            }),
            '%' => lex_tree!(Mod, {
                '=' => ModEquals
            }),
            '!' => lex_tree!(BangSign, {
                '=' => NotEqual,
            }),
            '~' => lex_tree!(Tilde, {
                '=' => TildeEquals,
                '>' => SquigglyArrow,
            }),
            '&' => lex_tree!(Ampersand, {
                '&' => DoubleAmpersand, {
                    '=' => DoubleAmpersandEquals,
                },
                '=' => AmpersandEquals,
            }),
            '|' => lex_tree!(Pipe, {
                '|' => DoublePipe, {
                    '=' => DoublePipeEquals,
                },
                '=' => PipeEquals,
                '>' => PipeArrow,
            }),
            '^' => lex_tree!(Hat, {
                '^' => DoubleHat, {
                    '=' => DoubleHatEquals,
                },
                '=' => HatEquals,
            }),
            ',' => lex_tree!(Comma),
            '.' => lex_tree!(Dot),
            '(' => lex_tree!(OpenParen),
            ')' => lex_tree!(CloseParen),
            '[' => lex_tree!(OpenBracket),
            ']' => lex_tree!(CloseBracket),
            '{' => lex_tree!(OpenBrace),
            '}' => lex_tree!(CloseBrace),
            '$' => lex_tree!(DollarSign),
            _ => Some(Err(LexError::Char(lc.value, lc.span))),
        }
    }
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(label: String, characters: I) -> Self {
        let mut characters = TextLocator::new(label, characters);
        let cache = characters.next();
        Self { characters, cache }
    }

    fn next_char(&mut self) -> Option<LocatedCharacter> {
        replace(&mut self.cache, self.characters.next())
    }

    fn next_if<C>(&mut self, mut cond: C) -> Option<LocatedCharacter>
    where
        C: FnMut(char) -> bool,
    {
        if let Some(lc) = &self.cache {
            if cond(lc.value) {
                self.next_char()
            } else {
                None
            }
        } else {
            None
        }
    }

    fn collect_while<T>(&mut self, mut cond: T, output: &mut String) -> Option<LocatedCharacter>
    where
        T: FnMut(char) -> bool,
    {
        let mut final_char = None;
        while let Some(lc) = self.next_if(&mut cond) {
            output.push(lc.value);
            final_char = Some(lc);
        }
        final_char
    }

    fn num(&mut self, lc: LocatedCharacter) -> Result<Token, LexError> {
        let mut nums = String::new();
        nums.push(lc.value);
        let final_char = self
            .collect_while(|c| c.is_numeric(), &mut nums)
            .unwrap_or_else(|| lc.clone());
        let span = lc.span.to(&final_char.span);
        let Some(dotlc) = self.next_if(|c| c == '.') else {
            return Ok(Token {
                span,
                value: TokenKind::Integer(
                    nums.parse()
                        .expect("numerical string failed to parse as int"),
                ),
            });
        };
        nums.push('.');
        let end = self
            .collect_while(|c| c.is_numeric(), &mut nums)
            .unwrap_or(dotlc)
            .span;
        Ok(Token {
            span: span.to(&end),
            value: TokenKind::Float(
                nums.parse()
                    .expect("numerical string failed to parse as int"),
            ),
        })
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

    fn string(&mut self, lc: LocatedCharacter) -> Result<Token, LexError> {
        let mut chars = String::new();
        let mut prev_slash = false;
        let Some(_lc) = self.collect_while(
            |c| {
                let rtn = c != '"' || prev_slash;
                prev_slash = c == '\\';
                rtn
            },
            &mut chars,
        ) else {
            return Err(LexError::EOFString(lc.span));
        };
        if let Some(end_quote_lc) = self.next_if(|c| c == '"') {
            Ok(Token {
                span: lc.span.to(&end_quote_lc.span),
                value: TokenKind::String(Self::escape(&chars)),
            })
        } else {
            Err(LexError::EOFString(lc.span))
        }
    }

    fn identifier_or_keyword(&mut self, lc: LocatedCharacter) -> Result<Token, LexError> {
        let mut id = String::new();
        id.push(lc.value);
        let end = self
            .collect_while(|c| c == '_' || c.is_alphabetic() || c.is_numeric(), &mut id)
            .unwrap_or_else(|| lc.clone())
            .span;

        Ok(Token {
            span: lc.span.to(&end),
            value: TokenKind::Identifier(id),
        })
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
    fn operators() {
        let operators = vec![
            TokenKind::SingleEquals,
            TokenKind::DoubleEquals,
            TokenKind::FatArrow,
            TokenKind::LessThan,
            TokenKind::LessThanOrEqual,
            TokenKind::BitShiftLeft,
            TokenKind::BitShiftLeftEquals,
            TokenKind::GreaterThan,
            TokenKind::GreaterThanOrEqual,
            TokenKind::BitShiftRight,
            TokenKind::BitShiftRightEquals,
            TokenKind::Plus,
            TokenKind::Increment,
            TokenKind::PlusEquals,
            TokenKind::CrossArrow,
            TokenKind::Minus,
            TokenKind::Decrement,
            TokenKind::MinusEquals,
            TokenKind::SkinnyArrow,
            TokenKind::Asterik,
            TokenKind::TimesEquals,
            TokenKind::ForwardSlash,
            TokenKind::DoubleForwardSlash,
            TokenKind::ForwardSlashEquals,
            TokenKind::DoubleForwardSlashEquals,
            TokenKind::Mod,
            TokenKind::ModEquals,
            TokenKind::BangSign,
            TokenKind::NotEqual,
            TokenKind::Ampersand,
            TokenKind::AmpersandEquals,
            TokenKind::DoubleAmpersand,
            TokenKind::DoubleAmpersandEquals,
        ];

        let ops: Vec<String> = operators.iter().map(|o| o.to_string()).collect();
        let ops = ops.join(" ");

        let l = Lexer::new("operators".to_owned(), ops.chars());

        for (kind, res_token) in operators.iter().zip(l) {
            assert_eq!(res_token.unwrap().value, *kind);
        }
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
