use crate::{
    span::{ControlBlock, Span},
    token::{Kind as TokenKind, Token},
};
use std::mem::replace;
use std::{cell::RefCell, rc::Rc};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum LexError {
    #[error("{1}: unexpecteded character `{0}`")]
    Char(char, Span),
    #[error("{1}: unexpecteded character `{0}`, expected `{1}`")]
    UnexpectedChar(Span, char),
    #[error("unexpected EOF while lexing string beginning at `{0}`")]
    EOFString(Span),
    #[error("unexpected EOF while lexing char beginning at `{0}`")]
    EOFChar(Span),
}

#[derive(Debug)]
pub struct Lexer {
    characters: Vec<char>,
    cursor: usize,
    meta: Rc<ControlBlock>,
}

impl Iterator for Lexer {
    type Item = Result<Token, LexError>;
    fn next(&mut self) -> Option<Self::Item> {
        use TokenKind::*;
        while let Some(c) = self.next_if(|c| c.is_whitespace()) {
            if c == '\n' {
                self.meta.newlines.borrow_mut().push(self.cursor - 1)
            }
        }
        let start_idx = self.cursor;
        let lc = self.next_char()?;

        macro_rules! lex_tree {
            ($default:expr) => {
                Some(Ok(Token { value: $default, span: self.span_to_here(start_idx)}))
            };
            ($pat_paramtern:pat => $($rest:tt)*) => {
                Some($pat_paramtern) => lex_tree!($($rest)*),
            };
            ($default:expr, {$($subpat_param:literal => $val:expr $(,{$($rest:tt)*})?),+ $(,)?}) => {{
                match self.next_char() {
                    $(Some($subpat_param) => lex_tree!($val $(,{$($rest)*})?),)+
                    Some(_) => {
                        self.cursor -= 1;
                        lex_tree!($default)
                    }
                    None => lex_tree!($default)
                }
            }};
        }
        match lc {
            '0'..='9' => Some(self.num(lc, start_idx)),
            '"' => Some(self.string(lc, start_idx)),
            '\'' => Some(self.char_(lc, start_idx)),
            '_' | 'a'..='z' | 'A'..='Z' => Some(self.identifier_or_keyword(lc, start_idx)),
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
            '\\' => {
                if let Some(nc) = self.next_if(|c| c.is_numeric()) {
                    let mut num = String::new();
                    num.push(nc);
                    self.collect_while(|c| c.is_numeric(), &mut num);
                    let num = num.parse().expect("Parsing on only numeric characters");
                    Some(Ok(Token {
                        span: self.span_to_here(start_idx),
                        value: LambdaArg(num),
                    }))
                } else {
                    lex_tree!(BackSlash)
                }
            }
            _ => Some(Err(LexError::Char(lc, self.span_to_here(start_idx)))),
        }
    }
}

fn escape_char(c: char) -> char {
    match c {
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        '0' => '\0',
        c => c,
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
        let next = if prev_slash { escape_char(c) } else { c };
        prev_slash = false;
        output.push(next);
    }
    output
}

impl Lexer {
    pub fn new(label: String, characters: Vec<char>) -> Self {
        let meta = Rc::new(ControlBlock {
            label,
            newlines: RefCell::new(Vec::new()),
        });
        Self {
            characters,
            cursor: 0,
            meta,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        if self.cursor < self.characters.len() {
            let next = self.cursor + 1;
            let cursor = replace(&mut self.cursor, next);
            Some(self.characters[cursor])
        } else {
            None
        }
    }

    fn must_next_char<C>(&mut self, start: usize, error: C) -> Result<char, LexError>
    where
        C: Fn(Span) -> LexError,
    {
        self.next_char()
            .ok_or_else(|| error(self.span_to_here(start)))
    }

    fn next_if<C>(&mut self, mut cond: C) -> Option<char>
    where
        C: FnMut(char) -> bool,
    {
        let lc = self.next_char()?;
        if cond(lc) {
            Some(lc)
        } else {
            self.cursor -= 1;
            None
        }
    }

    fn collect_while<T>(&mut self, mut cond: T, output: &mut String)
    where
        T: FnMut(char) -> bool,
    {
        while let Some(lc) = self.next_if(&mut cond) {
            output.push(lc);
        }
    }

    fn span_to_here(&self, from: usize) -> Span {
        self.span(from, self.cursor - 1)
    }

    fn span(&self, from: usize, to: usize) -> Span {
        Span {
            from,
            to,
            meta: self.meta.clone(),
        }
    }

    fn num(&mut self, lc: char, start_idx: usize) -> Result<Token, LexError> {
        let mut nums = String::new();
        nums.push(lc);
        self.collect_while(|c| c.is_numeric(), &mut nums);
        let int_val = nums.parse().expect("numerical string failed to parse");
        let Some('.') = self.next_if(|c| c == '.') else {
            return Ok(Token {
                span: self.span_to_here(start_idx),
                value: TokenKind::Integer(int_val),
            });
        };
        nums.push('.');
        let l = nums.len();
        self.collect_while(|c| c.is_numeric(), &mut nums);
        //we didn't get any more characters so we will let the number and dot be lexed separately
        if nums.len() == l {
            self.cursor -= 1;
            return Ok(Token {
                span: self.span_to_here(start_idx),
                value: TokenKind::Integer(int_val),
            });
        }
        Ok(Token {
            span: self.span_to_here(start_idx),
            value: TokenKind::Float(
                nums.parse()
                    .expect("numerical string failed to parse as int"),
            ),
        })
    }

    fn char_(&mut self, _start_quote: char, start_idx: usize) -> Result<Token, LexError> {
        let c = match self.must_next_char(start_idx, LexError::EOFChar)? {
            '\\' => escape_char(self.must_next_char(start_idx, LexError::EOFChar)?),
            c => c,
        };
        let '\'' = self.must_next_char(start_idx, LexError::EOFChar)? else {
            return Err(LexError::UnexpectedChar(
                self.span_to_here(self.cursor - 1),
                '\'',
            ));
        };
        Ok(Token {
            span: self.span_to_here(start_idx),
            value: TokenKind::Char(c),
        })
    }

    fn string(&mut self, _start_quote: char, start_idx: usize) -> Result<Token, LexError> {
        let mut chars = String::new();
        let mut prev_slash = false;
        self.collect_while(
            |c| {
                let rtn = c != '"' || prev_slash;
                prev_slash = c == '\\';
                rtn
            },
            &mut chars,
        );
        if let Some('"') = self.next_if(|c| c == '"') {
            let span = self.span_to_here(start_idx);
            Ok(Token {
                span,
                value: TokenKind::Str(escape(&chars)),
            })
        } else {
            let span = self.span_to_here(start_idx);
            Err(LexError::EOFString(span))
        }
    }

    fn identifier_or_keyword(
        &mut self,
        start_char: char,
        start_idx: usize,
    ) -> Result<Token, LexError> {
        let mut id = String::new();
        id.push(start_char);
        self.collect_while(|c| c == '_' || c.is_alphabetic() || c.is_numeric(), &mut id);

        Ok(Token {
            span: self.span_to_here(start_idx),
            value: match id.as_str() {
                "true" => TokenKind::Bool(true),
                "false" => TokenKind::Bool(false),
                "for" => TokenKind::For,
                "in" => TokenKind::In,
                "if" => TokenKind::If,
                "while" => TokenKind::While,
                "fn" => TokenKind::Function,
                "let" => TokenKind::Let,
                "var" => TokenKind::Var,
                _ => TokenKind::Identifier(id),
            },
        })
    }
}

#[cfg(test)]
mod tests {
    macro_rules! test_lex {
        ($lexer:expr => $expt:pat) => {
            assert!(matches!($lexer.next().unwrap().unwrap().value, $expt))
        };
    }
    use super::*;

    #[test]
    fn sequence() {
        let test = "\"this is as\" 1 \"ssfsd\"\"lsdkfjsd\" 1 12 12.12 12. 1 true false";
        let mut l = Lexer::new("test".to_owned(), test.chars().collect());

        test_lex!(l => TokenKind::Str(_));
        test_lex!(l => TokenKind::Integer(1));
        test_lex!(l => TokenKind::Str(_));
        test_lex!(l => TokenKind::Str(_));
        test_lex!(l => TokenKind::Integer(1));
        test_lex!(l => TokenKind::Integer(12));
        test_lex!(l => TokenKind::Float(_));
        test_lex!(l => TokenKind::Integer(12));
        test_lex!(l => TokenKind::Dot);
        test_lex!(l => TokenKind::Integer(1));
        test_lex!(l => TokenKind::Bool(true));
        test_lex!(l => TokenKind::Bool(false));
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

        let l = Lexer::new("operators".to_owned(), ops.chars().collect());

        for (kind, res_token) in operators.iter().zip(l) {
            assert_eq!(res_token.unwrap().value, *kind);
        }
    }

    #[test]
    fn string() {
        let test = "\"test\\n\"";
        let mut l = Lexer::new("test".to_owned(), test.chars().collect());

        let TokenKind::Str(s) = l.next().unwrap().unwrap().value else {
            panic!("Lexer did not yield String token");
        };

        assert_eq!(s, "test\n");
    }

    #[test]
    fn float() {
        let test = "1.2";
        let mut l = Lexer::new("test".to_owned(), test.chars().collect());

        let TokenKind::Float(s) = l.next().unwrap().unwrap().value else {
            panic!("Lexer did not yield Float token");
        };

        assert_eq!(s, 1.2);
    }

    #[test]
    fn escape() {
        let test = "\\n\\t\\r\\0";
        let output = super::escape(test);

        assert_eq!(output, "\n\t\r\0");
    }

    #[test]
    fn locations() {
        let test = "\"this is as\" 1 \n\"ssfsd\"\n\"lsdkfjsd\" 1 12 12.12 12. 1";
        let mut l = Lexer::new("test".to_owned(), test.chars().collect());

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
        assert_eq!(span!(l).to_string(), "test:3,23-3,24");
        assert_eq!(span!(l).to_string(), "test:3,25");
        assert_eq!(span!(l).to_string(), "test:3,27");
        assert!(l.next().is_none())
    }

    #[test]
    fn correct_put_back() {
        let test = "(1,2)+(3,4)";
        let mut l = Lexer::new("test".to_owned(), test.chars().collect());

        test_lex!(l => TokenKind::OpenParen);
        test_lex!(l => TokenKind::Integer(1));
        test_lex!(l => TokenKind::Comma);
        test_lex!(l => TokenKind::Integer(2));
        test_lex!(l => TokenKind::CloseParen);
        test_lex!(l => TokenKind::Plus);
        test_lex!(l => TokenKind::OpenParen);
        test_lex!(l => TokenKind::Integer(3));
        test_lex!(l => TokenKind::Comma);
        test_lex!(l => TokenKind::Integer(4));
        test_lex!(l => TokenKind::CloseParen);
        assert!(l.next().is_none())
    }

    #[test]
    fn cpb() {
        let test = "(test,";
        let mut l = Lexer::new("test".to_owned(), test.chars().collect());

        test_lex!(l => TokenKind::OpenParen);
        test_lex!(l => TokenKind::Identifier(_));
        test_lex!(l => TokenKind::Comma);
        assert!(l.next().is_none())
    }
}
