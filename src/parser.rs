use crate::{
    expression::{BinOpKind, UntypedExpression, UntypedExpressionKind},
    lexer::LexError,
    span::Span,
    token::{Token, TokenKind},
};
use itertools::{put_back, structs::PutBack};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    LexError(#[from] LexError),
    #[error("Expected `{0}` found EOF")]
    UnexpectedEOF(&'static str),
    #[error("{}: Expected one of {expected:?} found `{}`", found.span, found.value)]
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Token,
    },
}

#[derive(Clone, Debug)]
pub struct ParseTree {
    pub exprs: Vec<UntypedExpression>,
}

struct Parser<I: Iterator<Item = Result<Token, LexError>>> {
    lexer: PutBack<I>,
}

pub fn parse(
    lexer: impl Iterator<Item = Result<Token, LexError>>,
) -> Result<ParseTree, ParseError> {
    let mut tree = ParseTree { exprs: Vec::new() };
    let mut parser = Parser {
        lexer: put_back(lexer),
    };
    while let Some(e) = parser.parse_expr() {
        tree.exprs.push(e?);
    }
    Ok(tree)
}

type ParseResult = Result<UntypedExpression, ParseError>;

macro_rules! binops {
    ($name:ident) => {};
    ($name:ident { $($tok:pat_param => $result:expr),+ $(,)? } => $next:ident $($rest:tt)*) => {
        fn $name(&mut self, token: Token) -> ParseResult {
            let mut left = self.$next(token)?;
            loop {
                let Some(sep) = self.lexer.next() else {
                    return Ok(left);
                };
                let sep = sep?;
                let op = match &sep.value {
                    $($tok => $result),+,
                    _ => {
                        self.lexer.put_back(Ok(sep));
                        return Ok(left);
                    }
                };
                let Some(tok2) = self.lexer.next() else {
                    return Err(ParseError::UnexpectedEOF("expression"));
                };

                let right = self.$next(tok2?)?;
                left = UntypedExpression {
                    span: left.span.to(&right.span),
                    value: UntypedExpressionKind::BinOp {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    }
                };
            }
        }
        binops!($next $($rest)*);
    };
    ($name:ident #flatten { $($tok:pat_param => $result:expr),+ $(,)? } => $next:ident $($rest:tt)*) => {
        fn $name(&mut self, mut token: Token) -> ParseResult {
            let mut bulk = Vec::new();
            let mut last_expr;
            loop {
                last_expr = self.$next(token)?;
                let Some(sep) = self.lexer.next() else {
                    break;
                };
                let sep = sep?;
                let op = match &sep.value {
                    $($tok => $result),+,
                    _ => {
                        self.lexer.put_back(Ok(sep));
                        break;
                    }
                };
                bulk.push((Box::new(last_expr), op));
                let Some(tok2) = self.lexer.next() else {
                    return Err(ParseError::UnexpectedEOF("expression"));
                };
                token = tok2?;
            }
            Ok(if bulk.is_empty() {
                last_expr
            } else {
                UntypedExpression {
                    span : bulk[0].0.span.to(&last_expr.span),
                    value: UntypedExpressionKind::FlatBinOp {
                        bulk,
                        last: Box::new(last_expr),
                    }
                }
            })
        }
        binops!($next $($rest)*);
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    fn parse_expr(&mut self) -> Option<ParseResult> {
        match self.lexer.next()? {
            Err(e) => Some(Err(ParseError::LexError(e.clone()))),
            Ok(t) => Some(self.expr(t)),
        }
    }
    fn tag(&mut self, value: UntypedExpressionKind, span: Span) -> UntypedExpression {
        UntypedExpression { span, value }
    }

    fn expr(&mut self, token: Token) -> ParseResult {
        self.comma(token)
    }

    binops!(
        comma #flatten {
            TokenKind::Comma => BinOpKind::MakeTuple,
        } =>

        pipe {
            TokenKind::PipeArrow => BinOpKind::Pipe,
            TokenKind::SkinnyArrow => BinOpKind::InvertedCall,
        } =>

        boolean_op {
            TokenKind::DoublePipe => BinOpKind::BoolOr,
            TokenKind::DoubleAmpersand => BinOpKind::BoolAnd,
            TokenKind::DoubleHat => BinOpKind::BoolXor,
        } =>

        bitwise_or { TokenKind::Pipe => BinOpKind::BitwiseOr } =>
        bitwise_xor { TokenKind::Hat => BinOpKind::BitwiseXor } =>
        bitwise_and { TokenKind::Ampersand => BinOpKind::BitwiseAnd } =>

        comparision #flatten {
            TokenKind::GreaterThanOrEqual => BinOpKind::GreaterThanOrEqual,
            TokenKind::GreaterThan => BinOpKind::GreaterThan,
            TokenKind::LessThanOrEqual => BinOpKind::LessThanOrEqual,
            TokenKind::LessThan => BinOpKind::LessThan,
            TokenKind::DoubleEquals => BinOpKind::CmpEquals,
            TokenKind::NotEqual => BinOpKind::CmpNotEquals,
        } =>

        bitshift {
            TokenKind::BitShiftLeft => BinOpKind::BitShiftLeft,
            TokenKind::BitShiftRight => BinOpKind::BitShiftRight,
        } =>

        additive {
            TokenKind::Minus => BinOpKind::Subtract,
            TokenKind::Plus => BinOpKind::Add,
        } =>

        multiplicative {
            TokenKind::Asterik => BinOpKind::Multiply,
            TokenKind::ForwardSlash => BinOpKind::Divide,
            TokenKind::DoubleForwardSlash => BinOpKind::IntegerDivide,
            TokenKind::Mod => BinOpKind::Mod,
        } => literal
    );

    fn literal(&mut self, token: Token) -> ParseResult {
        use UntypedExpressionKind::*;
        Ok(match token.value {
            TokenKind::Integer(i) => self.tag(Integer(i), token.span),
            TokenKind::Float(f) => self.tag(Float(f), token.span),
            TokenKind::Str(s) => self.tag(Str(s), token.span),
            TokenKind::OpenParen => self.paren(token)?,
            a => todo!("{a:?}"),
        })
    }

    fn paren(&mut self, open_paren_token: Token) -> ParseResult {
        let Some(token) = self.lexer.next() else {
            return Err(ParseError::UnexpectedEOF("expression"));
        };
        let expr = self.expr(token?)?;

        let Some(end_paren_token) = self.lexer.next() else {
            return Err(ParseError::UnexpectedEOF("`)`"));
        };
        let end_paren_token = end_paren_token?;
        let Token {
            span,
            value: TokenKind::CloseParen,
        } = end_paren_token
        else {
            return Err(ParseError::UnexpectedToken {
                expected: vec![TokenKind::CloseParen],
                found: end_paren_token,
            });
        };
        Ok(UntypedExpression {
            span: open_paren_token.span.to(&span),
            value: UntypedExpressionKind::ParenExpr(Box::new(expr)),
        })
    }
}
