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
    #[error("Unexpected EOF. Expected {0}")]
    UnexpectedEOF(&'static str),
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
            let left = self.$next(token)?;
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
            Ok(UntypedExpression {
                span: left.span.to(&right.span),
                value: UntypedExpressionKind::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                }
            })
        }
        binops!($next $($rest)*);
    };
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
        self.boolean_op(token)
    }

    binops!(
        boolean_op {
            TokenKind::DoublePipe => BinOpKind::BoolOr,
            TokenKind::DoubleAmpersand => BinOpKind::BoolAnd,
            TokenKind::DoubleHat => BinOpKind::BoolXor,
        } =>

        bitwise_or { TokenKind::Pipe => BinOpKind::BitwiseOr } =>
        bitwise_xor { TokenKind::Hat => BinOpKind::BitwiseXor } =>
        bitwise_and { TokenKind::Ampersand => BinOpKind::BitwiseAnd } =>

        comparision {
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
            _ => todo!(),
        })
    }
}
