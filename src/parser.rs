use crate::{
    expression::{
        AssignmentKind, BinOpKind, FlatBinOpKind, LValueKind, RValueKind, ShortCircuitBinOpKind,
        UntypedExpression, UntypedExpressionKind,
    },
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
    #[error("{}: `{}` cannot be assigned to", .0.span, .0.value)]
    NotAnLValue(UntypedExpression),
}

#[derive(Debug)]
pub struct Ast {
    pub exprs: Vec<UntypedExpression>,
}

struct Parser<I: Iterator<Item = Result<Token, LexError>>> {
    lexer: PutBack<I>,
}

pub fn parse(lexer: impl Iterator<Item = Result<Token, LexError>>) -> Result<Ast, ParseError> {
    let mut tree = Ast { exprs: Vec::new() };
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
    ($name:ident $return:ident { $($tok:pat_param => $result:expr),+ $(,)? } => $next:ident $($rest:tt)*) => {
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
                    value: UntypedExpressionKind::RValue(RValueKind::$return {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    })
                };
            }
        }
        binops!($next $($rest)*);
    };
    ($name:ident { $($tok:pat_param => $result:expr),+ $(,)? } => $next:ident $($rest:tt)*) => {
        binops!($name BinOp { $($tok => $result),+ } => $next $($rest)*);
    };
    ($name:ident #short_circuit { $($tok:pat_param => $result:expr),+ $(,)? } => $next:ident $($rest:tt)*) => {
        binops!($name ShortCircuitBinOp { $($tok => $result),+ } => $next $($rest)*);
    };
    ($name:ident #flatten { $($tok:pat_param => $result:expr),+ $(,)? } => $next:ident $($rest:tt)*) => {
        fn $name(&mut self, token: Token) -> ParseResult {
            let first = self.$next(token)?;
            let mut rest = Vec::new();
            loop {
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
                let Some(token) = self.lexer.next() else {
                    return Err(ParseError::UnexpectedEOF("expression"));
                };
                let expr = self.$next(token?)?;
                rest.push((op, Box::new(expr)));
            }
            Ok(if rest.is_empty() {
                first
            } else {
                UntypedExpression {
                    span : first.span.to(&rest.last().unwrap().1.span),
                    value: UntypedExpressionKind::RValue(RValueKind::FlatBinOp {
                        first: Box::new(first),
                        rest,
                    })
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

    fn tag_rval(&mut self, value: RValueKind, span: Span) -> UntypedExpression {
        UntypedExpression {
            span,
            value: UntypedExpressionKind::RValue(value),
        }
    }

    fn tag_lval(&mut self, value: LValueKind, span: Span) -> UntypedExpression {
        UntypedExpression {
            span,
            value: UntypedExpressionKind::LValue(value),
        }
    }

    fn expr(&mut self, token: Token) -> ParseResult {
        self.equals(token)
    }

    fn equals(&mut self, token: Token) -> ParseResult {
        let left = self.comma(token)?;
        let Some(token) = self.lexer.next() else {
            return Ok(left);
        };
        let op = match token?.value {
            TokenKind::SingleEquals => AssignmentKind::Equals,
            _ => return Ok(left),
        };
        let left = match left.to_lvalue() {
            Ok(l) => l,
            Err(left) => return Err(ParseError::NotAnLValue(left)),
        };
        let Some(token) = self.lexer.next() else {
            return Err(ParseError::UnexpectedEOF("expression"));
        };
        let right = Box::new(self.expr(token?)?);
        Ok(UntypedExpression {
            span: left.span.to(&right.span),
            value: UntypedExpressionKind::RValue(RValueKind::Assignment { left, op, right }),
        })
    }

    binops!(

        comma #flatten {
            TokenKind::Comma => FlatBinOpKind::MakeTuple,
        } =>

        pipe {
            TokenKind::PipeArrow => BinOpKind::Pipe,
            TokenKind::SkinnyArrow => BinOpKind::InvertedCall,
        } =>

        boolean_or #short_circuit { TokenKind::DoublePipe => ShortCircuitBinOpKind::BoolOr, } =>
        boolean_xor { TokenKind::DoubleHat => BinOpKind::BoolXor, } =>
        boolean_and #short_circuit { TokenKind::DoubleAmpersand => ShortCircuitBinOpKind::BoolAnd, } =>

        bitwise_or  { TokenKind::Pipe => BinOpKind::BitwiseOr } =>
        bitwise_xor { TokenKind::Hat => BinOpKind::BitwiseXor } =>
        bitwise_and  { TokenKind::Ampersand => BinOpKind::BitwiseAnd } =>

        comparision #flatten {
            TokenKind::GreaterThanOrEqual => FlatBinOpKind::GreaterThanOrEqual,
            TokenKind::GreaterThan => FlatBinOpKind::GreaterThan,
            TokenKind::LessThanOrEqual => FlatBinOpKind::LessThanOrEqual,
            TokenKind::LessThan => FlatBinOpKind::LessThan,
            TokenKind::DoubleEquals => FlatBinOpKind::CmpEquals,
            TokenKind::NotEqual => FlatBinOpKind::CmpNotEquals,
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
        } => literal_id_or_recurse
    );

    fn literal_id_or_recurse(&mut self, token: Token) -> ParseResult {
        Ok(match token.value {
            TokenKind::Integer(i) => self.tag_rval(RValueKind::Integer(i), token.span),
            TokenKind::Float(f) => self.tag_rval(RValueKind::Float(f), token.span),
            TokenKind::Str(s) => self.tag_rval(RValueKind::Str(s), token.span),
            TokenKind::Bool(b) => self.tag_rval(RValueKind::Bool(b), token.span),
            TokenKind::Identifier(i) => self.tag_lval(LValueKind::Identifier(i), token.span),
            TokenKind::OpenParen => self.paren(token)?,
            a => todo!("{}:{a:?}", token.span),
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
            value: UntypedExpressionKind::RValue(RValueKind::ParenExpr(Box::new(expr))),
        })
    }
}
