use crate::{
    expression::{UntypedExpression, UntypedExpressionKind},
    lexer::LexError,
    span::Span,
    token::{Token, TokenKind},
};
use std::iter::Peekable;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    LexError(#[from] LexError),
}

#[derive(Clone, Debug)]
pub struct ParseTree {
    exprs: Vec<UntypedExpression>,
}

struct Parser<I: Iterator<Item = Result<Token, LexError>>> {
    lexer: Peekable<I>,
}

pub fn parse(
    lexer: impl Iterator<Item = Result<Token, LexError>>,
) -> Result<ParseTree, ParseError> {
    let mut tree = ParseTree { exprs: Vec::new() };
    let mut parser = Parser {
        lexer: lexer.peekable(),
    };
    while let Some(e) = parser.parse_expr() {
        tree.exprs.push(e?);
    }
    Ok(tree)
}

type ParseResult = Result<UntypedExpression, ParseError>;

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
        self.literal(token)
    }

    fn literal(&mut self, token: Token) -> ParseResult {
        use UntypedExpressionKind::*;
        Ok(match token.value {
            TokenKind::Integer(i) => self.tag(Integer(i), token.span),
            TokenKind::Float(f) => self.tag(Float(f), token.span),
            TokenKind::String(s) => self.tag(String(s), token.span),
            _ => todo!(),
        })
    }
}
