use crate::{
    expression::{
        AssignmentKind, BinOpKind, FlatBinOpKind, LValueKind, RValueKind, ShortCircuitBinOpKind,
        UntypedExpression, UntypedExpressionKind, UntypedLValue,
    },
    lexer::LexError,
    span::Span,
    token::{Token, TokenKind},
};
use itertools::{put_back, structs::PutBack};
use std::rc::Rc;
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

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for e in self.exprs.iter() {
            writeln!(f, "{e}")?;
        }
        Ok(())
    }
}

enum LambdaLevel {
    None,
    Arg,
    Full,
}

struct Parser<I: Iterator<Item = Result<Token, LexError>>> {
    lexer: PutBack<I>,
    in_lambda: LambdaLevel,
}

pub fn parse(lexer: impl Iterator<Item = Result<Token, LexError>>) -> Result<Ast, ParseError> {
    let mut tree = Ast { exprs: Vec::new() };
    let mut parser = Parser {
        lexer: put_back(lexer),
        in_lambda: LambdaLevel::None,
    };
    while let Some(e) = parser.parse_expr() {
        let e = e?;
        tree.exprs.push(e);
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

    fn must_next_token(&mut self, context: &'static str) -> Result<Token, ParseError> {
        let Some(t) = self.lexer.next() else {
            return Err(ParseError::UnexpectedEOF(context));
        };
        Ok(t?)
    }

    fn expr(&mut self, token: Token) -> ParseResult {
        match token.value {
            TokenKind::For => self.for_(token),
            TokenKind::If => self.if_(token),
            _ => self.equals(token),
        }
    }

    fn lval(&mut self, token: Token) -> Result<UntypedLValue, ParseError> {
        match self.expr(token)?.to_lval() {
            Ok(l) => Ok(l),
            Err(e) => Err(ParseError::NotAnLValue(e)),
        }
    }

    fn equals(&mut self, token: Token) -> ParseResult {
        let left = self.comma(token)?;
        let Some(token) = self.lexer.next() else {
            return Ok(left);
        };
        let token = token?;
        let op = match &token.value {
            TokenKind::SingleEquals => AssignmentKind::Equals,
            _ => {
                self.lexer.put_back(Ok(token));
                return Ok(left);
            }
        };
        let left = match left.to_lval() {
            Ok(l) => l,
            Err(left) => return Err(ParseError::NotAnLValue(left)),
        };
        let Some(token) = self.lexer.next() else {
            return Err(ParseError::UnexpectedEOF("expression"));
        };
        let right = Box::new(self.expr(token?)?);
        let rtn = UntypedExpression {
            span: left.span.to(&right.span),
            value: UntypedExpressionKind::RValue(RValueKind::Assignment { left, op, right }),
        };
        Ok(rtn)
    }

    binops!(
        comma #flatten {
            TokenKind::Comma => FlatBinOpKind::MakeTuple,
        } =>

        pipe #short_circuit {
            TokenKind::PipeArrow => ShortCircuitBinOpKind::Pipe,
            TokenKind::SkinnyArrow => ShortCircuitBinOpKind::InvertedCall,
        } => lambda
    );

    fn lambda(&mut self, token: Token) -> ParseResult {
        let expr = self.boolean_or(token)?;
        Ok(if let LambdaLevel::Arg = self.in_lambda {
            self.in_lambda = LambdaLevel::None;
            UntypedExpression {
                span: expr.span.clone(),
                value: UntypedExpressionKind::RValue(RValueKind::Lambda(Rc::new(expr))),
            }
        } else {
            expr
        })
    }

    binops!(
        boolean_or #short_circuit { TokenKind::DoublePipe => ShortCircuitBinOpKind::BoolOr, } =>
        boolean_xor { TokenKind::DoubleHat => BinOpKind::BoolXor, } =>
        boolean_and #short_circuit { TokenKind::DoubleAmpersand => ShortCircuitBinOpKind::BoolAnd, } =>

        bitwise_or  { TokenKind::Pipe => BinOpKind::BitwiseOr } =>
        bitwise_xor { TokenKind::Hat => BinOpKind::BitwiseXor } =>
        bitwise_and  { TokenKind::Ampersand => BinOpKind::BitwiseAnd } =>

        comparision #flatten {
            TokenKind::GreaterThanOrEqual => FlatBinOpKind::GreaterThanOrEqual,
            TokenKind::GreaterThan => FlatBinOpKind::GreaterThan,
            TokenKind::LessThanOrEqual => FlatBinOpKind::LessThanOrEqual, TokenKind::LessThan => FlatBinOpKind::LessThan,
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
        } => call
    );

    fn call(&mut self, token: Token) -> ParseResult {
        let mut callable = self.literal_id_or_recurse(token)?;
        while let Some(token) = self.lexer.next() {
            let token = token?;
            callable = match &token.value {
                TokenKind::OpenParen => {
                    let args = Box::new(self.paren(token)?);

                    UntypedExpression {
                        span: callable.span.to(&args.span),
                        value: UntypedExpressionKind::RValue(RValueKind::Call {
                            callable: Box::new(callable),
                            args,
                        }),
                    }
                }
                TokenKind::OpenBracket => {
                    let subscript = Box::new(self.bracket(token)?);

                    UntypedExpression {
                        span: callable.span.to(&subscript.span),
                        value: UntypedExpressionKind::LValue(LValueKind::BracketExpr {
                            left: Box::new(callable),
                            subscript,
                        }),
                    }
                }
                _ => {
                    self.lexer.put_back(Ok(token));
                    return Ok(callable);
                }
            }
        }
        Ok(callable)
    }

    fn literal_id_or_recurse(&mut self, token: Token) -> ParseResult {
        Ok(match token.value {
            TokenKind::Integer(i) => self.tag_rval(RValueKind::Integer(i), token.span),
            TokenKind::Float(f) => self.tag_rval(RValueKind::Float(f), token.span),
            TokenKind::Str(s) => self.tag_rval(RValueKind::Str(s), token.span),
            TokenKind::Char(c) => self.tag_rval(RValueKind::Char(c), token.span),
            TokenKind::Bool(b) => self.tag_rval(RValueKind::Bool(b), token.span),
            TokenKind::Identifier(i) => self.tag_lval(LValueKind::Variable(i), token.span),
            TokenKind::LambdaArg(i) => {
                if let LambdaLevel::None = self.in_lambda {
                    self.in_lambda = LambdaLevel::Arg;
                }
                self.tag_rval(RValueKind::LambdaArg(i), token.span)
            }
            TokenKind::BackSlash => {
                let token = self.must_next_token("lambda expression")?;
                let save = std::mem::replace(&mut self.in_lambda, LambdaLevel::Full);
                let expr = self.lambda(token)?;
                self.in_lambda = LambdaLevel::None;
                self.in_lambda = save;
                UntypedExpression {
                    span: expr.span.clone(),
                    value: UntypedExpressionKind::RValue(RValueKind::Lambda(Rc::new(expr))),
                }
            }
            TokenKind::OpenParen => self.paren(token)?,
            TokenKind::OpenBrace => self.block(token)?,
            a => todo!("{}:{a:?}", token.span),
        })
    }

    fn paren(&mut self, open_paren_token: Token) -> ParseResult {
        let token = self.must_next_token("expression")?;
        let expr = self.expr(token)?;
        let end_paren_token = self.must_next_token("`)`")?;
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

    fn bracket(&mut self, open_bracket_token: Token) -> ParseResult {
        let token = self.must_next_token("expression")?;
        let expr = self.expr(token)?;
        let end_paren_token = self.must_next_token("`]`")?;
        let Token {
            span,
            value: TokenKind::CloseBracket,
        } = end_paren_token
        else {
            println!("{expr}");
            return Err(ParseError::UnexpectedToken {
                expected: vec![TokenKind::CloseBracket],
                found: end_paren_token,
            });
        };
        Ok(UntypedExpression {
            span: open_bracket_token.span.to(&span),
            value: UntypedExpressionKind::RValue(RValueKind::BracketExpr(Box::new(expr))),
        })
    }

    fn block(&mut self, open_brace_token: Token) -> ParseResult {
        let mut exprs = Vec::new();
        loop {
            let Some(token) = self.lexer.next() else {
                return Err(ParseError::UnexpectedEOF("expression expression in block"));
            };
            let token = token?;
            if let TokenKind::CloseBrace = token.value {
                return Ok(UntypedExpression {
                    span: open_brace_token.span.to(&token.span),
                    value: UntypedExpressionKind::RValue(RValueKind::Block(exprs)),
                });
            }
            exprs.push(self.expr(token)?);
        }
    }

    fn for_(&mut self, for_token: Token) -> ParseResult {
        let token = self.must_next_token("lvalue in for")?;
        let item = self.lval(token)?;
        let in_token = self.must_next_token("`in`")?;
        let TokenKind::In = in_token.value else {
            return Err(ParseError::UnexpectedToken {
                expected: vec![TokenKind::In],
                found: in_token,
            });
        };
        let token = self.must_next_token("expresion items of for loop")?;
        let items = Box::new(self.expr(token)?);
        let token = self.must_next_token("expresion body of for loop")?;
        let body = Box::new(self.expr(token)?);
        Ok(UntypedExpression {
            span: for_token.span.to(&body.span),
            value: UntypedExpressionKind::RValue(RValueKind::For { item, items, body }),
        })
    }

    fn if_(&mut self, if_token: Token) -> ParseResult {
        let token = self.must_next_token("condition in if")?;
        let condition = Box::new(self.expr(token)?);
        let token = self.must_next_token("body in if")?;
        let body = Box::new(self.expr(token)?);
        Ok(UntypedExpression {
            span: if_token.span.to(&body.span),
            value: UntypedExpressionKind::RValue(RValueKind::If { condition, body }),
        })
    }
}
