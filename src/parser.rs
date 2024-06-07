#![allow(clippy::needless_pass_by_value)]
// TODO: go clean up the UntypedExpression with a new method and get ris of the useless typespec lines
use crate::{
    expression::{
        BinOpKind, DeclarationKind, FlatBinOpKind, Pattern, ShortCircuitBinOpKind, UntypedAst,
        UntypedExpr, UntypedExprKind, UntypedLValue, UntypedLValueKind, UntypedRValueKind,
    },
    lexer::LexError,
    span::Span,
    token::{Kind as TokenKind, Token},
};
use itertools::{put_back, structs::PutBack};
use std::rc::Rc;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[allow(clippy::enum_variant_names)]
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
    NotAnLValue(UntypedExpr),
}

struct Parser<I: Iterator<Item = Result<Token, LexError>>> {
    lexer: PutBack<I>,
    in_lambda: bool,
}

pub fn parse(lexer: impl Iterator<Item = Result<Token, LexError>>) -> Result<UntypedAst, Error> {
    let mut tree = UntypedAst { exprs: Vec::new() };
    let mut parser = Parser {
        lexer: put_back(lexer),
        in_lambda: false,
    };
    while let Some(e) = parser.parse_expr() {
        let e = e?;
        tree.exprs.push(e);
    }
    Ok(tree)
}

type ParseResult = Result<UntypedExpr, Error>;

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
                    return Err(Error::UnexpectedEOF("expression"));
                };

                let right = self.$next(tok2?)?;
                left = UntypedExpr {
                    span: left.span.to(&right.span),
                    value: UntypedExprKind::RValue(UntypedRValueKind::$return {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    })
                    , typespec: (),
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
            while let Some(sep) = self.lexer.next() {
                let sep = sep?;
                let op = match &sep.value {
                    $($tok => $result),+,
                    _ => {
                        self.lexer.put_back(Ok(sep));
                        break;
                    }
                };
                let Some(token) = self.lexer.next() else {
                    return Err(Error::UnexpectedEOF("expression"));
                };
                let expr = self.$next(token?)?;
                rest.push((op, Box::new(expr)));
            }
            Ok(if rest.is_empty() {
                first
            } else {
                UntypedExpr {
                    span : first.span.to(&rest.last().unwrap().1.span),
                    value: UntypedExprKind::RValue(UntypedRValueKind::FlatBinOp {
                        first: Box::new(first),
                        rest,
                    })
                    , typespec: (),
                }
            })
        }
        binops!($next $($rest)*);
    }
}

macro_rules! expect {
    ($self:ident => $token:ident { $( $tree:tt )* }) => {{
        let Some($token) = $self.lexer.next() else {
            todo!()
        };
        let $token = $token?;
        match $token.value {
            $($tree)*
                v => todo!("{v}"),
            }
    }};
}

impl<I> Parser<I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    fn parse_expr(&mut self) -> Option<ParseResult> {
        match self.lexer.next()? {
            Err(e) => Some(Err(Error::LexError(e))),
            Ok(t) => match t.value {
                TokenKind::CloseParen | TokenKind::CloseBrace | TokenKind::CloseBracket => {
                    self.lexer.put_back(Ok(t));
                    None
                }
                _ => Some(self.expr(t)),
            },
        }
    }

    const fn tag_rval(value: UntypedRValueKind, span: Span) -> UntypedExpr {
        UntypedExpr {
            span,
            value: UntypedExprKind::RValue(value),
            typespec: (),
        }
    }

    const fn tag_lval(value: UntypedLValueKind, span: Span) -> UntypedExpr {
        UntypedExpr {
            span,
            value: UntypedExprKind::LValue(value),
            typespec: (),
        }
    }

    fn must_next_token(&mut self, context: &'static str) -> Result<Token, Error> {
        let Some(t) = self.lexer.next() else {
            return Err(Error::UnexpectedEOF(context));
        };
        Ok(t?)
    }

    fn must<T>(
        &mut self,
        c: fn(&mut Self, Token) -> Result<T, Error>,
        context: &'static str,
    ) -> Result<T, Error> {
        let token = self.must_next_token(context)?;
        c(self, token)
    }

    fn expr(&mut self, token: Token) -> ParseResult {
        match token.value {
            TokenKind::Let | TokenKind::Var => self.declaration(token),
            _ => self.equals(token),
        }
    }

    fn lval(&mut self, token: Token) -> Result<UntypedLValue, Error> {
        match self.expr(token)?.into_lval() {
            Ok(l) => Ok(l),
            Err(e) => Err(Error::NotAnLValue(e)),
        }
    }

    fn equals(&mut self, token: Token) -> ParseResult {
        let left = self.comma(token)?;
        let Some(token) = self.lexer.next() else {
            return Ok(left);
        };
        let token = token?;
        let op = match &token.value {
            TokenKind::SingleEquals => {
                let left = match left.into_lval() {
                    Ok(l) => l,
                    Err(left) => return Err(Error::NotAnLValue(left)),
                };
                let Some(token) = self.lexer.next() else {
                    return Err(Error::UnexpectedEOF("expression"));
                };
                let right = Box::new(self.expr(token?)?);
                let rtn = UntypedExpr {
                    span: left.span.to(&right.span),
                    value: UntypedExprKind::RValue(UntypedRValueKind::Assignment { left, right }),
                    typespec: (),
                };
                return Ok(rtn);
            }
            TokenKind::PlusEquals => BinOpKind::Add,
            TokenKind::MinusEquals => BinOpKind::Subtract,
            TokenKind::BitShiftLeftEquals => BinOpKind::BitShiftLeft,
            TokenKind::BitShiftRightEquals => BinOpKind::BitShiftRight,
            TokenKind::ForwardSlashEquals => BinOpKind::Divide,
            TokenKind::DoubleForwardSlash => BinOpKind::IntegerDivide,
            TokenKind::Mod => BinOpKind::Mod,
            _ => {
                self.lexer.put_back(Ok(token));
                return Ok(left);
            }
        };
        let r = Box::new(left.clone());
        let left = match left.into_lval() {
            Ok(l) => l,
            Err(left) => return Err(Error::NotAnLValue(left)),
        };
        let Some(token) = self.lexer.next() else {
            return Err(Error::UnexpectedEOF("expression"));
        };
        let right = Box::new(self.expr(token?)?);
        let span = left.span.to(&right.span);
        let rtn = UntypedExpr {
            span: span.clone(),
            value: UntypedExprKind::RValue(UntypedRValueKind::Assignment {
                left,
                right: Box::new(UntypedExpr {
                    span,
                    value: UntypedExprKind::RValue(UntypedRValueKind::BinOp { left: r, op, right }),
                    typespec: (),
                }),
            }),
            typespec: (),
        };
        Ok(rtn)
    }

    fn lambda(&mut self, token: Token) -> ParseResult {
        let save = std::mem::replace(&mut self.in_lambda, true);
        let expr = self.boolean_or(token)?;
        self.in_lambda = save;
        Ok(UntypedExpr {
            span: expr.span.clone(),
            value: UntypedExprKind::RValue(UntypedRValueKind::Lambda(Rc::new(expr))),
            typespec: (),
        })
    }

    fn comma(&mut self, token: Token) -> ParseResult {
        let mut expr = self.pipe(token)?;
        let mut exprs = Vec::new();
        let mut force_tuple = false;
        while let Some(sep) = self.lexer.next() {
            let sep = sep?;
            let TokenKind::Comma = &sep.value else {
                self.lexer.put_back(Ok(sep));
                break;
            };
            // allow single trailing comma
            let Some(token) = self.lexer.next() else {
                break;
            };
            force_tuple = true;
            exprs.push(expr);
            expr = self.pipe(token?)?;
        }
        if exprs.is_empty() && !force_tuple {
            return Ok(expr);
        }

        let span = if let Some(e) = exprs.first() {
            e.span.to(&expr.span)
        } else {
            expr.span.clone()
        };
        exprs.push(expr);

        Ok(UntypedExpr {
            span,
            value: UntypedExprKind::LValue(UntypedLValueKind::Tuple(exprs)),
            typespec: (),
        })
    }

    fn binop(&mut self, token: Token) -> ParseResult {
        self.pipe(token)
    }

    binops!(
        pipe #short_circuit {
            TokenKind::PipeArrow => ShortCircuitBinOpKind::Pipe,
            TokenKind::SkinnyArrow => ShortCircuitBinOpKind::InvertedCall,
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
    } => prefix
    );

    fn prefix(&mut self, token: Token) -> ParseResult {
        match token.value {
            TokenKind::Asterik => self.must(Self::postfix, "expression"),
            TokenKind::Increment => {
                let item = self.must(Self::lval, "expression")?;
                Ok(UntypedExpr {
                    span: item.span.to(&token.span),
                    value: UntypedExprKind::RValue(UntypedRValueKind::PreIncr(item)),
                    typespec: (),
                })
            }
            TokenKind::Decrement => {
                let item = self.must(Self::lval, "expression")?;
                Ok(UntypedExpr {
                    span: item.span.to(&token.span),
                    value: UntypedExprKind::RValue(UntypedRValueKind::PreDecr(item)),
                    typespec: (),
                })
            }
            TokenKind::Minus => {
                let item = self.must(Self::postfix, "expression")?;
                Ok(UntypedExpr {
                    span: token.span.to(&item.span),
                    value: UntypedExprKind::RValue(UntypedRValueKind::Negate(Box::new(item))),
                    typespec: (),
                })
            }
            TokenKind::Plus => {
                if let Some(tt) = self.lexer.next() {
                    let tt = tt?;
                    if matches!(&tt.value, TokenKind::Integer(_) | TokenKind::Float(_)) {
                        return self.postfix(tt);
                    }
                    self.lexer.put_back(Ok(tt));
                }
                self.postfix(token)
            }
            _ => self.postfix(token),
        }
    }

    fn postfix(&mut self, token: Token) -> ParseResult {
        let mut callable = self.literal_id_or_recurse(token)?;
        while let Some(token) = self.lexer.next() {
            let token = token?;
            callable = match &token.value {
                TokenKind::OpenParen => {
                    let (args, span) = self.cse(token, TokenKind::CloseParen)?;
                    UntypedExpr {
                        span: callable.span.to(&span),
                        value: UntypedExprKind::RValue(UntypedRValueKind::Call {
                            callable: Box::new(callable),
                            args,
                        }),
                        typespec: (),
                    }
                }
                TokenKind::OpenBracket => {
                    let subscript = Box::new(self.must(Self::expr, "expression in brackets")?);
                    let token = self.must_next_token("close bracket")?;
                    let Token {
                        span: _,
                        value: TokenKind::CloseBracket,
                    } = token
                    else {
                        return Err(Error::UnexpectedToken {
                            expected: vec![TokenKind::CloseBracket],
                            found: token,
                        });
                    };

                    UntypedExpr {
                        span: callable.span.to(&subscript.span),
                        value: UntypedExprKind::LValue(UntypedLValueKind::BracketExpr {
                            left: Box::new(callable),
                            subscript,
                        }),
                        typespec: (),
                    }
                }
                TokenKind::Increment => {
                    let item = match callable.into_lval() {
                        Ok(l) => l,
                        Err(left) => return Err(Error::NotAnLValue(left)),
                    };
                    UntypedExpr {
                        span: item.span.to(&token.span),
                        value: UntypedExprKind::RValue(UntypedRValueKind::PostIncr(item)),
                        typespec: (),
                    }
                }
                TokenKind::Decrement => {
                    let item = match callable.into_lval() {
                        Ok(l) => l,
                        Err(left) => return Err(Error::NotAnLValue(left)),
                    };
                    UntypedExpr {
                        span: item.span.to(&token.span),
                        value: UntypedExprKind::RValue(UntypedRValueKind::PostDecr(item)),
                        typespec: (),
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
            TokenKind::Integer(i) => Self::tag_rval(UntypedRValueKind::Integer(i), token.span),
            TokenKind::Float(f) => Self::tag_rval(UntypedRValueKind::Float(f), token.span),
            TokenKind::Str(s) => Self::tag_rval(UntypedRValueKind::Str(s), token.span),
            TokenKind::Char(c) => Self::tag_rval(UntypedRValueKind::Char(c), token.span),
            TokenKind::Bool(b) => Self::tag_rval(UntypedRValueKind::Bool(b), token.span),
            TokenKind::Identifier(i) => Self::tag_lval(UntypedLValueKind::Variable(i), token.span),
            TokenKind::LambdaArg(i) if self.in_lambda => {
                Self::tag_rval(UntypedRValueKind::LambdaArg(i), token.span)
            }
            TokenKind::LambdaArg(_) => self.lambda(token)?,
            TokenKind::BackSlash => self.must(Self::lambda, "lambda expression")?,
            TokenKind::OpenParen => self.paren(token)?,
            TokenKind::OpenBrace => self.block(token)?,
            TokenKind::Function => self.function(token)?,
            TokenKind::OpenBracket => self.array(token)?,
            TokenKind::For => self.for_(token)?,
            TokenKind::If => self.if_(token)?,
            TokenKind::While => self.while_(token)?,
            TokenKind::Break => self.break_(token)?,
            TokenKind::Continue => self.continue_(token)?,
            TokenKind::Plus => UntypedExpr {
                span: token.span,
                value: UntypedExprKind::LValue(UntypedLValueKind::Variable("+".into())),
                typespec: (),
            },
            a => todo!("{}:{a:?}", token.span),
        })
    }

    fn function(&mut self, token: Token) -> ParseResult {
        todo!("{:?}", token)
    }

    fn paren(&mut self, _open_paren_token: Token) -> ParseResult {
        let expr = self.must(Self::expr, "expression")?;
        let end_paren_token = self.must_next_token("`)`")?;
        let Token {
            value: TokenKind::CloseParen,
            ..
        } = end_paren_token
        else {
            return Err(Error::UnexpectedToken {
                expected: vec![TokenKind::CloseParen],
                found: end_paren_token,
            });
        };
        Ok(expr)
    }

    fn cse(&mut self, opener: Token, end: TokenKind) -> Result<(Vec<UntypedExpr>, Span), Error> {
        let mut items = Vec::new();

        let mut token = self.must_next_token("expression or closing")?; // TODO update this message
        if end == token.value {
            return Ok((items, opener.span.to(&token.span)));
        }
        let span = loop {
            items.push(self.binop(token)?);
            token = self.must_next_token("comma or closing")?; // TODO update this message
            if end == token.value {
                break token.span;
            }
            match &token.value {
                TokenKind::Comma => {
                    token = self.must_next_token("expr")?;
                }
                _ => {
                    return Err(Error::UnexpectedToken {
                        expected: vec![end],
                        found: token,
                    })
                }
            }
            if end == token.value {
                break token.span;
            }
        };
        Ok((items, opener.span.to(&span)))
    }

    fn array(&mut self, open_bracket_token: Token) -> ParseResult {
        let (items, span) = self.cse(open_bracket_token, TokenKind::CloseBracket)?;
        Ok(UntypedExpr {
            span,
            value: UntypedExprKind::RValue(UntypedRValueKind::Array(items)),
            typespec: (),
        })
    }

    fn block(&mut self, open_brace_token: Token) -> ParseResult {
        let mut exprs = Vec::new();
        loop {
            let Some(token) = self.lexer.next() else {
                return Err(Error::UnexpectedEOF("expression in block"));
            };
            let token = token?;
            if TokenKind::CloseBrace == token.value {
                return Ok(UntypedExpr {
                    span: open_brace_token.span.to(&token.span),
                    value: UntypedExprKind::RValue(UntypedRValueKind::Block(exprs)),
                    typespec: (),
                });
            }
            exprs.push(self.expr(token)?);
        }
    }

    fn for_(&mut self, for_token: Token) -> ParseResult {
        let item = self.must(Self::lval, "lval in for")?;
        let in_token = self.must_next_token("`in`")?;
        let TokenKind::In = in_token.value else {
            return Err(Error::UnexpectedToken {
                expected: vec![TokenKind::In],
                found: in_token,
            });
        };
        let items = Box::new(self.must(Self::expr, "expresion items of for loop")?);
        let body = Box::new(self.must(Self::expr, "expresion body of for loop")?);
        Ok(UntypedExpr {
            span: for_token.span.to(&body.span),
            value: UntypedExprKind::RValue(UntypedRValueKind::For { item, items, body }),
            typespec: (),
        })
    }

    fn if_(&mut self, if_token: Token) -> ParseResult {
        let condition = Box::new(self.must(Self::expr, "condition in if")?);
        let body = Box::new(self.must(Self::expr, "body in if")?);
        // This does not bring joy
        let else_ = if let Some(token) = self.lexer.next() {
            let token = token?;
            if matches!(token.value, TokenKind::Else) {
                let token = self.must_next_token("else expression")?;
                Some(Box::new(self.expr(token)?))
            } else {
                self.lexer.put_back(Ok(token));
                None
            }
        } else {
            None
        };
        Ok(UntypedExpr {
            span: if_token.span.to(&body.span),
            value: UntypedExprKind::RValue(UntypedRValueKind::If {
                condition,
                body,
                else_,
            }),
            typespec: (),
        })
    }

    fn while_(&mut self, while_token: Token) -> ParseResult {
        let condition = Box::new(self.must(Self::expr, "condition in while")?);
        let body = Box::new(self.must(Self::expr, "body in while")?);
        Ok(UntypedExpr {
            span: while_token.span.to(&body.span),
            value: UntypedExprKind::RValue(UntypedRValueKind::While { condition, body }),
            typespec: (),
        })
    }

    fn declaration(&mut self, decl_type: Token) -> ParseResult {
        let kind = match decl_type.value {
            TokenKind::Let => DeclarationKind::Let,
            TokenKind::Var => DeclarationKind::Var,
            _ => unreachable!("Only pass tokens of type Let and Var into parser.declaration()"),
        };
        let token = self.must_next_token("variable name")?;
        let names = self.pattern(token, false)?;
        let token = self.must_next_token("`=`")?;
        let Token {
            span: _,
            value: TokenKind::SingleEquals,
        } = token
        else {
            return Err(Error::UnexpectedToken {
                expected: vec![TokenKind::SingleEquals],
                found: token,
            });
        };
        let value = Box::new(self.must(Self::expr, "expression")?);
        Ok(UntypedExpr {
            span: decl_type.span.to(&value.span),
            value: UntypedExprKind::RValue(UntypedRValueKind::Declaration { kind, names, value }),
            typespec: (),
        })
    }

    fn pattern(&mut self, token: Token, recursed: bool) -> Result<Pattern, Error> {
        let mut pattern = Vec::new();
        let mut force_tuple = false;
        self.lexer.put_back(Ok(token));
        loop {
            pattern.push(expect!(self => token {
                TokenKind::Identifier(s) => Pattern::One(s),
                TokenKind::OpenParen => {
                    let token = self.must_next_token("open paren or identifier")?;
                    self.pattern(token, true)?
                }
            }));
            expect!(self => token {
                TokenKind::CloseParen if recursed => break,
                TokenKind::SingleEquals if !recursed => {
                    self.lexer.put_back(Ok(token));
                    break
                }
                TokenKind::Comma => {
                    force_tuple = true;
                }
            });
        }
        if pattern.len() == 1 && !force_tuple {
            pattern.pop().map_or_else(|| unreachable!(), Ok)
        } else {
            Ok(Pattern::Many(pattern))
        }
    }

    fn break_(&mut self, token: Token) -> ParseResult {
        let mut span = token.span;
        let subexpr = if let Some(subexpr) = self.parse_expr() {
            let subexpr = subexpr?;
            span = span.to(&subexpr.span);
            Some(Box::new(subexpr))
        } else {
            None
        };
        Ok(UntypedExpr {
            span,
            value: UntypedExprKind::RValue(UntypedRValueKind::Break(subexpr)),
            typespec: (),
        })
    }

    #[allow(clippy::unnecessary_wraps)]
    fn continue_(&mut self, token: Token) -> ParseResult {
        let _ = self;
        Ok(UntypedExpr {
            span: token.span,
            value: UntypedExprKind::RValue(UntypedRValueKind::Continue),
            typespec: (),
        })
    }
}
