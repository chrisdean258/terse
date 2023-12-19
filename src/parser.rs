#![allow(clippy::needless_pass_by_value)]
use crate::{
    expression::{
        BinOpKind, DeclIds, DeclarationKind, FlatBinOpKind, LValueKind, RValueKind,
        ShortCircuitBinOpKind, UntypedExpr, UntypedExprKind, UntypedLValue,
    },
    lexer::LexError,
    span::Span,
    token::{Kind as TokenKind, Token},
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
    NotAnLValue(UntypedExpr),
}

#[derive(Debug)]
pub struct Ast {
    pub exprs: Vec<UntypedExpr>,
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for e in &self.exprs {
            writeln!(f, "{e}")?;
        }
        Ok(())
    }
}

struct Parser<I: Iterator<Item = Result<Token, LexError>>> {
    lexer: PutBack<I>,
    in_lambda: bool,
}

pub fn parse(lexer: impl Iterator<Item = Result<Token, LexError>>) -> Result<Ast, ParseError> {
    let mut tree = Ast { exprs: Vec::new() };
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

type ParseResult = Result<UntypedExpr, ParseError>;

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
                left = UntypedExpr {
                    span: left.span.to(&right.span),
                    value: UntypedExprKind::RValue(RValueKind::$return {
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
                    return Err(ParseError::UnexpectedEOF("expression"));
                };
                let expr = self.$next(token?)?;
                rest.push((op, Box::new(expr)));
            }
            Ok(if rest.is_empty() {
                first
            } else {
                UntypedExpr {
                    span : first.span.to(&rest.last().unwrap().1.span),
                    value: UntypedExprKind::RValue(RValueKind::FlatBinOp {
                        first: Box::new(first),
                        rest,
                    })
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
            Err(e) => Some(Err(ParseError::LexError(e))),
            Ok(t) => match t.value {
                TokenKind::CloseParen | TokenKind::CloseBrace | TokenKind::CloseBracket => {
                    self.lexer.put_back(Ok(t));
                    None
                }
                _ => Some(self.expr(t)),
            },
        }
    }

    const fn tag_rval(value: RValueKind, span: Span) -> UntypedExpr {
        UntypedExpr {
            span,
            value: UntypedExprKind::RValue(value),
        }
    }

    const fn tag_lval(value: LValueKind, span: Span) -> UntypedExpr {
        UntypedExpr {
            span,
            value: UntypedExprKind::LValue(value),
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
            TokenKind::Let | TokenKind::Var => self.declaration(token),
            _ => self.equals(token),
        }
    }

    fn lval(&mut self, token: Token) -> Result<UntypedLValue, ParseError> {
        match self.expr(token)?.into_lval() {
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
            TokenKind::SingleEquals => {
                let left = match left.into_lval() {
                    Ok(l) => l,
                    Err(left) => return Err(ParseError::NotAnLValue(left)),
                };
                let Some(token) = self.lexer.next() else {
                    return Err(ParseError::UnexpectedEOF("expression"));
                };
                let right = Box::new(self.expr(token?)?);
                let rtn = UntypedExpr {
                    span: left.span.to(&right.span),
                    value: UntypedExprKind::RValue(RValueKind::Assignment { left, right }),
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
            Err(left) => return Err(ParseError::NotAnLValue(left)),
        };
        let Some(token) = self.lexer.next() else {
            return Err(ParseError::UnexpectedEOF("expression"));
        };
        let right = Box::new(self.expr(token?)?);
        let span = left.span.to(&right.span);
        let rtn = UntypedExpr {
            span: span.clone(),
            value: UntypedExprKind::RValue(RValueKind::Assignment {
                left,
                right: Box::new(UntypedExpr {
                    span,
                    value: UntypedExprKind::RValue(RValueKind::BinOp { left: r, op, right }),
                }),
            }),
        };
        Ok(rtn)
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
            value: UntypedExprKind::LValue(LValueKind::Tuple(exprs)),
        })
    }

    fn under_comma(&mut self, token: Token) -> ParseResult {
        self.pipe(token)
    }

    binops!(
        pipe #short_circuit {
            TokenKind::PipeArrow => ShortCircuitBinOpKind::Pipe,
            TokenKind::SkinnyArrow => ShortCircuitBinOpKind::InvertedCall,
        } => boolean_or
    );

    fn lambda(&mut self, token: Token) -> ParseResult {
        let save = std::mem::replace(&mut self.in_lambda, true);
        let expr = self.boolean_or(token)?;
        self.in_lambda = save;
        Ok(UntypedExpr {
            span: expr.span.clone(),
            value: UntypedExprKind::RValue(RValueKind::Lambda(Rc::new(expr))),
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
                    let (args, span) = self.cse(token, TokenKind::CloseParen)?;
                    UntypedExpr {
                        span: callable.span.to(&span),
                        value: UntypedExprKind::RValue(RValueKind::Call {
                            callable: Box::new(callable),
                            args,
                        }),
                    }
                }
                TokenKind::OpenBracket => {
                    let token = self.must_next_token("expression in brackets")?;
                    let subscript = Box::new(self.expr(token)?);
                    let token = self.must_next_token("close bracket")?;
                    let Token {
                        span: _,
                        value: TokenKind::CloseBracket,
                    } = token
                    else {
                        return Err(ParseError::UnexpectedToken {
                            expected: vec![TokenKind::CloseBracket],
                            found: token,
                        });
                    };

                    UntypedExpr {
                        span: callable.span.to(&subscript.span),
                        value: UntypedExprKind::LValue(LValueKind::BracketExpr {
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
            TokenKind::Integer(i) => Self::tag_rval(RValueKind::Integer(i), token.span),
            TokenKind::Float(f) => Self::tag_rval(RValueKind::Float(f), token.span),
            TokenKind::Str(s) => Self::tag_rval(RValueKind::Str(s), token.span),
            TokenKind::Char(c) => Self::tag_rval(RValueKind::Char(c), token.span),
            TokenKind::Bool(b) => Self::tag_rval(RValueKind::Bool(b), token.span),
            TokenKind::Identifier(i) => Self::tag_lval(LValueKind::Variable(i), token.span),
            TokenKind::LambdaArg(i) if self.in_lambda => {
                Self::tag_rval(RValueKind::LambdaArg(i), token.span)
            }
            TokenKind::LambdaArg(_) => self.lambda(token)?,
            TokenKind::BackSlash => {
                let token = self.must_next_token("lambda expression")?;
                let expr = self.lambda(token)?;
                UntypedExpr {
                    span: expr.span.clone(),
                    value: UntypedExprKind::RValue(RValueKind::Lambda(Rc::new(expr))),
                }
            }
            TokenKind::OpenParen => self.paren(token)?,
            TokenKind::OpenBrace => self.block(token)?,
            TokenKind::Function => self.function(token)?,
            TokenKind::OpenBracket => self.array(token)?,
            TokenKind::For => self.for_(token)?,
            TokenKind::If => self.if_(token)?,
            TokenKind::While => self.while_(token)?,
            TokenKind::Break => self.break_(token)?,
            a => todo!("{}:{a:?}", token.span),
        })
    }

    fn function(&mut self, token: Token) -> ParseResult {
        todo!("{:?}", token)
    }

    fn paren(&mut self, open_paren_token: Token) -> ParseResult {
        let token = self.must_next_token("expression")?;
        let mut expr = self.expr(token)?;
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
        expr.span = open_paren_token.span.to(&span);
        Ok(expr)
    }

    fn cse(
        &mut self,
        token: Token,
        end: TokenKind,
    ) -> Result<(Vec<UntypedExpr>, Span), ParseError> {
        let mut items = Vec::new();
        let mut need_comma = false;
        let span = loop {
            let mut token = self.must_next_token("close parens or comma")?; // TODO update this message
            if end == token.value {
                break token.span;
            }
            match (&token.value, need_comma) {
                (TokenKind::Comma, true) => {
                    token = self.must_next_token("expr")?;
                }
                (_, false) => {}
                (_, true) => {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec![TokenKind::CloseParen],
                        found: token,
                    })
                }
            }
            items.push(self.under_comma(token)?);
            need_comma = true;
        };
        Ok((items, token.span.to(&span)))
    }

    fn array(&mut self, open_bracket_token: Token) -> ParseResult {
        let (items, span) = self.cse(open_bracket_token, TokenKind::CloseBracket)?;
        Ok(UntypedExpr {
            span,
            value: UntypedExprKind::RValue(RValueKind::Array(items)),
        })
    }

    fn block(&mut self, open_brace_token: Token) -> ParseResult {
        let mut exprs = Vec::new();
        loop {
            let Some(token) = self.lexer.next() else {
                return Err(ParseError::UnexpectedEOF("expression in block"));
            };
            let token = token?;
            if TokenKind::CloseBrace == token.value {
                return Ok(UntypedExpr {
                    span: open_brace_token.span.to(&token.span),
                    value: UntypedExprKind::RValue(RValueKind::Block(exprs)),
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
        Ok(UntypedExpr {
            span: for_token.span.to(&body.span),
            value: UntypedExprKind::RValue(RValueKind::For { item, items, body }),
        })
    }

    fn if_(&mut self, if_token: Token) -> ParseResult {
        let token = self.must_next_token("condition in if")?;
        let condition = Box::new(self.expr(token)?);
        let token = self.must_next_token("body in if")?;
        let body = Box::new(self.expr(token)?);
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
            value: UntypedExprKind::RValue(RValueKind::If {
                condition,
                body,
                else_,
            }),
        })
    }

    fn while_(&mut self, while_token: Token) -> ParseResult {
        let token = self.must_next_token("condition in while")?;
        let condition = Box::new(self.expr(token)?);
        let token = self.must_next_token("body in while")?;
        let body = Box::new(self.expr(token)?);
        Ok(UntypedExpr {
            span: while_token.span.to(&body.span),
            value: UntypedExprKind::RValue(RValueKind::While { condition, body }),
        })
    }

    fn declaration(&mut self, decl_type: Token) -> ParseResult {
        let kind = match decl_type.value {
            TokenKind::Let => DeclarationKind::Let,
            TokenKind::Var => DeclarationKind::Var,
            _ => unreachable!("Only pass tokens of type Let and Var into parser.declaration()"),
        };
        let token = self.must_next_token("variable name")?;
        let names = self.ids(token, false)?;
        // let Token {
        // span: _,
        // value: TokenKind::Identifier(name),
        // } = token
        // else {
        // return Err(ParseError::UnexpectedToken {
        // expected: vec![TokenKind::Identifier("".into())],
        // found: token,
        // });
        // };
        let token = self.must_next_token("`=`")?;
        let Token {
            span: _,
            value: TokenKind::SingleEquals,
        } = token
        else {
            return Err(ParseError::UnexpectedToken {
                expected: vec![TokenKind::SingleEquals],
                found: token,
            });
        };
        let token = self.must_next_token("expression")?;
        let value = Box::new(self.expr(token)?);
        Ok(UntypedExpr {
            span: decl_type.span.to(&value.span),
            value: UntypedExprKind::RValue(RValueKind::Declaration { kind, names, value }),
        })
    }

    fn ids(&mut self, token: Token, recursed: bool) -> Result<DeclIds, ParseError> {
        let mut ids = Vec::new();
        let mut id;
        let mut force_tuple = false;
        self.lexer.put_back(Ok(token));
        loop {
            id = expect!(self => token {
                TokenKind::Identifier(s) => DeclIds::One(s),
                TokenKind::OpenParen => {
                    let token = self.must_next_token("open paren or identifier")?;
                    self.ids(token, true)?
                }
            });
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
            ids.push(id);
        }
        if ids.is_empty() && !force_tuple {
            Ok(id)
        } else {
            ids.push(id);
            Ok(DeclIds::Many(ids))
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
            value: UntypedExprKind::RValue(RValueKind::Break(subexpr)),
        })
    }
}
