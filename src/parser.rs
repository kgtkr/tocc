use crate::clang::{
    Decl, DeclFunc, DeclPayload, Expr, ExprAdd, ExprDiv, ExprIntLit, ExprMul, ExprNeg, ExprPayload,
    ExprSub, Program, Stmt, StmtCompound, StmtExpr, StmtPayload, StmtReturn,
};
use crate::token::{Token, TokenPayload};
use derive_more::Display;
use thiserror::Error;

macro_rules! parser_or {
    ($parser: expr, $f1: expr, $f2: expr,) => {
        $parser.or($f1, $f2)
    };
    ($parser: expr, $f1: expr, $($f: expr,)+) => {
        $parser.or($f1, |p| parser_or!(p, $($f,)*))
    };
}

#[derive(Error, Debug, Clone)]
#[error("{token}: {payload}")]
pub struct ParseError {
    token: Token,
    payload: ParseErrorPayload,
}

#[derive(Debug, Clone, Display)]
pub enum ParseErrorPayload {
    #[display(fmt = "unexpected token, expected: {expected}")]
    UnexpectedToken { expected: String },
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, idx: 0 }
    }

    fn inc_idx(&mut self) {
        self.idx += 1;
    }

    fn peek(&self) -> &Token {
        // 最後のトークンはEOF
        self.tokens.get(self.idx).unwrap()
    }

    fn satisfy<T>(
        &mut self,
        f: impl FnOnce(&Token) -> Result<T, ParseErrorPayload>,
    ) -> Result<T, ParseError> {
        let token = self.peek().clone();
        let result = f(&token).map_err(|payload| ParseError {
            token: token.clone(),
            payload,
        })?;
        self.inc_idx();
        Ok(result)
    }

    fn satisfy_(
        &mut self,
        f: impl FnOnce(&Token) -> bool,
        expected: impl ToString,
    ) -> Result<(), ParseError> {
        self.satisfy(|token| {
            if f(token) {
                Ok(())
            } else {
                Err(ParseErrorPayload::UnexpectedToken {
                    expected: expected.to_string(),
                })
            }
        })
    }

    fn optional_with_error<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, ParseError>,
    ) -> Result<Result<T, ParseError>, ParseError> {
        let prev_idx = self.idx;
        match f(self) {
            Ok(result) => Ok(Ok(result)),
            Err(e) => {
                if self.idx != prev_idx {
                    Err(e)
                } else {
                    Ok(Err(e))
                }
            }
        }
    }

    fn optional<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, ParseError>,
    ) -> Result<Option<T>, ParseError> {
        self.optional_with_error(f).map(Result::ok)
    }

    fn or<T>(
        &mut self,
        f1: impl FnOnce(&mut Self) -> Result<T, ParseError>,
        f2: impl FnOnce(&mut Self) -> Result<T, ParseError>,
    ) -> Result<T, ParseError> {
        match self.optional_with_error(f1)? {
            Ok(result) => Ok(result),
            Err(e1) => {
                match self.optional_with_error(f2)? {
                    Ok(result) => Ok(result),
                    Err(e2) => {
                        match (e1.payload, e2.payload) {
                            (
                                ParseErrorPayload::UnexpectedToken {
                                    expected: expected1,
                                },
                                ParseErrorPayload::UnexpectedToken {
                                    expected: expected2,
                                },
                            ) => Err(ParseError {
                                // 普通はe1.token==e2.tokenになるはず
                                token: e1.token,
                                payload: ParseErrorPayload::UnexpectedToken {
                                    expected: format!("{}, {}", expected1, expected2),
                                },
                            }),
                        }
                    }
                }
            }
        }
    }

    fn fold<T, R>(
        &mut self,
        f: impl Fn(&mut Self) -> Result<T, ParseError>,
        init: R,
        fold: impl Fn(R, T) -> R,
    ) -> Result<R, ParseError> {
        let mut result = init;
        loop {
            match self.optional(|p| f(p))? {
                Some(t) => result = fold(result, t),
                None => break,
            }
        }
        Ok(result)
    }

    fn fold1<T>(
        &mut self,
        f: impl Fn(&mut Self) -> Result<T, ParseError>,
        fold: impl Fn(T, T) -> T,
    ) -> Result<T, ParseError> {
        let first = f(self)?;
        self.fold(f, first, fold)
    }

    fn many<T>(
        &mut self,
        f: impl Fn(&mut Self) -> Result<T, ParseError>,
    ) -> Result<Vec<T>, ParseError> {
        self.fold(f, Vec::new(), |mut v, t| {
            v.push(t);
            v
        })
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        parser_or!(
            self,
            |p| p.satisfy(|token| match token.payload {
                TokenPayload::IntLit(i) => Ok(Expr {
                    loc: token.loc.clone(),
                    payload: ExprPayload::IntLit(ExprIntLit { value: i }),
                }),
                _ => Err(ParseErrorPayload::UnexpectedToken {
                    expected: "int literal".to_string(),
                }),
            }),
            |p| {
                p.satisfy_(
                    |token| matches!(token.payload, TokenPayload::ParenOpen),
                    "(",
                )?;
                let expr = p.expr()?;
                p.satisfy_(
                    |token| matches!(token.payload, TokenPayload::ParenClose),
                    ")",
                )?;
                Ok(expr)
            },
        )
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        parser_or!(
            self,
            |p| {
                p.satisfy_(|token| matches!(token.payload, TokenPayload::Plus), "+")?;
                let expr = p.primary()?;
                Ok(expr)
            },
            |p| {
                p.satisfy_(|token| matches!(token.payload, TokenPayload::Minus), "-")?;
                let expr = p.primary()?;
                Ok(Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Neg(ExprNeg {
                        expr: Box::new(expr),
                    }),
                })
            },
            |p| p.primary(),
        )
    }

    fn muldiv(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        loop {
            let token = self.peek().clone();
            match token.payload {
                TokenPayload::Asterisk => {
                    self.inc_idx();
                    let rhs = self.unary()?;
                    expr = Expr {
                        loc: token.loc,
                        payload: ExprPayload::Mul(ExprMul {
                            lhs: Box::new(expr),
                            rhs: Box::new(rhs),
                        }),
                    };
                }
                TokenPayload::Slash => {
                    self.inc_idx();
                    let rhs = self.unary()?;
                    expr = Expr {
                        loc: token.loc,
                        payload: ExprPayload::Div(ExprDiv {
                            lhs: Box::new(expr),
                            rhs: Box::new(rhs),
                        }),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn addsub(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.muldiv()?;
        loop {
            let token = self.peek().clone();
            match token.payload {
                TokenPayload::Plus => {
                    self.inc_idx();
                    let rhs = self.muldiv()?;
                    expr = Expr {
                        loc: token.loc,
                        payload: ExprPayload::Add(ExprAdd {
                            lhs: Box::new(expr),
                            rhs: Box::new(rhs),
                        }),
                    };
                }
                TokenPayload::Minus => {
                    self.inc_idx();
                    let rhs = self.muldiv()?;
                    expr = Expr {
                        loc: token.loc,
                        payload: ExprPayload::Sub(ExprSub {
                            lhs: Box::new(expr),
                            rhs: Box::new(rhs),
                        }),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        let expr = self.addsub()?;
        Ok(expr)
    }

    fn stat(&mut self) -> Result<Stmt, ParseError> {
        let token = self.peek().clone();
        let payload = parser_or!(
            self,
            |p| p.return_stmt().map(StmtPayload::Return),
            |p| p.compound_stmt().map(StmtPayload::Compound),
            |p| p.expr_stmt().map(StmtPayload::Expr),
        )?;

        Ok(Stmt {
            loc: token.loc,
            payload,
        })
    }

    fn expr_stmt(&mut self) -> Result<StmtExpr, ParseError> {
        let expr = self.expr()?;
        self.satisfy(|token| match token.payload {
            TokenPayload::Semicolon => Ok(()),
            _ => Err(ParseErrorPayload::UnexpectedToken {
                expected: ";".to_string(),
            }),
        })?;
        Ok(StmtExpr { expr })
    }

    fn return_stmt(&mut self) -> Result<StmtReturn, ParseError> {
        self.satisfy(|token| match token.payload {
            TokenPayload::Return => Ok(()),
            _ => Err(ParseErrorPayload::UnexpectedToken {
                expected: "return".to_string(),
            }),
        })?;
        let expr = self.expr()?;
        self.satisfy(|token| match token.payload {
            TokenPayload::Semicolon => Ok(()),
            _ => Err(ParseErrorPayload::UnexpectedToken {
                expected: ";".to_string(),
            }),
        })?;
        Ok(StmtReturn { expr })
    }

    fn compound_stmt(&mut self) -> Result<StmtCompound, ParseError> {
        let mut stmts = vec![];
        self.satisfy(|token| match token.payload {
            TokenPayload::BraceOpen => Ok(()),
            _ => Err(ParseErrorPayload::UnexpectedToken {
                expected: "{".to_string(),
            }),
        })?;
        loop {
            let token = self.peek();
            match &token.payload {
                TokenPayload::BraceClose => {
                    self.inc_idx();
                    break;
                }
                _ => stmts.push(self.stat()?),
            }
        }

        Ok(StmtCompound { stmts })
    }

    fn decl(&mut self) -> Result<Decl, ParseError> {
        let token = self.peek().clone();
        let payload = DeclPayload::Func(self.func_decl()?);
        Ok(Decl {
            loc: token.loc,
            payload,
        })
    }

    fn func_decl(&mut self) -> Result<DeclFunc, ParseError> {
        let name = self.satisfy(|token| match &token.payload {
            TokenPayload::Ident(name) => Ok(name.clone()),
            _ => Err(ParseErrorPayload::UnexpectedToken {
                expected: "identifier".to_string(),
            }),
        })?;
        self.satisfy(|token| match token.payload {
            TokenPayload::ParenOpen => Ok(()),
            _ => Err(ParseErrorPayload::UnexpectedToken {
                expected: "(".to_string(),
            }),
        })?;
        self.satisfy(|token| match token.payload {
            TokenPayload::ParenClose => Ok(()),
            _ => Err(ParseErrorPayload::UnexpectedToken {
                expected: ")".to_string(),
            }),
        })?;
        let stmts = self.compound_stmt()?;
        Ok(DeclFunc {
            name: name.clone(),
            body: stmts,
        })
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut decls = vec![];
        loop {
            let token = self.peek();
            match &token.payload {
                TokenPayload::EOF => {
                    self.inc_idx();
                    return Ok(Program { decls });
                }
                _ => decls.push(self.decl()?),
            }
        }
    }
}
