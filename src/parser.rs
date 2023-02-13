use crate::clang::{
    Decl, DeclFunc, DeclPayload, Expr, ExprAdd, ExprDiv, ExprIntLit, ExprMul, ExprPayload, ExprSub,
    Program, Stmt, StmtCompound, StmtExpr, StmtPayload, StmtReturn,
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

    fn or<T>(
        &mut self,
        f1: impl FnOnce(&mut Self) -> Result<T, ParseError>,
        f2: impl FnOnce(&mut Self) -> Result<T, ParseError>,
    ) -> Result<T, ParseError> {
        let prev_idx = self.idx;
        match f1(self) {
            Ok(result) => Ok(result),
            Err(e1) => {
                if self.idx != prev_idx {
                    Err(e1)
                } else {
                    match f2(self) {
                        Ok(result) => Ok(result),
                        Err(e2) => {
                            if self.idx != prev_idx {
                                Err(e2)
                            } else {
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
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek().clone();
        match token.payload {
            TokenPayload::IntLit(i) => {
                self.inc_idx();
                Ok(Expr {
                    loc: token.loc,
                    payload: ExprPayload::IntLit(ExprIntLit { value: i }),
                })
            }
            TokenPayload::ParenOpen => {
                self.inc_idx();
                let expr = self.expr()?;
                self.satisfy(|token| match token.payload {
                    TokenPayload::ParenClose => Ok(()),
                    _ => Err(ParseErrorPayload::UnexpectedToken {
                        expected: ")".to_string(),
                    }),
                })?;
                Ok(expr)
            }
            _ => Err(ParseError {
                token,
                payload: ParseErrorPayload::UnexpectedToken {
                    expected: "int literal".to_string(),
                },
            }),
        }
    }

    fn muldiv(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            let token = self.peek().clone();
            match token.payload {
                TokenPayload::Asterisk => {
                    self.inc_idx();
                    let rhs = self.primary()?;
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
                    let rhs = self.primary()?;
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
