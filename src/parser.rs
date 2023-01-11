use crate::clang::{
    Decl, DeclFunc, DeclPayload, Expr, ExprAdd, ExprIntLit, ExprPayload, ExprSub, Program, Stmt,
    StmtCompound, StmtExpr, StmtPayload, StmtReturn,
};
use crate::loc::Loc;
use crate::token::{Token, TokenPayload};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("{loc} - invalid token: {token}, expected: {expected}")]
    InvalidToken {
        loc: Loc,
        token: Token,
        expected: String,
    },
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
                let token = self.peek().clone();
                let TokenPayload::ParenClose = token.payload else {
                    return Err(ParseError::InvalidToken {
                        loc: token.loc.clone(),
                        token: token.clone(),
                        expected: ")".to_string(),
                    });
                };
                self.inc_idx();
                Ok(expr)
            }
            _ => Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token,
                expected: "int literal".to_string(),
            }),
        }
    }

    fn addsub(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            let token = self.peek().clone();
            match token.payload {
                TokenPayload::Plus => {
                    self.inc_idx();
                    let rhs = self.primary()?;
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
                    let rhs = self.primary()?;
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
        let payload = match &token.payload {
            TokenPayload::Return => self.return_stmt()?,
            TokenPayload::BraceOpen => self.compound_stmt()?,
            _ => self.expr_stmt()?,
        };
        Ok(Stmt {
            loc: token.loc,
            payload,
        })
    }

    fn expr_stmt(&mut self) -> Result<StmtPayload, ParseError> {
        let expr = self.expr()?;
        let token = self.peek();
        let TokenPayload::Semicolon = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: ";".to_string(),
            });
        };
        self.inc_idx();
        Ok(StmtPayload::Expr(StmtExpr { expr }))
    }

    fn return_stmt(&mut self) -> Result<StmtPayload, ParseError> {
        {
            let token = self.peek();
            let TokenPayload::Return = &token.payload else {
                return Err(ParseError::InvalidToken {
                    loc: token.loc.clone(),
                    token: token.clone(),
                    expected: "return".to_string(),
                });
            };
            self.inc_idx();
        }
        let expr = self.expr()?;
        {
            let token = self.peek();
            let TokenPayload::Semicolon = &token.payload else {
                return Err(ParseError::InvalidToken {
                    loc: token.loc.clone(),
                    token: token.clone(),
                    expected: ";".to_string(),
                });
            };
            self.inc_idx();
        }
        Ok(StmtPayload::Return(StmtReturn { expr }))
    }

    fn compound_stmt_inner(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = vec![];
        {
            let token = self.peek();
            let TokenPayload::BraceOpen = &token.payload else {
                return Err(ParseError::InvalidToken {
                    loc: token.loc.clone(),
                    token: token.clone(),
                    expected: "{".to_string(),
                });
            };
            self.inc_idx();
        }
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

        Ok(stmts)
    }

    fn compound_stmt(&mut self) -> Result<StmtPayload, ParseError> {
        let stmts = self.compound_stmt_inner()?;
        Ok(StmtPayload::Compound(StmtCompound { stmts }))
    }

    fn decl(&mut self) -> Result<Decl, ParseError> {
        let token = self.peek().clone();
        let payload = self.func_decl()?;
        Ok(Decl {
            loc: token.loc,
            payload,
        })
    }

    fn func_decl(&mut self) -> Result<DeclPayload, ParseError> {
        let token = self.peek().clone();
        let TokenPayload::Ident(name) = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: "identifier".to_string(),
            });
        };
        self.inc_idx();
        let token = self.peek();
        let TokenPayload::ParenOpen = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: "(".to_string(),
            });
        };
        self.inc_idx();
        let token = self.peek();
        let TokenPayload::ParenClose = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: ")".to_string(),
            });
        };
        self.inc_idx();
        let stmts = self.compound_stmt_inner()?;
        Ok(DeclPayload::Func(DeclFunc {
            name: name.clone(),
            body: StmtCompound { stmts },
        }))
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
