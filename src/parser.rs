use crate::ast::{
    Decl, DeclFunc, DeclPayload, Expr, ExprIntLit, ExprPayload, Program, Stmt, StmtCompound,
    StmtExpr, StmtPayload, StmtReturn,
};
use crate::loc::Loc;
use crate::token::{Token, TokenPayload};
use guard::guard;
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

    fn int_lit(&mut self) -> Result<ExprPayload, ParseError> {
        let token = self.peek().clone();
        guard!(let TokenPayload::IntLit(i) = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: "int literal".to_string(),
            });
        });
        self.inc_idx();
        Ok(ExprPayload::IntLit(ExprIntLit { value: *i }))
    }

    fn eof(&mut self) -> Result<(), ParseError> {
        let token = self.peek();
        guard!(let TokenPayload::EOF = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: "EOF".to_string(),
            });
        });
        self.inc_idx();
        Ok(())
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek().clone();
        let payload = self.int_lit()?;
        Ok(Expr {
            loc: token.loc,
            payload,
        })
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
        guard!(let TokenPayload::Semicolon = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: ";".to_string(),
            });
        });
        self.inc_idx();
        Ok(StmtPayload::Expr(StmtExpr { expr }))
    }

    fn return_stmt(&mut self) -> Result<StmtPayload, ParseError> {
        {
            let token = self.peek();
            guard!(let TokenPayload::Return = &token.payload else {
                return Err(ParseError::InvalidToken {
                    loc: token.loc.clone(),
                    token: token.clone(),
                    expected: "return".to_string(),
                });
            });
            self.inc_idx();
        }
        let expr = self.expr()?;
        {
            let token = self.peek();
            guard!(let TokenPayload::Semicolon = &token.payload else {
                return Err(ParseError::InvalidToken {
                    loc: token.loc.clone(),
                    token: token.clone(),
                    expected: ";".to_string(),
                });
            });
            self.inc_idx();
        }
        Ok(StmtPayload::Return(StmtReturn { expr }))
    }

    fn compound_stmt_inner(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = vec![];
        {
            let token = self.peek();
            guard!(let TokenPayload::BraceOpen = &token.payload else {
                return Err(ParseError::InvalidToken {
                    loc: token.loc.clone(),
                    token: token.clone(),
                    expected: "{".to_string(),
                });
            });
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
        guard!(let TokenPayload::Ident(name) = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: "identifier".to_string(),
            });
        });
        self.inc_idx();
        let token = self.peek();
        guard!(let TokenPayload::ParenOpen = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: "(".to_string(),
            });
        });
        self.inc_idx();
        let token = self.peek();
        guard!(let TokenPayload::ParenClose = &token.payload else {
            return Err(ParseError::InvalidToken {
                loc: token.loc.clone(),
                token: token.clone(),
                expected: ")".to_string(),
            });
        });
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
