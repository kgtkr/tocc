use crate::clang::{
    Decl, DeclFunc, DeclParam, DeclPayload, Expr, ExprAdd, ExprAssign, ExprCall, ExprDiv, ExprEq,
    ExprGe, ExprGt, ExprIntLit, ExprLValue, ExprLe, ExprLt, ExprMul, ExprNe, ExprNeg, ExprPayload,
    ExprSub, LValueVar, Program, Stmt, StmtCompound, StmtExpr, StmtFor, StmtIf, StmtPayload,
    StmtReturn, StmtVarDecl, StmtWhile, Type, TypeInt,
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
    ) -> Result<Token, ParseError> {
        self.satisfy(|token| {
            if f(token) {
                Ok(token.clone())
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
        while let Some(t) = self.optional(|p| f(p))? {
            result = fold(result, t);
        }
        Ok(result)
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

    fn sep_by<T>(
        &mut self,
        f: impl Fn(&mut Self) -> Result<T, ParseError>,
        sep: impl Fn(&mut Self) -> Result<(), ParseError>,
    ) -> Result<Vec<T>, ParseError> {
        let mut result = Vec::new();
        if let Some(x) = self.optional(|p| f(p))? {
            result.push(x);
            result.extend(self.many(|p| {
                sep(p)?;
                f(p)
            })?);
        }
        Ok(result)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        parser_or!(
            self,
            |p| p.satisfy(|token| match token.payload {
                TokenPayload::IntLit(i) => Ok(Expr {
                    loc: token.loc.clone(),
                    payload: ExprPayload::IntLit(ExprIntLit {
                        value: i,
                        value_loc: token.loc.clone()
                    }),
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
            |p| {
                let token = p.peek().clone();
                let (name, name_loc) = p.satisfy(|token| match &token.payload {
                    TokenPayload::Ident(ident) => Ok((ident.clone(), token.loc.clone())),
                    _ => Err(ParseErrorPayload::UnexpectedToken {
                        expected: "identifier".to_string(),
                    }),
                })?;
                parser_or!(
                    p,
                    |p| {
                        p.satisfy_(
                            |token| matches!(token.payload, TokenPayload::ParenOpen),
                            "(",
                        )?;
                        let args = p.sep_by(
                            |p| p.expr(),
                            |p| {
                                p.satisfy_(
                                    |token| matches!(token.payload, TokenPayload::Comma),
                                    ",",
                                )?;
                                Ok(())
                            },
                        )?;
                        p.satisfy_(
                            |token| matches!(token.payload, TokenPayload::ParenClose),
                            ")",
                        )?;
                        Ok(Expr {
                            loc: token.loc.clone(),
                            payload: ExprPayload::Call(ExprCall {
                                name: name.clone(),
                                name_loc: name_loc.clone(),
                                args,
                            }),
                        })
                    },
                    |_| {
                        Ok(Expr {
                            loc: token.loc.clone(),
                            payload: ExprPayload::LValue(ExprLValue::Var(LValueVar {
                                name: name.clone(),
                                name_loc: name_loc.clone(),
                            })),
                        })
                    },
                )
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
                let minus =
                    p.satisfy_(|token| matches!(token.payload, TokenPayload::Minus), "-")?;
                let expr = p.primary()?;
                Ok(Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Neg(ExprNeg {
                        minus_loc: minus.loc.clone(),
                        expr: Box::new(expr),
                    }),
                })
            },
            |p| p.primary(),
        )
    }

    fn muldiv(&mut self) -> Result<Expr, ParseError> {
        enum Op {
            Mul,
            Div,
        }
        let expr = self.unary()?;
        self.fold(
            |p| {
                parser_or!(
                    p,
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::Asterisk), "*")?;
                        let rhs = p.unary()?;
                        Ok((Op::Mul, rhs))
                    },
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::Slash), "/")?;
                        let rhs = p.unary()?;
                        Ok((Op::Div, rhs))
                    },
                )
            },
            expr,
            |expr, (op, rhs)| match op {
                Op::Mul => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Mul(ExprMul {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
                Op::Div => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Div(ExprDiv {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
            },
        )
    }

    fn addsub(&mut self) -> Result<Expr, ParseError> {
        enum Op {
            Add,
            Sub,
        }
        let expr = self.muldiv()?;
        self.fold(
            |p| {
                parser_or!(
                    p,
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::Plus), "+")?;
                        let rhs = p.muldiv()?;
                        Ok((Op::Add, rhs))
                    },
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::Minus), "-")?;
                        let rhs = p.muldiv()?;
                        Ok((Op::Sub, rhs))
                    },
                )
            },
            expr,
            |expr, (op, rhs)| match op {
                Op::Add => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Add(ExprAdd {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
                Op::Sub => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Sub(ExprSub {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
            },
        )
    }

    fn relational(&mut self) -> Result<Expr, ParseError> {
        enum Op {
            Lt,
            Gt,
            Le,
            Ge,
        }
        let expr = self.addsub()?;
        self.fold(
            |p| {
                parser_or!(
                    p,
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::Lt), "<")?;
                        let rhs = p.addsub()?;
                        Ok((Op::Lt, rhs))
                    },
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::Le), "<=")?;
                        let rhs = p.addsub()?;
                        Ok((Op::Le, rhs))
                    },
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::Gt), ">")?;
                        let rhs = p.addsub()?;
                        Ok((Op::Gt, rhs))
                    },
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::Ge), ">=")?;
                        let rhs = p.addsub()?;
                        Ok((Op::Ge, rhs))
                    },
                )
            },
            expr,
            |expr, (op, rhs)| match op {
                Op::Lt => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Lt(ExprLt {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
                Op::Le => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Le(ExprLe {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
                Op::Gt => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Gt(ExprGt {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
                Op::Ge => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Ge(ExprGe {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
            },
        )
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        enum Op {
            Eq,
            Ne,
        }
        let expr = self.relational()?;
        self.fold(
            |p| {
                parser_or!(
                    p,
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::EqEq), "==")?;
                        let rhs = p.relational()?;
                        Ok((Op::Eq, rhs))
                    },
                    |p| {
                        p.satisfy_(|token| matches!(token.payload, TokenPayload::Neq), "!=")?;
                        let rhs = p.relational()?;
                        Ok((Op::Ne, rhs))
                    },
                )
            },
            expr,
            |expr, (op, rhs)| match op {
                Op::Eq => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Eq(ExprEq {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
                Op::Ne => Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Ne(ExprNe {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }),
                },
            },
        )
    }

    fn assign(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality()?;
        parser_or!(
            self,
            |p| {
                p.satisfy_(|token| matches!(token.payload, TokenPayload::Eq), "=")?;
                let rhs = p.assign()?;
                Ok(Expr {
                    loc: expr.loc.clone(),
                    payload: ExprPayload::Assign(ExprAssign {
                        lhs: Box::new(expr.clone()),
                        rhs: Box::new(rhs),
                    }),
                })
            },
            |_| Ok(expr.clone()),
        )
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        let expr = self.assign()?;
        Ok(expr)
    }

    fn stat(&mut self) -> Result<Stmt, ParseError> {
        let token = self.peek().clone();
        let payload = parser_or!(
            self,
            |p| p.return_stmt().map(StmtPayload::Return),
            |p| p.compound_stmt().map(StmtPayload::Compound),
            |p| p.expr_stmt().map(StmtPayload::Expr),
            |p| p.var_decl_stmt().map(StmtPayload::VarDecl),
            |p| p.if_stmt().map(StmtPayload::If),
            |p| p.while_stmt().map(StmtPayload::While),
            |p| p.for_stmt().map(StmtPayload::For),
        )?;

        Ok(Stmt {
            loc: token.loc,
            payload,
        })
    }

    fn expr_stmt(&mut self) -> Result<StmtExpr, ParseError> {
        let expr = self.expr()?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::Semicolon),
            ";",
        )?;
        Ok(StmtExpr { expr })
    }

    fn return_stmt(&mut self) -> Result<StmtReturn, ParseError> {
        let return_ = self.satisfy_(
            |token| matches!(token.payload, TokenPayload::Return),
            "return",
        )?;
        let expr = self.expr()?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::Semicolon),
            ";",
        )?;
        Ok(StmtReturn {
            return_loc: return_.loc,
            expr,
        })
    }

    fn compound_stmt(&mut self) -> Result<StmtCompound, ParseError> {
        let lbrace = self.satisfy_(
            |token| matches!(token.payload, TokenPayload::BraceOpen),
            "{",
        )?;
        let stmts = self.many(|p| p.stat())?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::BraceClose),
            "}",
        )?;

        Ok(StmtCompound {
            lbrace_loc: lbrace.loc,
            stmts,
        })
    }

    fn var_decl_stmt(&mut self) -> Result<StmtVarDecl, ParseError> {
        let typ = self.typ()?;
        let (name, name_loc) = self.satisfy(|token| match &token.payload {
            TokenPayload::Ident(name) => Ok((name.clone(), token.loc.clone())),
            _ => Err(ParseErrorPayload::UnexpectedToken {
                expected: "identifier".to_string(),
            }),
        })?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::Semicolon),
            ";",
        )?;
        Ok(StmtVarDecl {
            typ,
            name,
            name_loc,
        })
    }

    fn if_stmt(&mut self) -> Result<StmtIf, ParseError> {
        let if_ = self.satisfy_(|token| matches!(token.payload, TokenPayload::If), "if")?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::ParenOpen),
            "(",
        )?;
        let cond = self.expr()?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::ParenClose),
            ")",
        )?;
        let then = self.stat()?;
        let else_ = self.optional(|p| {
            p.satisfy_(|token| matches!(token.payload, TokenPayload::Else), "else")?;
            p.stat()
        })?;
        Ok(StmtIf {
            if_loc: if_.loc,
            cond,
            then: Box::new(then),
            else_: else_.map(Box::new),
        })
    }

    fn while_stmt(&mut self) -> Result<StmtWhile, ParseError> {
        let while_ = self.satisfy_(
            |token| matches!(token.payload, TokenPayload::While),
            "while",
        )?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::ParenOpen),
            "(",
        )?;
        let cond = self.expr()?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::ParenClose),
            ")",
        )?;
        let body = self.stat()?;
        Ok(StmtWhile {
            while_loc: while_.loc,
            cond,
            body: Box::new(body),
        })
    }

    fn for_stmt(&mut self) -> Result<StmtFor, ParseError> {
        let for_ = self.satisfy_(|token| matches!(token.payload, TokenPayload::For), "for")?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::ParenOpen),
            "(",
        )?;
        let init = self.optional(|p| p.expr())?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::Semicolon),
            ";",
        )?;
        let cond = self.optional(|p| p.expr())?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::Semicolon),
            ";",
        )?;
        let step = self.optional(|p| p.expr())?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::ParenClose),
            ")",
        )?;
        let body = self.stat()?;
        Ok(StmtFor {
            for_loc: for_.loc,
            init,
            cond,
            step,
            body: Box::new(body),
        })
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
        let typ = self.typ()?;

        let (name, name_loc) = self.satisfy(|token| match &token.payload {
            TokenPayload::Ident(name) => Ok((name.clone(), token.loc.clone())),
            _ => Err(ParseErrorPayload::UnexpectedToken {
                expected: "identifier".to_string(),
            }),
        })?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::ParenOpen),
            "(",
        )?;
        let params = self.sep_by(
            |p| {
                let typ = p.typ()?;
                let (name, name_loc) = p.satisfy(|token| match &token.payload {
                    TokenPayload::Ident(name) => Ok((name.clone(), token.loc.clone())),
                    _ => Err(ParseErrorPayload::UnexpectedToken {
                        expected: "identifier".to_string(),
                    }),
                })?;
                Ok(DeclParam {
                    typ,
                    name,
                    name_loc,
                })
            },
            |p| {
                p.satisfy_(|token| matches!(token.payload, TokenPayload::Comma), ",")?;
                Ok(())
            },
        )?;
        self.satisfy_(
            |token| matches!(token.payload, TokenPayload::ParenClose),
            ")",
        )?;
        let stmts = self.compound_stmt()?;
        Ok(DeclFunc {
            name,
            name_loc,
            params,
            body: stmts,
            typ,
        })
    }

    fn typ(&mut self) -> Result<Type, ParseError> {
        let int = self.satisfy_(|token| matches!(token.payload, TokenPayload::Int), "int")?;
        Ok(Type::Int(TypeInt { int_loc: int.loc }))
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let decls = self.many(|p| p.decl())?;
        self.satisfy_(|token| matches!(token.payload, TokenPayload::EOF), "EOF")?;
        Ok(Program { decls })
    }
}
