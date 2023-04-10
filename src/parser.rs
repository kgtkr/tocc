use crate::clang::{
    BinOp, Decl, DeclFunc, DeclParam, Expr, ExprAddr, ExprBinOp, ExprCall, ExprIntLit, ExprLValue,
    ExprNeg, LValueDeref, LValueVar, Program, Stmt, StmtCompound, StmtExpr, StmtFor, StmtIf,
    StmtReturn, StmtVarDecl, StmtWhile, Type, TypeInt, TypePtr,
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
    expr_count: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            idx: 0,
            expr_count: 0,
        }
    }

    fn gen_expr_id(&mut self) -> usize {
        let id = self.expr_count;
        self.expr_count += 1;
        id
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
            |p| {
                let (value, value_loc) = p.satisfy(|token| match token.payload {
                    TokenPayload::IntLit(i) => Ok((i, token.loc.clone())),
                    _ => Err(ParseErrorPayload::UnexpectedToken {
                        expected: "int literal".to_string(),
                    }),
                })?;
                Ok(Expr::IntLit(ExprIntLit {
                    value,
                    value_loc,
                    id: p.gen_expr_id(),
                }))
            },
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
                let (ident, ident_loc) = p.satisfy(|token| match &token.payload {
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
                        Ok(Expr::Call(ExprCall {
                            ident: ident.clone(),
                            ident_loc: ident_loc.clone(),
                            args,
                            id: p.gen_expr_id(),
                        }))
                    },
                    |p| {
                        Ok(Expr::LValue(ExprLValue::Var(LValueVar {
                            ident: ident.clone(),
                            ident_loc: ident_loc.clone(),
                            id: p.gen_expr_id(),
                        })))
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
                Ok(Expr::Neg(ExprNeg {
                    minus_loc: minus.loc,
                    expr: Box::new(expr),
                    id: p.gen_expr_id(),
                }))
            },
            |p| {
                let amp = p.satisfy_(|token| matches!(token.payload, TokenPayload::Amp), "&")?;
                let expr = p.primary()?;
                Ok(Expr::Addr(ExprAddr {
                    amp_loc: amp.loc,
                    expr: Box::new(expr),
                    id: p.gen_expr_id(),
                }))
            },
            |p| {
                let star =
                    p.satisfy_(|token| matches!(token.payload, TokenPayload::Asterisk), "*")?;
                let expr = p.primary()?;
                Ok(Expr::LValue(ExprLValue::Deref(LValueDeref {
                    star_loc: star.loc,
                    expr: Box::new(expr),
                    id: p.gen_expr_id(),
                })))
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
                        let op_token = p.satisfy_(
                            |token| matches!(token.payload, TokenPayload::Asterisk),
                            "*",
                        )?;
                        let rhs = p.unary()?;
                        Ok((Op::Mul, op_token.loc, rhs, p.gen_expr_id()))
                    },
                    |p| {
                        let op_token =
                            p.satisfy_(|token| matches!(token.payload, TokenPayload::Slash), "/")?;
                        let rhs = p.unary()?;
                        Ok((Op::Div, op_token.loc, rhs, p.gen_expr_id()))
                    },
                )
            },
            expr,
            |expr, (op, op_loc, rhs, expr_id)| match op {
                Op::Mul => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Mul,
                    op_loc,
                    id: expr_id,
                }),
                Op::Div => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Div,
                    op_loc,
                    id: expr_id,
                }),
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
                        let op_token =
                            p.satisfy_(|token| matches!(token.payload, TokenPayload::Plus), "+")?;
                        let rhs = p.muldiv()?;
                        Ok((Op::Add, op_token.loc, rhs, p.gen_expr_id()))
                    },
                    |p| {
                        let op_token =
                            p.satisfy_(|token| matches!(token.payload, TokenPayload::Minus), "-")?;
                        let rhs = p.muldiv()?;
                        Ok((Op::Sub, op_token.loc, rhs, p.gen_expr_id()))
                    },
                )
            },
            expr,
            |expr, (op, op_loc, rhs, expr_id)| match op {
                Op::Add => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Add,
                    op_loc,
                    id: expr_id,
                }),
                Op::Sub => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Sub,
                    op_loc,
                    id: expr_id,
                }),
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
                        let op_token =
                            p.satisfy_(|token| matches!(token.payload, TokenPayload::Lt), "<")?;
                        let rhs = p.addsub()?;
                        Ok((Op::Lt, op_token.loc, rhs, p.gen_expr_id()))
                    },
                    |p| {
                        let op_token =
                            p.satisfy_(|token| matches!(token.payload, TokenPayload::Le), "<=")?;
                        let rhs = p.addsub()?;
                        Ok((Op::Le, op_token.loc, rhs, p.gen_expr_id()))
                    },
                    |p| {
                        let op_token =
                            p.satisfy_(|token| matches!(token.payload, TokenPayload::Gt), ">")?;
                        let rhs = p.addsub()?;
                        Ok((Op::Gt, op_token.loc, rhs, p.gen_expr_id()))
                    },
                    |p| {
                        let op_token =
                            p.satisfy_(|token| matches!(token.payload, TokenPayload::Ge), ">=")?;
                        let rhs = p.addsub()?;
                        Ok((Op::Ge, op_token.loc, rhs, p.gen_expr_id()))
                    },
                )
            },
            expr,
            |expr, (op, op_loc, rhs, expr_id)| match op {
                Op::Lt => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Lt,
                    op_loc,
                    id: expr_id,
                }),
                Op::Le => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Le,
                    op_loc,
                    id: expr_id,
                }),
                Op::Gt => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Gt,
                    op_loc,
                    id: expr_id,
                }),
                Op::Ge => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Ge,
                    op_loc,
                    id: expr_id,
                }),
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
                        let op_token =
                            p.satisfy_(|token| matches!(token.payload, TokenPayload::EqEq), "==")?;
                        let rhs = p.relational()?;
                        Ok((Op::Eq, op_token.loc, rhs, p.gen_expr_id()))
                    },
                    |p| {
                        let op_token =
                            p.satisfy_(|token| matches!(token.payload, TokenPayload::Neq), "!=")?;
                        let rhs = p.relational()?;
                        Ok((Op::Ne, op_token.loc, rhs, p.gen_expr_id()))
                    },
                )
            },
            expr,
            |expr, (op, op_loc, rhs, expr_id)| match op {
                Op::Eq => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Eq,
                    op_loc,
                    id: expr_id,
                }),
                Op::Ne => Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: BinOp::Ne,
                    op_loc,
                    id: expr_id,
                }),
            },
        )
    }

    fn assign(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality()?;
        parser_or!(
            self,
            |p| {
                let op_token =
                    p.satisfy_(|token| matches!(token.payload, TokenPayload::Eq), "=")?;
                let rhs = p.assign()?;
                Ok(Expr::BinOp(ExprBinOp {
                    lhs: Box::new(expr.clone()),
                    rhs: Box::new(rhs),
                    op: BinOp::Assign,
                    op_loc: op_token.loc,
                    id: p.gen_expr_id(),
                }))
            },
            |_| Ok(expr.clone()),
        )
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        let expr = self.assign()?;
        Ok(expr)
    }

    fn stat(&mut self) -> Result<Stmt, ParseError> {
        parser_or!(
            self,
            |p| p.return_stmt().map(Stmt::Return),
            |p| p.compound_stmt().map(Stmt::Compound),
            |p| p.expr_stmt().map(Stmt::Expr),
            |p| p.var_decl_stmt().map(Stmt::VarDecl),
            |p| p.if_stmt().map(Stmt::If),
            |p| p.while_stmt().map(Stmt::While),
            |p| p.for_stmt().map(Stmt::For),
        )
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
        let (ident, ident_loc) = self.satisfy(|token| match &token.payload {
            TokenPayload::Ident(ident) => Ok((ident.clone(), token.loc.clone())),
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
            ident,
            ident_loc,
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
        self.func_decl().map(Decl::Func)
    }

    fn func_decl(&mut self) -> Result<DeclFunc, ParseError> {
        let typ = self.typ()?;

        let (ident, ident_loc) = self.satisfy(|token| match &token.payload {
            TokenPayload::Ident(ident) => Ok((ident.clone(), token.loc.clone())),
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
                let (ident, ident_loc) = p.satisfy(|token| match &token.payload {
                    TokenPayload::Ident(ident) => Ok((ident.clone(), token.loc.clone())),
                    _ => Err(ParseErrorPayload::UnexpectedToken {
                        expected: "identifier".to_string(),
                    }),
                })?;
                Ok(DeclParam {
                    typ,
                    ident,
                    ident_loc,
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
            ident,
            ident_loc,
            params,
            body: stmts,
            typ,
        })
    }

    fn typ_primary(&mut self) -> Result<Type, ParseError> {
        let int = self.satisfy_(|token| matches!(token.payload, TokenPayload::Int), "int")?;

        Ok(Type::Int(TypeInt { int_loc: int.loc }))
    }

    fn typ_suffix_unary(&mut self) -> Result<Type, ParseError> {
        let typ = self.typ_primary()?;
        self.fold(
            |p| p.satisfy_(|token| matches!(token.payload, TokenPayload::Asterisk), "*"),
            typ,
            |typ, _| Type::Ptr(TypePtr { typ: Box::new(typ) }),
        )
    }

    fn typ(&mut self) -> Result<Type, ParseError> {
        self.typ_suffix_unary()
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let decls = self.many(|p| p.decl())?;
        self.satisfy_(|token| matches!(token.payload, TokenPayload::EOF), "EOF")?;
        Ok(Program { decls })
    }
}
