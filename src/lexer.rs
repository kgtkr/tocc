use crate::loc::Loc;
use crate::token::{Token, TokenPayload};
use guard::guard;
use std::fmt;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum LexerError {
    #[error("{loc} - invalid character: {c}")]
    InvalidCharacter { loc: Loc, c: char },
    #[error("unexpected end of file")]
    UnexpectedEOF,
}

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,
    idx: usize,
    loc: Loc,
}

impl Lexer {
    pub fn new(filename: String, input: String) -> Lexer {
        Lexer {
            input: input.chars().collect(),
            idx: 0,
            loc: Loc {
                filename,
                line: 1,
                col: 1,
            },
        }
    }

    fn inc_loc(&mut self) {
        if self.input[self.idx] == '\n' {
            self.loc.line += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }

        self.idx += 1;
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.idx).copied()
    }

    fn expect(&mut self, f: impl Fn(char) -> bool) -> Result<char, LexerError> {
        guard!(let Some(c) = self.peek() else {
            return Err(LexerError::UnexpectedEOF);
        });
        if f(c) {
            self.inc_loc();
            Ok(c)
        } else {
            Err(LexerError::InvalidCharacter {
                loc: self.loc.clone(),
                c,
            })
        }
    }

    fn next(&mut self) -> Result<char, LexerError> {
        self.expect(|_| true)
    }

    fn expect_char(&mut self, c: char) -> Result<(), LexerError> {
        self.expect(|x| x == c).map(|_| ())
    }

    fn next_token_opt(&mut self) -> Result<Option<Token>, LexerError> {
        let begin_loc = self.loc.clone();
        let c = match self.next() {
            Ok(c) => c,
            Err(LexerError::UnexpectedEOF) => {
                return Ok(Some(Token {
                    payload: TokenPayload::EOF,
                    loc: begin_loc,
                }))
            }
            Err(e) => return Err(e),
        };

        let payload = match c {
            '0'..='9' => {
                let mut num = c.to_digit(10).unwrap() as i64;
                while let Ok(c) = self.expect(|c| c.is_digit(10)) {
                    num = num * 10 + c.to_digit(10).unwrap() as i64; // TODO: overflow check
                }
                Some(TokenPayload::IntLit(num))
            }
            ' ' | '\t' | '\r' | '\n' => None,
            _ => return Err(LexerError::InvalidCharacter { loc: begin_loc, c }),
        };

        Ok(payload.map(|payload| Token {
            payload,
            loc: begin_loc,
        }))
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token_opt()? {
            tokens.push(token);
        }
        Ok(tokens)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.payload {
            TokenPayload::IntLit(n) => write!(f, "{}", n),
            TokenPayload::EOF => write!(f, "EOF"),
        }
    }
}
