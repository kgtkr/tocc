use crate::loc::Loc;
use crate::token::{Token, TokenPayload};
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
        let Some(c) = self.peek() else {
            return Err(LexerError::UnexpectedEOF);
        };
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

    fn next_token_opt(&mut self) -> Result<Option<Token>, LexerError> {
        let begin_loc = self.loc.clone();
        let begin_idx = self.idx;
        let c = match self.next() {
            Ok(c) => c,
            Err(LexerError::UnexpectedEOF) => {
                return Ok(Some(Token {
                    payload: TokenPayload::EOF,
                    loc: begin_loc,
                    value: "<EOF>".to_string(),
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
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                ident.push(c);
                while let Ok(c) = self.expect(|c| c.is_ascii_alphanumeric() || c == '_') {
                    ident.push(c);
                }
                Some(match ident.as_str() {
                    "return" => TokenPayload::Return,
                    "int" => TokenPayload::Int,
                    _ => TokenPayload::Ident(ident),
                })
            }
            '(' => Some(TokenPayload::ParenOpen),
            ')' => Some(TokenPayload::ParenClose),
            '{' => Some(TokenPayload::BraceOpen),
            '}' => Some(TokenPayload::BraceClose),
            ';' => Some(TokenPayload::Semicolon),
            '+' => Some(TokenPayload::Plus),
            '-' => Some(TokenPayload::Minus),
            '*' => Some(TokenPayload::Asterisk),
            '/' => Some(TokenPayload::Slash),
            '=' => {
                if let Ok('=') = self.expect(|c| c == '=') {
                    Some(TokenPayload::EqEq)
                } else {
                    Some(TokenPayload::Eq)
                }
            }
            '!' => {
                self.expect(|c| c == '=')?;
                Some(TokenPayload::Neq)
            }
            '<' => {
                if let Ok('=') = self.expect(|c| c == '=') {
                    Some(TokenPayload::Le)
                } else {
                    Some(TokenPayload::Lt)
                }
            }
            '>' => {
                if let Ok('=') = self.expect(|c| c == '=') {
                    Some(TokenPayload::Ge)
                } else {
                    Some(TokenPayload::Gt)
                }
            }
            ' ' | '\t' | '\r' | '\n' => None,
            _ => return Err(LexerError::InvalidCharacter { loc: begin_loc, c }),
        };

        Ok(payload.map(|payload| Token {
            payload,
            loc: begin_loc,
            value: self.input[begin_idx..self.idx].iter().collect(),
        }))
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            if let Some(token) = self.next_token_opt()? {
                tokens.push(token.clone());
                if let TokenPayload::EOF = token.payload {
                    return Ok(tokens);
                }
            }
        }
    }
}
