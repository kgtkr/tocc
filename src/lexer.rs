use crate::pos::Pos;
use crate::token::{Token, TokenPayload};
use guard::guard;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum LexerError {
    #[error("{pos} - invalid character: {c}")]
    InvalidCharacter { pos: Pos, c: char },
    #[error("unexpected end of file")]
    UnexpectedEOF,
}

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    virtual_pos: Pos,
}

impl Lexer {
    pub fn new(filename: String, input: String) -> Lexer {
        Lexer {
            input: input.chars().collect(),
            pos: 0,
            virtual_pos: Pos {
                filename,
                line: 1,
                col: 1,
            },
        }
    }

    fn inc_pos(&mut self) {
        if self.input[self.pos] == '\n' {
            self.virtual_pos.line += 1;
            self.virtual_pos.col = 1;
        } else {
            self.virtual_pos.col += 1;
        }

        self.pos += 1;
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn expect(&mut self, f: impl Fn(char) -> bool) -> Result<char, LexerError> {
        guard!(let Some(c) = self.peek() else {
            return Err(LexerError::UnexpectedEOF);
        });
        if f(c) {
            self.inc_pos();
            Ok(c)
        } else {
            Err(LexerError::InvalidCharacter {
                pos: self.virtual_pos.clone(),
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
        let begin_pos = self.virtual_pos.clone();
        let c = match self.next() {
            Ok(c) => c,
            Err(LexerError::UnexpectedEOF) => {
                return Ok(Some(Token {
                    payload: TokenPayload::EOF,
                    pos: begin_pos,
                }))
            }
            Err(e) => return Err(e),
        };

        let payload = match c {
            '0'..='9' => {
                let mut num = c.to_digit(10).unwrap() as i64;
                while let Ok(c) = self.expect(|c| c.is_digit(10)) {
                    num = num * 10 + c.to_digit(10).unwrap() as i64;
                }
                Some(TokenPayload::IntLit(num))
            }
            ' ' | '\t' | '\r' | '\n' => None,
            _ => return Err(LexerError::InvalidCharacter { pos: begin_pos, c }),
        };

        Ok(payload.map(|payload| Token {
            payload,
            pos: begin_pos,
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
