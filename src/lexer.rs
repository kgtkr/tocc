use crate::pos::Pos;
use crate::token::Token;
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
    filename: String,
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(filename: String, input: String) -> Lexer {
        Lexer {
            filename,
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        let c = self.input.get(self.pos).copied()?;
        self.pos += 1;
        Some(c)
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        let c = self.next().ok_or(LexerError::UnexpectedEOF)?;
    }
}
