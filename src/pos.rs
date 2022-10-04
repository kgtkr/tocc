use std::fmt;

#[derive(Debug, Clone)]
pub struct Pos {
    pub filename: String,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.line, self.col)
    }
}
