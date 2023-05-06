use std::{fmt, rc::Rc};

#[derive(Debug, Clone)]
pub struct Loc {
    pub filename: Rc<String>,
    pub line: usize,
    pub col: usize,
}

impl Loc {
    pub fn dummy() -> Loc {
        Loc {
            filename: Rc::new(String::new()),
            line: 0,
            col: 0,
        }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.line, self.col)
    }
}

pub trait Locatable {
    fn loc(&self) -> &Loc;
}
