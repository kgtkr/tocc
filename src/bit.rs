#[derive(Debug, Copy, Clone)]
pub enum Bit {
    Bit8,
    Bit16,
    Bit32,
    Bit64,
}

impl Bit {
    pub fn to_size(self) -> usize {
        match self {
            Self::Bit8 => 1,
            Self::Bit16 => 2,
            Self::Bit32 => 4,
            Self::Bit64 => 8,
        }
    }
}
