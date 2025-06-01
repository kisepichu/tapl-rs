use nom_locate::LocatedSpan;
use std::fmt;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub v: T,
    pub start: usize,
    pub line: u32,
    pub column: usize,
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.v)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorWithPos {
    pub message: String,
    pub level: u32,
    pub kind: Option<nom::error::ErrorKind>,
    pub line: u32,
    pub column: usize,
}

impl fmt::Display for ErrorWithPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl nom::error::ParseError<Span<'_>> for ErrorWithPos {
    fn from_error_kind(input: Span, kind: nom::error::ErrorKind) -> Self {
        ErrorWithPos {
            message: format!("parse error: {:?}", kind),
            level: 0,
            kind: Some(kind),
            line: input.location_line(),
            column: input.get_utf8_column(),
        }
    }

    fn append(_input: Span, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prg<T> {
    pub st: Spanned<T>,
    pub lasterr: Option<ErrorWithPos>,
}

// Helper function for creating Spanned<T> in code
pub fn dummy_spanned<T>(value: T) -> Spanned<T> {
    Spanned {
        v: value,
        start: 0,
        line: 1,
        column: 1,
    }
}
