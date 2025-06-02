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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorWithPos {
    pub message: String,
    pub level: u32,
    pub kind: Option<nom::error::ErrorKind>,
    pub line: u32,
    pub column: usize,
}

impl PartialOrd for ErrorWithPos {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ErrorWithPos {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.level, self.line, self.column, self.message.len()).cmp(&(
            other.level,
            other.line,
            other.column,
            other.message.len(),
        ))
    }
}

impl fmt::Display for ErrorWithPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(k) = self.kind {
            write!(
                f,
                "{} at line {}, column {}: kind={:?}",
                self.message, self.line, self.column, k,
            )
        } else {
            write!(
                f,
                "{} at line {}, column {}",
                self.message, self.line, self.column
            )
        }
    }
}

impl nom::error::ParseError<Span<'_>> for ErrorWithPos {
    fn from_error_kind(input: Span, kind: nom::error::ErrorKind) -> Self {
        ErrorWithPos {
            message: "unexpected token".to_string(),
            level: 10,
            kind: Some(kind),
            line: input.location_line(),
            column: input.get_utf8_column(),
        }
    }

    fn append(_input: Span, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn or(self, other: Self) -> Self {
        if self < other { other } else { self }
    }
}

impl<E: std::fmt::Display> nom::error::FromExternalError<Span<'_>, E> for ErrorWithPos {
    fn from_external_error(input: Span, kind: nom::error::ErrorKind, e: E) -> Self {
        ErrorWithPos {
            message: format!("External error: {}", e),
            level: 100,
            kind: Some(kind),
            line: input.location_line(),
            column: input.get_utf8_column(),
        }
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
