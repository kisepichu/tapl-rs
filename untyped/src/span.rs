use nom::error::ParseError;
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub v: T,
    pub start: usize,
    pub line: u32,
    pub column: usize,
}

#[derive(Debug, PartialEq)]
pub struct ErrorWithPos {
    pub message: String,
    pub input: String,
    pub kind: Option<nom::error::ErrorKind>,
    pub line: u32,
    pub column: usize,
}

impl<'a> ParseError<Span<'a>> for ErrorWithPos {
    fn from_error_kind(input: Span<'a>, kind: nom::error::ErrorKind) -> Self {
        Self {
            message: "Parsing failed".to_string(),
            input: input.to_string(),
            kind: Some(kind),
            line: input.location_line(),
            column: input.get_utf8_column(),
        }
    }

    fn append(_input: Span<'a>, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn or(self, other: Self) -> Self {
        // println!(
        //     "other: ({}, {}), self: ({}, {}), c: {}",
        //     other.line,
        //     other.column,
        //     self.line,
        //     self.column,
        //     (other.line, other.column) > (self.line, self.column)
        // );
        if (other.line, other.column) > (self.line, self.column) {
            other
        } else {
            self
        }
    }
}

// pub fn parse<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>, ErrorWithPos> {
//     Err(nom::Err::Error(ErrorWithPos {
//         message: "Parse Error".to_string(),
//         line: i.location_line(),
//         column: i.get_utf8_column(),
//     }))
// }

impl<E: std::fmt::Display> nom::error::FromExternalError<Span<'_>, E> for ErrorWithPos {
    fn from_external_error(input: Span, kind: nom::error::ErrorKind, e: E) -> Self {
        ErrorWithPos {
            message: format!("External error: {}", e),
            input: input.to_string(),
            kind: Some(kind),
            line: input.location_line(),
            column: input.get_utf8_column(),
        }
    }
}

impl std::fmt::Display for ErrorWithPos {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(k) = self.kind {
            write!(
                f,
                "{}, kind={:?}. '{}' at line {}, column {}",
                self.message, k, self.input, self.line, self.column
            )
        } else {
            write!(
                f,
                "{}: '{}' at line {}, column {}",
                self.message, self.input, self.line, self.column
            )
        }
    }
}

pub fn spanned<T, U>(sp: &Spanned<T>, v: U) -> Spanned<U> {
    Spanned {
        v,
        start: sp.start,
        line: sp.line,
        column: sp.column,
    }
}

pub fn spanned_res<T, U>(
    sp: &Spanned<T>,
    r: Result<U, String>,
) -> Result<Spanned<U>, ErrorWithPos> {
    match r {
        Ok(v) => Ok(Spanned {
            v,
            start: sp.start,
            line: sp.line,
            column: sp.column,
        }),
        Err(e) => Err(ErrorWithPos {
            message: e,
            line: sp.line,
            column: sp.column,
            input: "".to_string(),
            kind: None,
        }),
    }
}

pub fn map_spanned<T, U>(
    input: &Spanned<T>,
    f: impl FnOnce(&T) -> Result<U, ErrorWithPos>,
) -> Result<Spanned<U>, ErrorWithPos> {
    let v = f(&input.v)?;
    Ok(Spanned {
        v,
        start: input.start,
        line: input.line,
        column: input.column,
    })
}
