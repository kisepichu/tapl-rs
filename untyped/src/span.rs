use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub v: T,
    pub start: usize,
    pub line: u32,
    pub column: usize,
}

#[derive(Debug)]
pub struct ErrorWithPos {
    pub message: String,
    pub line: u32,
    pub column: usize,
}

impl std::fmt::Display for ErrorWithPos {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} at line {}, column {}",
            self.message, self.line, self.column
        )
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
