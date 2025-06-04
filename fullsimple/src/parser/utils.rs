// todo

use nom::{IResult, Parser};

use crate::span::{ErrorWithPos, Prg, Span, Spanned};

pub fn with_pos<'a, F, O>(
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Prg<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = O, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        println!(
            "i: {}, line: {}, column: {}",
            i.fragment(),
            i.location_line(),
            i.get_utf8_column()
        );
        parser
            .parse(i)
            .map(|(rest, v)| {
                let st = Spanned {
                    v,
                    start: i.location_offset(),
                    line: i.location_line(),
                    column: i.get_utf8_column(),
                };
                (rest, Prg { st, lasterr: None })
            })
            .map_err(|e| match e {
                nom::Err::Error(e) => {
                    let e_ = ErrorWithPos {
                        message: format!("Parse error: {}", e.message),
                        level: e.level,
                        line: i.location_line(),
                        column: i.get_utf8_column(),
                        kind: e.kind,
                    };
                    println!("Error: {:?}", e_);
                    nom::Err::Error(e_)
                }
                _ => {
                    let e = ErrorWithPos {
                        message: "Unknown error".to_string(),
                        level: 100,
                        line: i.location_line(),
                        column: i.get_utf8_column(),
                        kind: None,
                    };
                    nom::Err::Error(e)
                }
            })
    }
}

#[allow(unused)]
pub fn update_pos<'a, F, O>(
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Prg<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = Prg<O>, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        parser.parse(i).map(|(rest, sp)| {
            let st = Spanned {
                v: sp.st.v,
                start: i.location_offset(),
                line: i.location_line(),
                column: i.get_utf8_column(),
            };
            (
                rest,
                Prg {
                    st,
                    lasterr: sp.lasterr,
                },
            )
        })
    }
}

pub fn update_err<'a, F, O>(
    message: &str,
    level: usize,
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Prg<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = Prg<O>, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        parser.parse(i).map_err(|e| match e {
            nom::Err::Error(e) => {
                let e_ = ErrorWithPos {
                    message: message.to_string(),
                    level: level as u32,
                    line: i.location_line(),
                    column: i.get_utf8_column(),
                    kind: e.kind,
                };

                let mx = std::cmp::max(e, e_);
                println!(
                    "---update_err: {}, line: {}, column: {}",
                    mx.message, mx.line, mx.column
                );
                nom::Err::Error(mx)
            }
            _ => {
                let e = ErrorWithPos {
                    message: message.to_string(),
                    level: level as u32,
                    line: i.location_line(),
                    column: i.get_utf8_column(),
                    kind: None,
                };
                println!(
                    "----update_err: {}, line: {}, column: {}",
                    e.message, e.line, e.column
                );
                nom::Err::Error(e)
            }
        })
    }
}

pub fn append_err<'a, F, O>(
    message: &str,
    level: usize,
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Prg<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = Prg<O>, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        parser.parse(i).map_err(|e| match e {
            nom::Err::Error(e) => {
                let e = ErrorWithPos {
                    message: format!(
                        "{} at line {}, column {}\n {}",
                        e.message, e.line, e.column, message
                    ),
                    level: level as u32,
                    line: i.location_line(),
                    column: i.get_utf8_column(),
                    kind: None,
                };
                nom::Err::Error(e)
            }
            _ => {
                let e = ErrorWithPos {
                    message: message.to_string(),
                    level: level as u32,
                    line: i.location_line(),
                    column: i.get_utf8_column(),
                    kind: None,
                };
                nom::Err::Error(e)
            }
        })
    }
}

pub fn chmax_err<'a, F, O>(
    lasterr: &Option<ErrorWithPos>,
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Prg<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = Prg<O>, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        parser.parse(i).map_err(|e| match e {
            nom::Err::Error(e) => {
                let mx = if let Some(lasterr) = lasterr.clone() {
                    std::cmp::max(e, lasterr)
                } else {
                    e
                };
                nom::Err::Error(mx)
            }
            e => {
                let ep = ErrorWithPos {
                    message: format!("unknown error {}", e),
                    level: 100,
                    line: i.location_line(),
                    column: i.get_utf8_column(),
                    kind: None,
                };
                let mx = if let Some(lasterr) = lasterr.clone() {
                    std::cmp::max(ep, lasterr)
                } else {
                    ep
                };
                nom::Err::Error(mx)
            }
        })
    }
}

#[allow(unused)]
pub fn dbg<'a, F, O: std::fmt::Debug>(
    log: &str,
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Prg<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = Prg<O>, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        let r = parser.parse(i);
        println!("{}: r: {:?}, i: {}", log, r, i);
        r
    }
}
