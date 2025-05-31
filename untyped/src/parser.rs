use crate::{
    span::{ErrorWithPos, Span, Spanned},
    syntax::Term,
};
use nom::{
    IResult, Parser,
    branch::alt,
    character::complete::{alphanumeric1, char, digit1, multispace0},
    combinator::{map, map_res},
    multi::many0,
    sequence::preceded,
};

struct Prg<T> {
    st: Spanned<T>,
    lasterr: Option<ErrorWithPos>,
}

fn with_pos<'a, F, O>(
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Prg<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = O, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        parser.parse(i).map(|(rest, v)| {
            let st = Spanned {
                v,
                start: i.location_offset(),
                line: i.location_line(),
                column: i.get_utf8_column(),
            };
            (rest, Prg { st, lasterr: None })
        })
    }
}

fn update_err_pos<'a, F, O>(
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Prg<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = Prg<O>, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        parser
            .parse(i)
            .map(|(rest, sp)| {
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
            .map_err(|e| match e {
                nom::Err::Error(e) => {
                    let e_ = ErrorWithPos {
                        message: e.message,
                        line: i.location_line(),
                        column: i.get_utf8_column(),
                        kind: e.kind,
                    };
                    nom::Err::Error(e_)
                }
                _ => {
                    let e = ErrorWithPos {
                        message: "Parse Error".to_string(),
                        line: i.location_line(),
                        column: i.get_utf8_column(),
                        kind: None,
                    };
                    nom::Err::Error(e)
                }
            })
    }
}

fn chmax_err<'a, F, O>(
    lasterr: &Option<ErrorWithPos>,
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Prg<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = Prg<O>, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        parser.parse(i).map_err(|e| match e {
            nom::Err::Error(e) => {
                let e_ = ErrorWithPos {
                    message: e.message,
                    line: i.location_line(),
                    column: i.get_utf8_column(),
                    kind: e.kind,
                };
                let mx = if let Some(lasterr) = lasterr.clone() {
                    std::cmp::max(e_, lasterr)
                } else {
                    e_
                };
                nom::Err::Error(mx)
            }
            _ => {
                let e = ErrorWithPos {
                    message: "Parse Error".to_string(),
                    line: i.location_line(),
                    column: i.get_utf8_column(),
                    kind: None,
                };
                let mx = if let Some(lasterr) = lasterr.clone() {
                    std::cmp::max(e, lasterr)
                } else {
                    e
                };
                nom::Err::Error(mx)
            }
        })
    }
}

// <term> ::= <app>
// <app> ::= <atom> <app> | <atom>
// <atom> ::= <encl> | <abs> | <var>
// <encl> ::= "(" <term> ")"
// <abs> ::= "\" <term>
// <var> ::= number

/// <var> ::= number
fn parse_var(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (_i, _check) = alphanumeric1.parse(i)?;
    update_err_pos(with_pos(map_res(digit1, |s: Span| {
        s.fragment().parse::<usize>().map(Term::Var)
    })))
    .parse(i)
}

/// <abs> ::= "\" <term>
fn parse_abs(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    with_pos(map(preceded(char('\\'), parse_term), |t| {
        Term::Abs(Box::new(t.st))
    }))
    .parse(i)
}

/// <encl> ::= "(" <term> ")"
fn parse_encl(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (i, _) = char('(').parse(i)?;
    let (i, t) = parse_term_space.parse(i)?;
    let (i, _) = chmax_err(&t.lasterr, with_pos(char(')'))).parse(i)?;
    Ok((i, t))
    // delimited(char('('), parse_term_space, char(')')).parse(i)
}

/// <atom> ::= <encl> | <abs> | <var>
fn parse_atom(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    preceded(multispace0, alt((parse_encl, parse_abs, parse_var))).parse(i)
}

/// <app> ::= <atom> <app> | <atom>
fn parse_app(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (i, head) = parse_atom(i)?;
    let (i, tail) = many0(parse_atom).parse(i)?;
    if let Err(lasterr) = parse_atom.parse(i) {
        match lasterr {
            nom::Err::Error(lasterr) => {
                let st = tail.into_iter().fold(head.st, |acc, arg| Spanned {
                    start: acc.start,
                    line: acc.line,
                    column: acc.column,
                    v: Term::App(Box::new(acc), Box::new(arg.st)),
                });
                Ok((
                    i,
                    Prg {
                        st,
                        lasterr: Some(lasterr),
                    },
                ))
            }
            e => Err(nom::Err::Error(ErrorWithPos {
                message: format!("internal error: unknown error: {}", e),
                kind: None,
                line: i.location_line(),
                column: i.get_utf8_column(),
            })),
        }
    } else {
        Err(nom::Err::Error(ErrorWithPos {
            message: "internal error: many0 did not take it to the end".to_string(),
            kind: None,
            line: i.location_line(),
            column: i.get_utf8_column(),
        }))
    }
}

/// <term> ::= <app>
fn parse_term(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    preceded(multispace0, parse_app).parse(i)
}

fn parse_term_space(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (i, t) = parse_term.parse(i)?;
    let (i, _) = multispace0.parse(i)?;
    Ok((i, t))
}

pub fn display_position(input: &str, line: u32, column: usize) {
    println!();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        println!("\n^");
        return;
    }
    if line as usize > lines.len() {
        println!("error while displaying position");
        return;
    }
    let target_line = lines[line as usize - 1];
    if target_line.is_empty() {
        println!("{}\n^", target_line);
        return;
    }
    if column > target_line.len() + 1 {
        println!("error while displaying position");
        return;
    }
    println!("{}", target_line);
    println!("{:>width$}^", "", width = column - 1);
}

pub fn parse(input: &str) -> Result<Term, String> {
    let (rest, t) = parse_term_space(Span::new(input)).map_err(|e| {
        if let nom::Err::Error(e) = e {
            display_position(input, e.line, e.column);
            e.to_string()
        } else {
            "Unknown parsing error".to_string()
        }
    })?;
    if rest.is_empty() {
        Ok(t.st.v)
    } else {
        let e = ErrorWithPos {
            message: "Parse failed, input not fully consumed".to_string(),
            kind: None,
            line: rest.location_line(),
            column: rest.get_utf8_column(),
        };
        display_position(input, e.line, e.column);

        Err(e.to_string())
    }
}

mod test {
    use rstest::rstest;

    use crate::{span::Spanned, syntax::Term};

    // dummy Spanned Term for testing
    #[allow(unused)]
    fn b(t: Term) -> Box<Spanned<Term>> {
        Box::new(Spanned {
            v: t,
            start: 0,
            line: 0,
            column: 0,
        })
    }

    #[allow(unused)]
    fn term_eq(t: &Term, u: &Term) -> bool {
        match (t, u) {
            (Term::Var(i), Term::Var(j)) => i == j,
            (Term::Abs(t1), Term::Abs(u1)) => term_eq(&t1.v, &u1.v),
            (Term::App(t1, t2), Term::App(u1, u2)) => {
                term_eq(&t1.v, &u1.v) && term_eq(&t2.v, &u2.v)
            }
            _ => false,
        }
    }

    #[allow(unused)]
    fn term_eq_res(t: &Result<Term, String>, u: &Result<Term, String>) -> bool {
        match (t, u) {
            (Ok(t), Ok(u)) => term_eq(t, u),
            (Err(et), Err(eu)) => et == eu,
            _ => false,
        }
    }

    #[rstest]
    #[case("1", Ok(Term::Var(1)))]
    #[case(r"\0 ", Ok(Term::Abs(b(Term::Var(0)))))]
    #[case(r"(\0 )", Ok(Term::Abs(b(Term::Var(0)))))]
    #[case(
        r"( \0) 1",
        Ok(Term::App(b(Term::Abs(b(Term::Var(0)))), b(Term::Var(1))))
    )]
    #[case(
        r"(\0) (\0)",
        Ok(Term::App(b(Term::Abs(b(Term::Var(0)))), b(Term::Abs(b(Term::Var(0))))))
    )]
    #[case(
        r"(\0) (\0) (\0)",
        Ok(Term::App(
            b(Term::App(b(Term::Abs(b(Term::Var(0)))), b(Term::Abs(b(Term::Var(0)))))),
            b(Term::Abs(b(Term::Var(0))))
        ))
    )]
    #[case(
        r"\\1\0",
        Ok(Term::Abs(b(Term::Abs(b(Term::App(
            b(Term::Var(1)),
            b(Term::Abs(b(Term::Var(0))))
        ))))))
    )]
    #[case(
        r"\\1\(0)",
        Ok(Term::Abs(b(Term::Abs(b(Term::App(
            b(Term::Var(1)),
            b(Term::Abs(b(Term::Var(0))))
        ))))))
    )]
    #[case(
        r"\\1(\0)",
        Ok(Term::Abs(b(Term::Abs(b(Term::App(
            b(Term::Var(1)),
            b(Term::Abs(b(Term::Var(0))))
        ))))))
    )]
    #[case(
        r"\\(1\0)",
        Ok(Term::Abs(b(Term::Abs(b(Term::App(
            b(Term::Var(1)),
            b(Term::Abs(b(Term::Var(0))))
        ))))))
    )]
    #[case(
        r"\(\1\0)",
        Ok(Term::Abs(b(Term::Abs(b(Term::App(
            b(Term::Var(1)),
            b(Term::Abs(b(Term::Var(0))))
        ))))))
    )]
    #[case(
        r"(\\1\0)",
        Ok(Term::Abs(b(Term::Abs(b(Term::App(
            b(Term::Var(1)),
            b(Term::Abs(b(Term::Var(0))))
        ))))))
    )]
    #[case(
        r"\\(1)\0",
        Ok(Term::Abs(b(Term::Abs(b(Term::App(
            b(Term::Var(1)),
            b(Term::Abs(b(Term::Var(0))))
        ))))))
    )]
    #[case(
        r"\(\1)\0",
        Ok(Term::Abs(b(Term::App(
            b(Term::Abs(b(Term::Var(1)))),
            b(Term::Abs(b(Term::Var(0))))
        ))))
    )]
    #[case(
        r"(\\1)\0",
        Ok(Term::App(
            b(Term::Abs(b(Term::Abs(b(Term::Var(1)))))),
            b(Term::Abs(b(Term::Var(0))))
        ))
    )]
    #[case(r"\", Err("Parsing failed at line 1, column 2: kind=Char".to_string()))]
    #[case(
        r"
\\\
(
  (
    0
  )
  (
    2(1 a)
  )
)
        ",
        Err("Parsing failed at line 8, column 9: kind=Char".to_string())
    )]
    #[case(
        r"((()(())))",
        Err("Parsing failed at line 1, column 4: kind=Char".to_string())
    )]
    fn test_parse_term(#[case] input: &str, #[case] expected: Result<Term, String>) {
        use crate::parser::parse;
        println!("input: {input}");
        assert!(
            term_eq_res(&parse(input), &expected),
            "Assertion failed:\ninput: {input}\nexpected: {:?}\nactual: {:?}",
            expected,
            parse(input),
        );
    }
}
