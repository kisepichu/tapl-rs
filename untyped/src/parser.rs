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
    sequence::{delimited, preceded},
};

fn with_pos<'a, F, O>(
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Spanned<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = O, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        parser.parse(i).map(|(rest, v)| {
            let spanned = Spanned {
                v,
                start: i.location_offset(),
                line: i.location_line(),
                column: i.get_utf8_column(),
            };
            (rest, spanned)
        })
    }
}

fn update_err_pos<'a, F, O>(
    mut parser: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Spanned<O>, ErrorWithPos>
where
    F: Parser<Span<'a>, Output = Spanned<O>, Error = ErrorWithPos>,
{
    move |i: Span<'a>| {
        parser
            .parse(i)
            .map(|(rest, sp)| {
                let sp = Spanned {
                    v: sp.v,
                    start: i.location_offset(),
                    line: i.location_line(),
                    column: i.get_utf8_column(),
                };
                (rest, sp)
            })
            .map_err(|e| match e {
                nom::Err::Error(e) => {
                    println!(
                        "update_err_pos: {}, i: ({}, {}), e: ({}, {})",
                        i,
                        i.location_line(),
                        i.get_utf8_column(),
                        e.line,
                        e.column
                    );
                    let e_ = ErrorWithPos {
                        message: e.message,
                        line: i.location_line(),
                        column: i.get_utf8_column(),
                        input: i.to_string(),
                        kind: e.kind,
                    };
                    nom::Err::Error(e_)
                }
                _ => {
                    let e = ErrorWithPos {
                        message: "Parse Error".to_string(),
                        line: i.location_line(),
                        column: i.get_utf8_column(),
                        input: i.to_string(),
                        kind: None,
                    };
                    nom::Err::Error(e)
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
fn parse_var(i: Span) -> IResult<Span, Spanned<Term>, ErrorWithPos> {
    let (_i, _check) = alphanumeric1.parse(i)?;
    update_err_pos(with_pos(map_res(digit1, |s: Span| {
        s.fragment().parse::<usize>().map(Term::Var)
    })))
    .parse(i)
}

/// <abs> ::= "\" <term>
fn parse_abs(i: Span) -> IResult<Span, Spanned<Term>, ErrorWithPos> {
    with_pos(map(preceded(char('\\'), parse_term), |t| {
        Term::Abs(Box::new(t))
    }))
    .parse(i)
}

/// <encl> ::= "(" <term> ")"
fn parse_encl(i: Span) -> IResult<Span, Spanned<Term>, ErrorWithPos> {
    let (i, _) = char('(').parse(i)?;
    let r = parse_term_space.parse(i);
    println!("parse_encl r: {:?}, i: {}", r, i);
    let (i, t) = r?;
    let (i, _) = char(')').parse(i)?;
    Ok((i, t))
    // delimited(char('('), parse_term_space, char(')')).parse(i)
}

/// <atom> ::= <encl> | <abs> | <var>
fn parse_atom(i: Span) -> IResult<Span, Spanned<Term>, ErrorWithPos> {
    let r = preceded(multispace0, alt((parse_encl, parse_abs, parse_var))).parse(i);
    println!("parse_atom r: {:?}, i:{}", r, i);
    r
}

/// <app> ::= <atom> <atom> ...
fn parse_app(i: Span) -> IResult<Span, Spanned<Term>, ErrorWithPos> {
    println!("parse_app head{{-------------{{");
    let r = parse_atom(i);
    println!("parse_app head}}-------------}}: r: {:?}, i:{}", r, i);
    let (i, head) = r?;
    println!("parse_app tail{{-------------{{");
    let r = many0(parse_atom).parse(i); // many0 のためエラーが消される
    println!("parse_app tail}}-------------}}: r: {:?}, i:{}", r, i);
    let (i, tail) = r?;

    let result = tail.into_iter().fold(head, |acc, arg| Spanned {
        start: acc.start,
        line: acc.line,
        column: acc.column,
        v: Term::App(Box::new(acc), Box::new(arg)),
    });

    Ok((i, result))
}
// fn parse_app(i: Span) -> IResult<Span, Spanned<Term>, ErrorWithPos> {
//     let (i, head) = parse_atom(i)?;
//     let (i, tail) = many0(parse_atom).parse(i)?;

//     let result = tail.into_iter().fold(head, |acc, arg| Spanned {
//         start: acc.start,
//         line: acc.line,
//         column: acc.column,
//         v: Term::App(Box::new(acc), Box::new(arg)),
//     });

//     Ok((i, result))
// }

/// <term> ::= <app>
fn parse_term(i: Span) -> IResult<Span, Spanned<Term>, ErrorWithPos> {
    preceded(multispace0, parse_app).parse(i)
}

fn parse_term_space(i: Span) -> IResult<Span, Spanned<Term>, ErrorWithPos> {
    let r = parse_term.parse(i);
    println!("parse_term_space: r: {:?}, i:{}", r, i);
    let (i, t) = r?;
    let (i, _) = multispace0.parse(i)?;
    Ok((i, t))
}
// fn parse_term_space(i: Span) -> IResult<Span, Spanned<Term>, ErrorWithPos> {
//     let (i, t) = parse_term.parse(i)?;
//     let (i, _) = multispace0.parse(i)?;
//     Ok((i, t))
// }

pub fn parse(input: &str) -> Result<Term, String> {
    let (rest, t) = parse_term_space(Span::new(input)).map_err(|e| {
        if let nom::Err::Error(e) = e {
            e.to_string()
        } else {
            "Unknown parsing error".to_string()
        }
    })?;
    if rest.is_empty() {
        Ok(t.v)
    } else {
        Err(format!(
            "Parse error at line {}, column {}: '{}'\n{}",
            rest.location_line(),
            rest.get_utf8_column(),
            rest.fragment(),
            "input not fully consumed"
        ))
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
    #[case(r"\", Err("Parse error at line 1, column 1: '\\'\nerror Digit at: \\".to_string()))]
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
            Err("Parse error at line 8, column 9: 'a'".to_string())
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
