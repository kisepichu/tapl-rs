use crate::{
    parser::utils::{chmax_err, update_err, with_pos},
    span::{ErrorWithPos, Prg, Span, Spanned},
    syntax::{term::Term, r#type::Type},
};
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric1, char, digit1, multispace0},
    combinator::{map, map_res},
    multi::many0,
    sequence::{delimited, preceded},
};
use std::iter::once;

mod utils;

// <term> ::= <app>
// <app>  ::= <atom> <app> | <atom>
// <atom> ::= <encl> | <abs> | <var> | <true> | <false> | <if>
// <encl> ::= "(" <term> ")"
// <abs> ::= "\:" <ty> "." <term>
// <if> ::= "if" <term> "then" <term> "else" <term>
// <var> ::= number
// <true> ::= "true"
// <false> ::= "false"

// <ty> ::= <tyarr>
// <tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
// <tyarrsub> ::= "->" <ty>
// <tyatom> ::= <tyencl> | <tybool>
// <tyencl> ::= "(" <ty> ")"
// <tybool> ::= "Bool"

/// <tyencl> ::= "(" <ty> ")"
fn parse_tyencl(i: Span) -> IResult<Span, Type, ErrorWithPos> {
    delimited(char('('), parse_ty_space, char(')')).parse(i)
}

/// <tyatom> ::= <tyencl> | <tybool>
fn parse_tyatom(i: Span) -> IResult<Span, Type, ErrorWithPos> {
    preceded(
        multispace0,
        alt((map(tag("Bool"), |_| Type::Bool), parse_tyencl)),
    )
    .parse(i)
}

/// <tyarrsub> ::= "->" <ty>
fn parse_tyarrsub(i: Span) -> IResult<Span, Type, ErrorWithPos> {
    let (i, _) = preceded(multispace0, tag("->")).parse(i)?;
    preceded(multispace0, parse_ty_space).parse(i)
}

/// <tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
fn parse_tyarr(i: Span) -> IResult<Span, Type, ErrorWithPos> {
    let (i, tyatom) = preceded(multispace0, parse_tyatom).parse(i)?;
    let (i, rest) = many0(parse_tyarrsub).parse(i)?;

    let res = once(tyatom)
        .chain(rest)
        .rev()
        .fold(None, |acc, ty| match acc {
            None => Some(ty),
            Some(acc) => Some(Type::Arr(Box::new(ty), Box::new(acc))),
        })
        .expect("expected Some because of once");
    Ok((i, res))
}

/// <ty> ::= <tyarr>
fn parse_ty(i: Span) -> IResult<Span, Type, ErrorWithPos> {
    parse_tyarr(i)
}

fn parse_ty_space(i: Span) -> IResult<Span, Type, ErrorWithPos> {
    let (i, t) = parse_ty(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, t))
}

/// <false> ::= "false"
fn parse_false(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    with_pos(map(tag("false"), |_| Term::False)).parse(i)
}

/// <true> ::= "true"
fn parse_true(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    with_pos(map(tag("true"), |_| Term::True)).parse(i)
}

/// <var> ::= number
fn parse_var(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (_i, _check) = alphanumeric1.parse(i)?;
    update_err(
        "variable must be numeric",
        90,
        with_pos(map_res(digit1, |s: Span| {
            s.fragment().parse::<usize>().map(Term::Var)
        })),
    )
    .parse(i)
}

/// <if> ::= "if" <term> "then" <term> "else" <term>
fn parse_if(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(multispace0, tag("if")).parse(i)?;
    let (i, t1) = update_err("condition expected", 20, parse_term).parse(i)?;
    let (i, _) = preceded(multispace0, tag("then")).parse(i)?;
    let (i, t2) = update_err("then branch expected", 20, parse_term).parse(i)?;
    let (i, _) = preceded(multispace0, tag("else")).parse(i)?;
    let (i, t3) = chmax_err(
        &t1.lasterr.or(t2.lasterr),
        update_err("else branch expected", 20, parse_term),
    )
    .parse(i)?;

    let result = Term::If(Box::new(t1.st), Box::new(t2.st), Box::new(t3.st));
    Ok((
        i,
        Prg {
            st: Spanned {
                v: result,
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: t3.lasterr,
        },
    ))
}

/// <abs> ::= "\:" <ty> "." <term>
fn parse_abs(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(char('\\'), multispace0).parse(i)?;
    let (i, _) = preceded(char(':'), multispace0).parse(i)?;
    let (i, ty) = parse_ty_space(i)?;
    let (i, _) = char('.').parse(i)?;
    let (i, t) = parse_term(i)?;

    let result = Term::Abs(ty, Box::new(t.st));
    Ok((
        i,
        Prg {
            st: Spanned {
                v: result,
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: t.lasterr,
        },
    ))
}

/// <encl> ::= "(" <term> ")"
fn parse_encl(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (i, _) = char('(').parse(i)?;
    let (i, t) = update_err("term expected", 20, parse_term_space).parse(i)?;
    let (i, _) = chmax_err(
        &t.lasterr,
        update_err("')' expected", 50, with_pos(char(')'))),
    )
    .parse(i)?;
    Ok((i, t))
}

/// <atom> ::= <var> | <abs> | <encl> | <true> | <false> | <if>
fn parse_atom(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    preceded(
        multispace0,
        alt((
            parse_if,
            parse_false,
            parse_true,
            parse_encl,
            parse_abs,
            parse_var,
        )),
    )
    .parse(i)
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
                message: format!("internal error: {}", e),
                level: 100,
                kind: None,
                line: i.location_line(),
                column: i.get_utf8_column(),
            })),
        }
    } else {
        Err(nom::Err::Error(ErrorWithPos {
            message: "internal error: many0 did not take it to the end".to_string(),
            level: 100,
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

pub fn display_position(input: &str, line: u32, column: usize) -> String {
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return "\n^".to_string();
    }
    if line as usize > lines.len() {
        return "error while displaying position".to_string();
    }
    let target_line = lines[line as usize - 1];
    if target_line.is_empty() {
        return format!("{}\n^", target_line);
    }
    if column > target_line.len() + 1 {
        return "error while displaying position".to_string();
    }
    format!("{}\n", target_line) + &format!("{:>width$}^", "", width = column - 1)
}

#[allow(unused)]
// maybe used by other modules
pub fn parse(input: &str) -> Result<Term, ErrorWithPos> {
    let (rest, t) = parse_term_space(Span::new(input)).map_err(|e| match e {
        nom::Err::Error(e) => e,
        e => ErrorWithPos {
            message: format!("internal error: {}", e),
            level: 100,
            kind: None,
            line: 0,
            column: 0,
        },
    })?;
    if rest.is_empty() {
        Ok(t.st.v)
    } else if let Some(lasterr) = t.lasterr {
        Err(lasterr)
    } else {
        Err(ErrorWithPos {
            message: "input not fully consumed".to_string(),
            level: 100,
            kind: None,
            line: rest.location_line(),
            column: rest.get_utf8_column(),
        })
    }
}

pub fn parse_spanned(input: &str) -> Result<Spanned<Term>, ErrorWithPos> {
    let (rest, t) = parse_term_space(Span::new(input)).map_err(|e| match e {
        nom::Err::Error(e) => e,
        e => ErrorWithPos {
            message: format!("internal error: {}", e),
            level: 100,
            kind: None,
            line: 0,
            column: 0,
        },
    })?;
    if rest.is_empty() {
        Ok(t.st)
    } else if let Some(lasterr) = t.lasterr {
        Err(lasterr)
    } else {
        Err(ErrorWithPos {
            message: "input not fully consumed".to_string(),
            level: 100,
            kind: None,
            line: rest.location_line(),
            column: rest.get_utf8_column(),
        })
    }
}

pub fn parse_spanned_and_render_err(input: &str) -> Result<Spanned<Term>, (String, String)> {
    match parse_spanned(input) {
        Ok(t) => Ok(t),
        Err(e) => Err((
            format!("Parsing failed: {}", e),
            display_position(input, e.line, e.column),
        )),
    }
}

mod test {
    use rstest::rstest;

    #[allow(unused)]
    use crate::{
        span::Spanned,
        syntax::{term::Term, r#type::Type},
    };

    #[allow(unused)]
    // Helper function to create Spanned<Term> for testing
    fn spanned(term: Term) -> Spanned<Term> {
        Spanned {
            v: term,
            start: 0,
            line: 1,
            column: 1,
        }
    }

    #[allow(unused)]
    // Helper function to extract just the term structure, ignoring position info
    fn extract_term_structure(term: &Term) -> Term {
        match term {
            Term::Var(x) => Term::Var(*x),
            Term::Abs(ty, t) => {
                Term::Abs(ty.clone(), Box::new(spanned(extract_term_structure(&t.v))))
            }
            Term::App(t1, t2) => Term::App(
                Box::new(spanned(extract_term_structure(&t1.v))),
                Box::new(spanned(extract_term_structure(&t2.v))),
            ),
            Term::True => Term::True,
            Term::False => Term::False,
            Term::If(t1, t2, t3) => Term::If(
                Box::new(spanned(extract_term_structure(&t1.v))),
                Box::new(spanned(extract_term_structure(&t2.v))),
                Box::new(spanned(extract_term_structure(&t3.v))),
            ),
        }
    }

    #[rstest]
    #[case("1", Some(Term::Var(1)))]
    #[case(
        r"\:Bool.0 ",
        Some(Term::Abs(Type::Bool, Box::new(spanned(Term::Var(0)))))
    )]
    #[case(r" true", Some(Term::True))]
    #[case(r" false ", Some(Term::False))]
    #[case(
        r"if true then false else true",
        Some(Term::If(
            Box::new(spanned(Term::True)),
            Box::new(spanned(Term::False)),
            Box::new(spanned(Term::True))
        ))
    )]
    #[case(r"\", None)]
    #[case(r"(", None)]
    #[case(r")", None)]
    #[case(r"()", None)]
    #[case(r"\()", None)]
    fn test_parse_term(#[case] input: &str, #[case] expected: Option<Term>) {
        println!("input: {input}");
        let result = super::parse_spanned(input)
            .ok()
            .map(|s| extract_term_structure(&s.v));
        assert_eq!(result, expected);
    }
}
