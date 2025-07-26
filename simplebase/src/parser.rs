use crate::{
    parser::utils::{chmax_err, update_err, with_pos},
    span::{ErrorWithPos, Prg, Span, Spanned},
    syntax::{term::Term, r#type::Type},
};
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, map_res, opt},
    multi::many0,
    sequence::preceded,
};
use std::iter::once;

mod utils;

// <term> ::= <app>
// <app>  ::= <atom> <app> | <atom>
// <atom> ::= <encl> | <abs> | <var> | <true> | <false> | <if>
// <encl> ::= "(" <term> ")"
// <abs> ::= "\" <ident>? ":" <ty> "." <term>
// <if> ::= "if" <term> "then" <term> "else" <term>
// <var> ::= number | <ident>
// <true> ::= "true"
// <false> ::= "false"

// <ty> ::= <tyarr>
// <tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
// <tyarrsub> ::= "->" <ty>
// <tyatom> ::= <tyencl> | <tybool>
// <tyencl> ::= "(" <ty> ")"
// <tybool> ::= "Bool"

/// <tyencl> ::= "(" <ty> ")"
fn parse_tyencl(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, _) = char('(').parse(i)?;
    let (i, t) = update_err("type expected", 20, parse_ty_space).parse(i)?;
    let (i, _) = chmax_err(
        &t.lasterr,
        update_err("')' expected", 50, with_pos(char(')'))),
    )
    .parse(i)?;
    Ok((i, t))
}

/// <ident> ::= <ident> (alphabet|digit) | alphabet
fn parse_ident_span(i: Span) -> IResult<Span, Prg<String>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, s0) = preceded(multispace0, alt((alpha1, tag("_")))).parse(i)?;
    let (i, s) = many0(alt((alpha1, digit1, tag("_")))).parse(i)?;
    let s = once(s0.fragment())
        .chain(s.iter().map(|x| x.fragment()))
        .fold("".to_string(), |acc, c| acc + c);
    Ok((
        i,
        Prg {
            st: Spanned {
                v: s,
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: None,
        },
    ))
}

/// <tyatom> ::= <tyencl> | <tybool>
fn parse_tyatom(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    preceded(
        multispace0,
        alt((
            parse_tyencl,
            map(parse_ident_span, |ident| Prg {
                st: Spanned {
                    v: Type::TyVar(ident.st.v),
                    start: ident.st.start,
                    line: ident.st.line,
                    column: ident.st.column,
                },
                lasterr: ident.lasterr,
            }),
        )),
    )
    .parse(i)
}

/// <tyarrsub> ::= "->" <ty>
fn parse_tyarrsub(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, _) = preceded(multispace0, tag("->")).parse(i)?;
    preceded(multispace0, parse_ty_space).parse(i)
}

/// <tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
fn parse_tyarr(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, tyatom) = preceded(multispace0, parse_tyatom).parse(i)?;
    let (i, rest) = many0(parse_tyarrsub).parse(i)?;

    let start_pos = (tyatom.st.start, tyatom.st.line, tyatom.st.column);
    let lasterr = tyatom.lasterr.clone();

    let res = once(tyatom.st)
        .chain(rest.into_iter().map(|t| t.st))
        .rev()
        .fold(None, |acc, ty| match acc {
            None => Some(ty.v),
            Some(acc) => Some(Type::Arr(
                Box::new(ty.clone()),
                Box::new(Spanned {
                    v: acc,
                    start: ty.start,
                    line: ty.line,
                    column: ty.column,
                }),
            )),
        })
        .expect("expected Some because of once");

    Ok((
        i,
        Prg {
            st: Spanned {
                v: res,
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr,
        },
    ))
}

/// <ty> ::= <tyarr>
fn parse_ty(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    parse_tyarr(i)
}

fn parse_ty_space(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, t) = parse_ty(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, t))
}

/// <var> ::= number | <ident>
fn parse_var(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    preceded(multispace0, alt((parse_varnum, parse_varstr))).parse(i)
}

fn parse_varnum(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
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

fn parse_varstr(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, s) = parse_ident_span(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::TmpVar(s.st.v),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: s.lasterr,
        },
    ))
}

/// <abs> ::= "\" <ident>? ":" <ty> "." <term>
fn parse_abs(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(char('\\'), multispace0).parse(i)?;
    let (i, name) = opt(parse_ident_span).parse(i)?;

    let lasterr_so_far = name.as_ref().and_then(|n| n.lasterr.clone());
    let (i, _) = chmax_err(
        &lasterr_so_far,
        update_err(
            "':' expected after '\\' or variable name in lambda",
            50,
            preceded(multispace0, with_pos(char(':'))),
        ),
    )
    .parse(i)?;

    let (i, ty) = chmax_err(
        &lasterr_so_far,
        update_err("type expected after ':'", 20, parse_ty_space),
    )
    .parse(i)?;

    let lasterr_so_far = lasterr_so_far.or(ty.lasterr.clone());

    let (i, _) = chmax_err(
        &lasterr_so_far,
        update_err("'.' expected after type in lambda", 50, with_pos(char('.'))),
    )
    .parse(i)?;
    let (i, t) = chmax_err(
        &lasterr_so_far,
        update_err("term expected after '.'", 20, parse_term),
    )
    .parse(i)?;

    let result = match name.clone() {
        Some(name) => {
            let renamed = t.st.v.subst_name_spanned(&name.st.v, &t.st);
            Term::Abs(ty.st.v, Box::new(renamed))
        }
        None => Term::Abs(ty.st.v, Box::new(t.st)),
    };
    Ok((
        i,
        Prg {
            st: Spanned {
                v: result,
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: lasterr_so_far.or(t.lasterr),
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
    println!();
    let lines: Vec<&str> = input.lines().collect();
    if line == 0 || (line as usize) > lines.len() {
        return "Invalid line number".to_string();
    }

    let target_line = lines[(line - 1) as usize];
    let mut result = String::new();
    result.push_str(&format!("at line {}, column {}:\n", line, column));
    result.push_str(&format!("  | {}\n", target_line));
    result.push_str("  | ");
    for _ in 0..column.saturating_sub(1) {
        result.push(' ');
    }
    result.push_str("^\n");
    result
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
            Term::TmpVar(s) => Term::TmpVar(s.clone()),
            Term::Abs(ty, t) => {
                Term::Abs(ty.clone(), Box::new(spanned(extract_term_structure(&t.v))))
            }
            Term::App(t1, t2) => Term::App(
                Box::new(spanned(extract_term_structure(&t1.v))),
                Box::new(spanned(extract_term_structure(&t2.v))),
            ),
        }
    }

    #[rstest]
    #[case("1", Some(Term::Var(1)))]
    #[case(
        r"\:Bool.0 ",
        Some(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Var(0)))))
    )]
    #[case(
        r"\x:Bool.x",
        Some(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Var(0)))))
    )]
    #[case(
        r"\x:Bool.\y:Bool.x",
        Some(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Var(1))))))))
    )]
    #[case(
        r"\x:Bool.\y:Bool.y",
        Some(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Var(0))))))))
    )]
    #[case(
        r"\:P.\:Q.0 1",
        Some(Term::Abs(Type::TyVar("P".to_string()), Box::new(spanned(Term::Abs(Type::TyVar("Q".to_string()), Box::new(spanned(Term::App(Box::new(spanned(Term::Var(0))), Box::new(spanned(Term::Var(1)))))))))))
    )]
    #[case(
        r"\x:P.\y:Q.x y",
        Some(Term::Abs(Type::TyVar("P".to_string()), Box::new(spanned(Term::Abs(Type::TyVar("Q".to_string()), Box::new(spanned(Term::App(Box::new(spanned(Term::Var(1))), Box::new(spanned(Term::Var(0)))))))))))
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
