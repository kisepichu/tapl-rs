use crate::{
    parser::utils::{chmax_err, update_err, with_pos},
    span::{ErrorWithPos, Prg, Span, Spanned},
    syntax::{
        term::{Info, Term},
        r#type::Type,
    },
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

/// <tybot> ::= "Bot"
fn parse_tybot(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, _) = tag("Bot").parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Type::Bot,
                start: i.location_offset(),
                line: i.location_line(),
                column: i.get_utf8_column(),
            },
            lasterr: None,
        },
    ))
}

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
    let (i, s0) = preceded(multispace0, alt((alpha1, tag("_"), tag("'")))).parse(i)?;
    let (i, s) = many0(alt((alpha1, digit1, tag("_"), tag("'")))).parse(i)?;
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

/// <tyatom> ::= <tyencl> | <tybot> | <tyvar>
fn parse_tyatom(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    preceded(
        multispace0,
        alt((
            parse_tyencl,
            parse_tybot,
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

/// <tyneg> ::= "!" <tyneg> | <tyatom>
fn parse_tyneg(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, neg) = many0(with_pos(char('!'))).parse(i)?;
    let (i, _) = multispace0.parse(i)?;
    let (i, ty) = update_err("type expected", 20, parse_tyatom).parse(i)?;

    let mut result = ty;
    for neg in neg {
        result = Prg {
            st: Spanned {
                v: Type::Arr(
                    Box::new(result.st.clone()),
                    Box::new(Spanned {
                        v: Type::Bot,
                        start: neg.st.start,
                        line: neg.st.line,
                        column: neg.st.column,
                    }),
                ),
                start: result.st.start,
                line: result.st.line,
                column: result.st.column,
            },
            lasterr: result.lasterr,
        };
    }
    Ok((i, result))
}

/// <tyarrsub> ::= "->" <ty>
fn parse_tyarrsub(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, _) = preceded(multispace0, tag("->")).parse(i)?;
    preceded(multispace0, parse_ty_space).parse(i)
}

/// <tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
fn parse_tyarr(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, tyneg) = preceded(multispace0, parse_tyneg).parse(i)?;
    let (i, rest) = many0(parse_tyarrsub).parse(i)?;

    let start_pos = (tyneg.st.start, tyneg.st.line, tyneg.st.column);
    let lasterr = tyneg.lasterr.clone();

    let res = once(tyneg.st)
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
            s.fragment().parse::<usize>().map(|x| {
                Term::Var(
                    x,
                    Info {
                        name: x.to_string(),
                        assumption_num: x,
                    },
                )
            })
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

/// <mabs> ::= "/:" <ty> "." <term> | "/" string ":" <ty> "." <term>
fn parse_mabs(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(char('/'), multispace0).parse(i)?;
    let (i, name) = opt(parse_ident_span).parse(i)?;

    let lasterr_so_far = name.as_ref().and_then(|n| n.lasterr.clone());
    let (i, _) = chmax_err(
        &lasterr_so_far,
        update_err(
            "':' expected after '/' or variable name in mu",
            50,
            with_pos(char(':')),
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
        update_err("'.' expected after type in mu", 50, with_pos(char('.'))),
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
            Term::MAbs(
                ty.st.v,
                Box::new(renamed),
                Info {
                    name: name.st.v,
                    assumption_num: 0,
                },
            )
        }
        None => Term::MAbs(
            ty.st.v,
            Box::new(t.st),
            Info {
                name: "".to_string(),
                assumption_num: 0,
            },
        ),
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

/// <labs> ::= "\:" <ty> "." <term> | "\" string ":" <ty> "." <term>
fn parse_labs(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
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
            Term::Abs(
                ty.st.v,
                Box::new(renamed),
                Info {
                    name: name.st.v,
                    assumption_num: 0,
                },
            )
        }
        None => Term::Abs(
            ty.st.v,
            Box::new(t.st),
            Info {
                name: "".to_string(),
                assumption_num: 0,
            },
        ),
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
    preceded(
        multispace0,
        alt((parse_encl, parse_labs, parse_mabs, parse_var)),
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
        syntax::{
            term::{Info, Term},
            r#type::Type,
        },
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
    fn spanned_type(ty: Type) -> Spanned<Type> {
        Spanned {
            v: ty,
            start: 0,
            line: 1,
            column: 1,
        }
    }

    #[allow(unused)]
    // Helper function to extract just the term structure, ignoring position info
    fn extract_term_structure(term: &Term) -> Term {
        match term {
            Term::Var(x, info) => Term::Var(*x, info.clone()),
            Term::TmpVar(s) => Term::TmpVar(s.clone()),
            Term::Abs(ty, t, info) => Term::Abs(
                extract_type_structure(ty),
                Box::new(spanned(extract_term_structure(&t.v))),
                info.clone(),
            ),
            Term::MAbs(ty, t, info) => Term::MAbs(
                extract_type_structure(ty),
                Box::new(spanned(extract_term_structure(&t.v))),
                info.clone(),
            ),
            Term::App(t1, t2) => Term::App(
                Box::new(spanned(extract_term_structure(&t1.v))),
                Box::new(spanned(extract_term_structure(&t2.v))),
            ),
        }
    }

    #[allow(unused)]
    fn extract_type_structure(ty: &Type) -> Type {
        match ty {
            Type::TyVar(s) => Type::TyVar(s.clone()),
            Type::Bot => Type::Bot,
            Type::Arr(t1, t2) => Type::Arr(
                Box::new(spanned_type(extract_type_structure(&t1.v))),
                Box::new(spanned_type(extract_type_structure(&t2.v))),
            ),
        }
    }

    #[rstest]
    #[case("1", Some(Term::Var(1, Info { name: "1".to_string(), assumption_num: 1 })))]
    #[case(
        r"\:Bool.0 ",
        Some(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Var(0, Info { name: "0".to_string(), assumption_num: 0 }))), Info { name: "".to_string(), assumption_num: 0 }))
    )]
    #[case(
        r"\x:Bool.x",
        Some(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Var(0, Info { name: "x".to_string(), assumption_num: 0 }))), Info { name: "x".to_string(), assumption_num: 0 }))
    )]
    #[case(
        r"\x:Bool.\y:Bool.x",
        Some(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Var(1, Info { name: "x".to_string(), assumption_num: 1 }))), Info { name: "y".to_string(), assumption_num: 0 }))), Info { name: "x".to_string(), assumption_num: 0 }))
    )]
    #[case(
        r"\x:Bool.\y:Bool.y",
        Some(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Abs(Type::TyVar("Bool".to_string()), Box::new(spanned(Term::Var(0, Info { name: "y".to_string(), assumption_num: 0 }))), Info { name: "y".to_string(), assumption_num: 0 }))), Info { name: "x".to_string(), assumption_num: 0 }))
    )]
    #[case(
        r"\:P.\:Q.0 1",
        Some(Term::Abs(
            Type::TyVar("P".to_string()),
                            Box::new(spanned(Term::Abs(
                    Type::TyVar("Q".to_string()),
                    Box::new(spanned(Term::App(
                        Box::new(spanned(Term::Var(0, Info { name: "0".to_string(), assumption_num: 0 }))),
                        Box::new(spanned(Term::Var(1, Info { name: "1".to_string(), assumption_num: 1 })))
                    ))),
                    Info { name: "".to_string(), assumption_num: 0 }
                ))),
            Info { name: "".to_string(), assumption_num: 0 }
        ))
    )]
    #[case(
        r"\x:P.\y:Q.x y",
        Some(Term::Abs(
            Type::TyVar("P".to_string()),
                            Box::new(spanned(Term::Abs(
                    Type::TyVar("Q".to_string()),
                    Box::new(spanned(Term::App(
                        Box::new(spanned(Term::Var(1, Info { name: "x".to_string(), assumption_num: 1 }))),
                        Box::new(spanned(Term::Var(0, Info { name: "y".to_string(), assumption_num: 0 })))
                    ))),
                    Info { name: "y".to_string(), assumption_num: 0 }
                ))),
            Info { name: "x".to_string(), assumption_num: 0 }
        ))
    )]
    #[case(
        r"\f:!!A->A.f",
        Some(Term::Abs(
            Type::Arr(
                Box::new(spanned_type(Type::Arr(
                    Box::new(spanned_type(Type::Arr(
                        Box::new(spanned_type(Type::TyVar("A".to_string()))),
                        Box::new(spanned_type(Type::Bot))
                    ))),
                    Box::new(spanned_type(Type::Bot))
                ))),
                Box::new(spanned_type(Type::TyVar("A".to_string())))
            ),
            Box::new(spanned(Term::Var(0, Info { name: "f".to_string(), assumption_num: 0 }))),
            Info { name: "f".to_string(), assumption_num: 0 }
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
