use std::iter::once;

use crate::syntax::{term::Term, r#type::Type};
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, digit1, multispace0},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::{delimited, preceded},
};

fn reserved(i: &str) -> bool {
    let rs = ["let", "in", "if", "then", "else", "true", "unit", "false"];
    rs.iter().any(|s| *s == i)
}

impl Term {
    fn subst_name(&self, zero_name: &str) -> Term {
        fn walk(t: &Term, z: &str, c: usize) -> Term {
            match t {
                Term::Var(x) => Term::Var(*x),
                Term::TmpVar(s) => {
                    if z == *s {
                        Term::Var(c)
                    } else {
                        t.clone()
                    }
                }
                Term::Abs(ty, t1) => Term::Abs(ty.clone(), Box::new(walk(t1, z, c + 1))),
                Term::App(t1, t2) => Term::App(Box::new(walk(t1, z, c)), Box::new(walk(t2, z, c))),
                Term::Unit => Term::Unit,
                Term::True => Term::True,
                Term::False => Term::False,
                Term::If(t1, t2, t3) => Term::If(
                    Box::new(walk(t1, z, c)),
                    Box::new(walk(t2, z, c)),
                    Box::new(walk(t3, z, c)),
                ),
                Term::Let(t1, t2) => {
                    let t1 = walk(t1, z, c);
                    let t2 = walk(t2, z, c + 1);
                    Term::Let(Box::new(t1), Box::new(t2))
                }
            }
        }
        walk(self, zero_name, 0)
    }
}

// <term> ::= <seq>
// <seq> ::= <app> ";" <seq> | <app>
// <app>  ::= <atom> <app> | <atom>
// <atom> ::= <encl> | <abs> | <let> | <if> | <var> | <unit> | <true> | <false>
// <encl> ::= "(" <term> ")"
// <abs> ::= "\:" <ty> "." <term> | "\" <bound> ":" <ty> "." <term>
// <let> ::= "let" <bound> "=" <term> "in" <term>
// <if> ::= "if" <term> "then" <term> "else" <term>
// <var> ::= number | string
// <unit> ::= "unit"
// <true> ::= "true"
// <false> ::= "false"

// <ty> ::= <tyarr>
// <tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
// <tyarrsub> ::= "->" <ty>
// <tyatom> ::= <tyencl> | <tyunit> | <tybool>
// <tyencl> ::= "(" <ty> ")"
// <tyunit> ::= "Unit"
// <tybool> ::= "Bool"

fn parse_string(i: &str) -> IResult<&str, String> {
    let (i, s) = many1(alpha1).parse(i)?;
    let s = s.iter().fold("".to_string(), |acc, c| acc + c);
    if reserved(s.as_str()) {
        Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Fail,
        )))
    } else {
        Ok((i, s))
    }
}

/// <tyatom> ::= <tyencl> | <tyunit> | <tybool>
fn parse_tyatom(i: &str) -> IResult<&str, Type> {
    preceded(
        multispace0,
        alt((
            map(tag("Bool"), |_| Type::Bool),
            map(tag("Unit"), |_| Type::Unit),
            parse_tyencl,
        )),
    )
    .parse(i)
}

/// <tyencl> ::= "(" <ty> ")"
fn parse_tyencl(i: &str) -> IResult<&str, Type> {
    delimited(char('('), parse_ty_space, char(')')).parse(i)
}

/// <tyarrsub> ::= "->" <ty>
fn parse_tyarrsub(i: &str) -> IResult<&str, Type> {
    let (i, _) = preceded(multispace0, tag("->")).parse(i)?;
    preceded(multispace0, parse_ty_space).parse(i)
}

/// <tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
fn parse_tyarr(i: &str) -> IResult<&str, Type> {
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
fn parse_ty(i: &str) -> IResult<&str, Type> {
    parse_tyarr(i)
}

fn parse_ty_space(i: &str) -> IResult<&str, Type> {
    let (i, t) = parse_ty(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, t))
}

/// <false> ::= "false"
fn parse_false(i: &str) -> IResult<&str, Term> {
    map(tag("false"), |_| Term::False).parse(i)
}

/// <true> ::= "true"
fn parse_true(i: &str) -> IResult<&str, Term> {
    map(tag("true"), |_| Term::True).parse(i)
}

// <unit> ::= "unit"
fn parse_unit(i: &str) -> IResult<&str, Term> {
    map(tag("unit"), |_| Term::Unit).parse(i)
}

/// <var> ::= number | string
fn parse_varnum(i: &str) -> IResult<&str, Term> {
    map_res(digit1, |s: &str| s.parse::<usize>().map(Term::Var)).parse(i)
}
fn parse_varstr(i: &str) -> IResult<&str, Term> {
    let (i, s) = parse_string(i)?;
    Ok((i, Term::TmpVar(s)))
}
fn parse_var(i: &str) -> IResult<&str, Term> {
    let (i, v) = preceded(multispace0, alt((parse_varnum, parse_varstr))).parse(i)?;
    Ok((i, v))
}

/// <if> ::= "if" <term> "then" <term> "else" <term>
fn parse_if(i: &str) -> IResult<&str, Term> {
    let (i, t1) = preceded(tag("if"), parse_term).parse(i)?;
    let (i, _) = multispace0(i)?;
    let (i, t2) = preceded(tag("then"), parse_term).parse(i)?;
    let (i, _) = multispace0(i)?;
    let (i, t3) = preceded(tag("else"), parse_term).parse(i)?;
    Ok((i, Term::If(Box::new(t1), Box::new(t2), Box::new(t3))))
}

/// <let> ::= "let" <bound> "=" <term> "in" <term>
fn parse_let(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, tag("let")).parse(i)?;
    let (i, name) = preceded(multispace0, alpha1).parse(i)?;
    let (i, _) = preceded(multispace0, tag("=")).parse(i)?;
    let (i, t1) = preceded(multispace0, parse_term).parse(i)?;
    let (i, _) = preceded(multispace0, tag("in")).parse(i)?;
    let (i, t2) = preceded(multispace0, parse_term).parse(i)?;
    let renamed = t2.subst_name(name);
    Ok((i, Term::Let(Box::new(t1), Box::new(renamed))))
}

/// <abs> ::= "\:" <ty> "." <term> | "\" <bound> ":" <ty> "." <term>
fn parse_abs(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(char('\\'), multispace0).parse(i)?;
    let (i, name) = opt(parse_string).parse(i)?;
    let (i, _) = preceded(char(':'), multispace0).parse(i)?;
    let (i, ty) = parse_ty_space(i)?;
    let (i, t) = preceded(char('.'), parse_term).parse(i)?;

    match name {
        Some(name) => {
            let renamed = t.subst_name(&name);
            Ok((i, Term::Abs(ty, Box::new(renamed))))
        }
        None => Ok((i, Term::Abs(ty, Box::new(t)))),
    }
}

/// <encl> ::= "(" <term> ")"
fn parse_encl(i: &str) -> IResult<&str, Term> {
    delimited(char('('), parse_term_space, char(')')).parse(i)
}

/// <atom> ::= <encl> | <abs> | <let> | <if> | <var> | <unit> | <true> | <false>
fn parse_atom(i: &str) -> IResult<&str, Term> {
    preceded(
        multispace0,
        alt((
            parse_let,
            parse_if,
            parse_false,
            parse_true,
            parse_unit,
            parse_encl,
            parse_abs,
            parse_var,
        )),
    )
    .parse(i)
}

/// <app> ::= <atom> <app> | <atom>
fn parse_app(i: &str) -> IResult<&str, Term> {
    let (i, first) = parse_atom.parse(i)?;
    let (i, rest) = nom::multi::many0(parse_atom).parse(i)?;
    let t = rest
        .into_iter()
        .fold(first, |acc, t| Term::App(Box::new(acc), Box::new(t)));
    Ok((i, t))
}

// <seq> ::= <app> ";" <seq> | <app>
fn parse_seq(i: &str) -> IResult<&str, Term> {
    let (i, first) = parse_app.parse(i)?;
    let (i, rest) = many0(preceded(multispace0, preceded(char(';'), parse_seq))).parse(i)?;
    let t = rest.into_iter().fold(first, |acc, t| {
        Term::App(
            Box::new(Term::Abs(Type::Unit, Box::new(t.shift(1).unwrap_or(t)))), // todo: abs bound var name
            Box::new(acc),
        )
    });
    Ok((i, t))
}

// <term> ::= <seq>
fn parse_term(i: &str) -> IResult<&str, Term> {
    preceded(multispace0, parse_seq).parse(i)
}

fn parse_term_space(i: &str) -> IResult<&str, Term> {
    let (i, t) = parse_term(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, t))
}

pub fn parse(input: &str) -> Result<Term, String> {
    let (rest, t) = parse_term_space(input).map_err(|e| e.to_string())?;
    if rest.is_empty() {
        Ok(t)
    } else {
        Err(format!("parse error: input not fully consumed: {}", rest))
    }
}

use rstest::rstest;

#[rstest]
#[case("1", Some(Term::Var(1)))]
#[case(r"\:Unit.unit ", Some(Term::Abs(Type::Unit, Box::new(Term::Unit))))]
#[case(
    r" unit ; true ",
    Some(Term::App(
        Box::new(Term::Abs(Type::Unit, Box::new(Term::True))),
        Box::new(Term::Unit)
    ))
)]
#[case(
    r"(\:Bool.unit) false; unit; \:Unit.true",
    Some(Term::App(
        Box::new(Term::Abs(
            Type::Unit,
            Box::new(Term::App(
                Box::new(Term::Abs(
                    Type::Unit,
                    Box::new(Term::Abs(Type::Unit, Box::new(Term::True)))
                )),
                Box::new(Term::Unit)
            ))
        )),
        Box::new(Term::App(
            Box::new(Term::Abs(Type::Bool, Box::new(Term::Unit))),
            Box::new(Term::False)
        ))
    ))
)]
#[case(
    r"   ( \ : Bool . 0 )   ",
    Some(Term::Abs(Type::Bool, Box::new(Term::Var(0))))
)]
#[case(
    r"( \ :Bool. 0) 1",
    Some(Term::App(
        Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0)))),
        Box::new(Term::Var(1))
    ))
)]
#[case(
    r"(\ : Bool.0) ( \   : Bool . 0 )",
    Some(Term::App(
        Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0)))),
        Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0))))
    ))
)]
#[case(
    r"(\:Bool.0) (\:Bool.0) (\:Bool.0)",
    Some(Term::App(
        Box::new(Term::App(
            Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0)))),
            Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0))))
        )),
        Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0))))
    ))
)]
#[case(
    r"\:Bool.1\:Bool.0",
    Some(Term::Abs(
        Type::Bool,
        Box::new(Term::App(
            Box::new(Term::Var(1)),
            Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0))))
        ))
    ))
)]
#[case(r" true", Some(Term::True))]
#[case(r" false ", Some(Term::False))]
#[case(
    r"if true then false else true",
    Some(Term::If(Box::new(Term::True), Box::new(Term::False), Box::new(Term::True)))
)]
#[case(
    r"if true then \:Bool.\:Bool.0 else \:Bool.\:Bool.1",
    Some(Term::If(
        Box::new(Term::True),
        Box::new(Term::Abs(Type::Bool, Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0)))))),
        Box::new(Term::Abs(Type::Bool, Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(1))))))
    ))
)]
// \f:Bool->Bool.\x:Bool.if f x then false else true
#[case(
    r"\:Bool->Bool.\:Bool.if 1 0 then false else true",
    Some(Term::Abs(
        Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)),
        Box::new(Term::Abs(
            Type::Bool,
            Box::new(Term::If(
                Box::new(Term::App(Box::new(Term::Var(1)), Box::new(Term::Var(0)))),
                Box::new(Term::False),
                Box::new(Term::True)
            )),
        ))
    ))
)]
// realbool = \b:Bool->Bool->Bool.b true false
#[case(
    r"\:Bool->Bool->Bool.0 true false",
    Some(Term::Abs(
        Type::Arr(
            Box::new(Type::Bool),
            Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
        ),
        Box::new(Term::App(
            Box::new(Term::App(Box::new(Term::Var(0)), Box::new(Term::True))),
            Box::new(Term::False)
        ))
    ))
)]
// \a:(Bool->Bool)->Bool.a (\b:Bool.b)
#[case(
    r" \ : ( Bool -> Bool ) -> Bool . 0 ( \ : Bool . 0 )",
    Some(Term::Abs(
        Type::Arr(
            Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
            Box::new(Type::Bool)
        ),
        Box::new(Term::App(
            Box::new(Term::Var(0)),
            Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0))))
        ))
    ))
)]
#[case(
    r"\:(Bool->Bool->Bool)->Bool->Bool.0",
    Some(Term::Abs(
        Type::Arr(
            Box::new(Type::Arr(
                Box::new(Type::Bool),
                Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
            )),
            Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
        ),
        Box::new(Term::Var(0))
    ))
)]
#[case(r"\", None)]
#[case(r"(", None)]
#[case(r")", None)]
#[case(r"()", None)]
#[case(r"\()", None)]
fn test_parse_term(#[case] input: &str, #[case] expected: Option<Term>) {
    println!("input: {input}");
    assert_eq!(parse(input).ok(), expected);
}
