use std::iter::once;

use crate::syntax::{term::Term, ty::Type};
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, multispace0},
    combinator::{map, map_res},
    multi::many0,
    sequence::{delimited, preceded},
};

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
// <tyatom> ::= <tyencl> | <tybool>
// <tyarrsub> ::= "->" <ty>
// <tyencl> ::= "(" <ty> ")"
// <tybool> ::= "Bool"

/// <tyencl> ::= "(" <ty> ")"
fn parse_tyencl(i: &str) -> IResult<&str, Type> {
    delimited(char('('), parse_ty_space, char(')')).parse(i)
}

/// <tyatom> ::= <tyencl> | <tybool>
fn parse_tyatom(i: &str) -> IResult<&str, Type> {
    preceded(
        multispace0,
        alt((map(tag("Bool"), |_| Type::Bool), parse_tyencl)),
    )
    .parse(i)
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

/// <var> ::= number
fn parse_var(i: &str) -> IResult<&str, Term> {
    map_res(digit1, |s: &str| s.parse::<usize>().map(Term::Var)).parse(i)
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

/// <abs> ::= "\:" <ty> "." <term>
fn parse_abs(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(char('\\'), multispace0).parse(i)?;
    let (i, _) = preceded(char(':'), multispace0).parse(i)?;
    let (i, ty) = parse_ty_space(i)?;
    let (i, t) = preceded(char('.'), parse_term).parse(i)?;
    Ok((i, Term::Abs(Box::new(t), ty)))
}

/// <encl> ::= "(" <term> ")"
fn parse_encl(i: &str) -> IResult<&str, Term> {
    delimited(char('('), parse_term_space, char(')')).parse(i)
}

/// <atom> ::= <var> | <abs> | <encl> | <true> | <false> | <if>
fn parse_atom(i: &str) -> IResult<&str, Term> {
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
fn parse_app(i: &str) -> IResult<&str, Term> {
    let (i, first) = parse_atom.parse(i)?;
    let (i, rest) = nom::multi::many0(parse_atom).parse(i)?;
    let t = rest
        .into_iter()
        .fold(first, |acc, t| Term::App(Box::new(acc), Box::new(t)));
    Ok((i, t))
}

/// <term> ::= <app>
fn parse_term(i: &str) -> IResult<&str, Term> {
    preceded(multispace0, parse_app).parse(i)
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
        Err("input not fully consumed".to_string())
    }
}

use rstest::rstest;

#[rstest]
#[case("1", Some(Term::Var(1)))]
#[case(r"\:Bool.0 ", Some(Term::Abs(Box::new(Term::Var(0)), Type::Bool)))]
#[case(
    r"   ( \ : Bool . 0 )   ",
    Some(Term::Abs(Box::new(Term::Var(0)), Type::Bool))
)]
#[case(
    r"( \ :Bool. 0) 1",
    Some(Term::App(
        Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool)),
        Box::new(Term::Var(1))
    ))
)]
#[case(
    r"(\ : Bool.0) ( \   : Bool . 0 )",
    Some(Term::App(
        Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool)),
        Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool))
    ))
)]
#[case(
    r"(\:Bool.0) (\:Bool.0) (\:Bool.0)",
    Some(Term::App(
        Box::new(Term::App(
            Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool)),
            Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool))
        )),
        Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool))
    ))
)]
#[case(
    r"\:Bool.1\:Bool.0",
    Some(Term::Abs(
        Box::new(Term::App(
            Box::new(Term::Var(1)),
            Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool))
        )),
        Type::Bool
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
        Box::new(Term::Abs(Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool)), Type::Bool)),
        Box::new(Term::Abs(Box::new(Term::Abs(Box::new(Term::Var(1)), Type::Bool)), Type::Bool))
    ))
)]
// \f:Bool->Bool.\x:Bool.if f x then false else true
#[case(
    r"\:Bool->Bool.\:Bool.if 1 0 then false else true",
    Some(Term::Abs(
        Box::new(Term::Abs(
            Box::new(Term::If(
                Box::new(Term::App(Box::new(Term::Var(1)), Box::new(Term::Var(0)))),
                Box::new(Term::False),
                Box::new(Term::True)
            )),
            Type::Bool
        )),
        Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))
    ))
)]
// realbool = \b:Bool->Bool->Bool.b true false
#[case(
    r"\:Bool->Bool->Bool.0 true false",
    Some(Term::Abs(
        Box::new(Term::App(
            Box::new(Term::App(Box::new(Term::Var(0)), Box::new(Term::True))),
            Box::new(Term::False)
        )),
        Type::Arr(
            Box::new(Type::Bool),
            Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
        )
    ))
)]
// \a:(Bool->Bool)->Bool.a (\b:Bool.b)
#[case(
    r" \ : ( Bool -> Bool ) -> Bool . 0 ( \ : Bool . 0 )",
    Some(Term::Abs(
        Box::new(Term::App(
            Box::new(Term::Var(0)),
            Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool))
        )),
        Type::Arr(
            Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
            Box::new(Type::Bool)
        )
    ))
)]
#[case(
    r"\:(Bool->Bool->Bool)->Bool->Bool.0",
    Some(Term::Abs(
        Box::new(Term::Var(0)),
        Type::Arr(
            Box::new(Type::Arr(
                Box::new(Type::Bool),
                Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
            )),
            Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
        )
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
