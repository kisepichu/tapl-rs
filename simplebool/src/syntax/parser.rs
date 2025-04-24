use crate::syntax::term::Term;
use nom::{
    IResult, Parser,
    branch::alt,
    character::complete::{char, digit1, multispace0},
    combinator::map_res,
    sequence::{delimited, preceded},
};

// <term> ::= <app>
/// <app> ::= <atom> <app> | <atom>
/// <atom> ::= <encl> | <abs> | <var>
// <encl> ::= "(" <term> ")"
// <abs> ::= "\" <term>
// <var> ::= number
/// <var> ::= number
fn parse_var(i: &str) -> IResult<&str, Term> {
    map_res(digit1, |s: &str| s.parse::<usize>().map(Term::Var)).parse(i)
}

/// <abs> ::= "\" <term>
fn parse_abs(i: &str) -> IResult<&str, Term> {
    map_res(
        preceded(char('\\'), parse_term),
        |t| -> Result<_, nom::error::Error<&str>> { Ok(Term::Abs(Box::new(t))) },
    )
    .parse(i)
}

/// <encl> ::= "(" <term> ")"
fn parse_encl(i: &str) -> IResult<&str, Term> {
    delimited(char('('), parse_term_space, char(')')).parse(i)
}

/// <atom> ::= <var> | <abs> | <encl>
fn parse_atom(i: &str) -> IResult<&str, Term> {
    preceded(multispace0, alt((parse_encl, parse_abs, parse_var))).parse(i)
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
#[case(r"\0 ", Some(Term::Abs(Box::new(Term::Var(0)))))]
#[case(r"(\0 )", Some(Term::Abs(Box::new(Term::Var(0)))))]
#[case(
    r"( \0) 1",
    Some(Term::App(Box::new(Term::Abs(Box::new(Term::Var(0)))), Box::new(Term::Var(1))))
)]
#[case(
    r"(\0) (\0)",
    Some(Term::App(
        Box::new(Term::Abs(Box::new(Term::Var(0)))),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))
)]
#[case(
    r"(\0) (\0) (\0)",
    Some(Term::App(
        Box::new(Term::App(
            Box::new(Term::Abs(Box::new(Term::Var(0)))),
            Box::new(Term::Abs(Box::new(Term::Var(0))))
        )),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))
)]
#[case(
    r"\\1\0",
    Some(Term::Abs(Box::new(Term::Abs(Box::new(Term::App(
        Box::new(Term::Var(1)),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))))))
)]
#[case(
    r"\\1\(0)",
    Some(Term::Abs(Box::new(Term::Abs(Box::new(Term::App(
        Box::new(Term::Var(1)),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))))))
)]
#[case(
    r"\\1(\0)",
    Some(Term::Abs(Box::new(Term::Abs(Box::new(Term::App(
        Box::new(Term::Var(1)),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))))))
)]
#[case(
    r"\\(1\0)",
    Some(Term::Abs(Box::new(Term::Abs(Box::new(Term::App(
        Box::new(Term::Var(1)),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))))))
)]
#[case(
    r"\(\1\0)",
    Some(Term::Abs(Box::new(Term::Abs(Box::new(Term::App(
        Box::new(Term::Var(1)),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))))))
)]
#[case(
    r"(\\1\0)",
    Some(Term::Abs(Box::new(Term::Abs(Box::new(Term::App(
        Box::new(Term::Var(1)),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))))))
)]
#[case(
    r"\\(1)\0",
    Some(Term::Abs(Box::new(Term::Abs(Box::new(Term::App(
        Box::new(Term::Var(1)),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))))))
)]
#[case(
    r"\(\1)\0",
    Some(Term::Abs(Box::new(Term::App(
        Box::new(Term::Abs(Box::new(Term::Var(1)))),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
    ))))
)]
#[case(
    r"(\\1)\0",
    Some(Term::App(
        Box::new(Term::Abs(Box::new(Term::Abs(Box::new(Term::Var(1)))))),
        Box::new(Term::Abs(Box::new(Term::Var(0))))
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
