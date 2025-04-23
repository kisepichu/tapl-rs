use crate::term::Term;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::take_while,
    character::complete::{char, multispace0},
    error::ErrorKind,
    sequence::delimited,
};

// term = var | abs | app
// var = NUMBER
// abs = "\" term
// app = term term

fn parse_var(i: &str) -> IResult<&str, Term> {
    let (i, x) = take_while(|c: char| c.is_ascii_digit()).parse(i)?;
    if let Ok(n) = x.parse::<usize>() {
        Ok((i, Term::Var(n)))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(i, ErrorKind::Digit)))
    }
}

fn parse_abs(i: &str) -> IResult<&str, Term> {
    let (i, _) = char('\\')(i)?;
    let (i, t) = parse_term(i)?;
    Ok((i, Term::Abs(Box::new(t))))
}

fn parse_app(i: &str) -> IResult<&str, Term> {
    let (i, first) = parse_atom.parse(i)?;
    let (i, rest) = nom::multi::many1(parse_atom).parse(i)?;
    let res = rest
        .into_iter()
        .fold(first, |acc, t| Term::App(Box::new(acc), Box::new(t)));

    Ok((i, res))
}

fn parse_atom(i: &str) -> IResult<&str, Term> {
    let (i, _) = multispace0.parse(i)?;
    alt((parse_paren, parse_abs, parse_var)).parse(i)
}

fn parse_paren(i: &str) -> IResult<&str, Term> {
    let (i, t) = delimited(char('('), parse_term, char(')')).parse(i)?;
    Ok((i, t))
}

fn parse_term(i: &str) -> IResult<&str, Term> {
    let (i, _) = multispace0.parse(i)?;
    alt((parse_app, parse_atom))
        .parse(i)
        .map_err(|_| nom::Err::Error(nom::error::Error::new(i, ErrorKind::Alt)))
}

pub fn parse(input: &str) -> Result<Term, String> {
    let (rest, t) = parse_term(input).map_err(|e| e.to_string())?;
    if rest.is_empty() {
        Ok(t)
    } else {
        Err("input not fully consumed".to_string())
    }
}

use rstest::rstest;

#[rstest]
#[case("1", Some(Term::Var(1)))]
#[case(r"\0", Some(Term::Abs(Box::new(Term::Var(0)))))]
#[case(r"(\0)", Some(Term::Abs(Box::new(Term::Var(0)))))]
#[case(
    r"(\0) 1",
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
