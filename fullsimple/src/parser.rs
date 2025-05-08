use std::iter::once;

use crate::syntax::{
    pattern::{PatField, Pattern},
    term::{Branch, Field, Term},
    r#type::{TyField, Type},
};
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
    let rs = [
        "let", "plet", "in", "if", "then", "else", "true", "unit", "false", "case", "of", "Self",
    ];
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
                Term::Record(fields) => {
                    let fields = fields
                        .iter()
                        .map(|field| Field {
                            label: field.label.clone(),
                            term: walk(&field.term, z, c),
                        })
                        .collect();
                    Term::Record(fields)
                }
                Term::Projection(t, label) => {
                    Term::Projection(Box::new(walk(t, z, c)), label.clone())
                }
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
                Term::Plet(_pat, _t1, _t2) => {
                    todo!()
                }
                Term::Tagging(ty, label) => Term::Tagging(ty.clone(), label.clone()),
                Term::Case(_t, _branches) => {
                    todo!()
                }
            }
        }
        walk(self, zero_name, 0)
    }
}

/// <ident> ::= <ident> (alphabet|digit) | alphabet
fn parse_ident(i: &str) -> IResult<&str, String> {
    let (i, s0) = preceded(multispace0, alpha1).parse(i)?;
    let (i, s) = many0(alt((alpha1, digit1))).parse(i)?;
    let s = once(s0).chain(s).fold("".to_string(), |acc, c| acc + c);
    if reserved(s.as_str()) {
        Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Fail,
        )))
    } else {
        Ok((i, s))
    }
}

fn parse_number(i: &str) -> IResult<&str, usize> {
    map_res(digit1, |s: &str| s.parse::<usize>()).parse(i)
}

fn parse_number_tostring(i: &str) -> IResult<&str, String> {
    map_res(digit1, |s: &str| s.parse::<usize>())
        .map(|n| n.to_string())
        .parse(i)
}

/// <labelorindex> ::= <label> | number
fn parse_labelorindex(i: &str) -> IResult<&str, String> {
    alt((
        preceded(multispace0, parse_ident),
        preceded(multispace0, parse_number_tostring),
    ))
    .parse(i)
}

// <tyfield> ::= <label> ":" <ty> | <ty>
fn parse_tyfield_tywithlabel(i: &str) -> IResult<&str, (Option<String>, Type)> {
    let (i, label) = preceded(multispace0, parse_ident).parse(i)?;
    let (i, _) = preceded(multispace0, char(':')).parse(i)?;
    let (i, ty) = preceded(multispace0, parse_type_space).parse(i)?;
    Ok((i, (Some(label), ty)))
}
fn parse_tyfield_ty(i: &str) -> IResult<&str, (Option<String>, Type)> {
    let (i, ty) = parse_type_space.parse(i)?;
    Ok((i, (None, ty)))
}
fn parse_tyfield(i: &str) -> IResult<&str, (Option<String>, Type)> {
    alt((parse_tyfield_tywithlabel, parse_tyfield_ty)).parse(i)
}
fn parse_tyfield_withcomma(i: &str) -> IResult<&str, (Option<String>, Type)> {
    let (i, p) = parse_tyfield.parse(i)?;
    let (i, _) = char(',').parse(i)?;
    Ok((i, p))
}

/// <tyfieldseq> ::= <tyfield> "," <tyfieldseq> | null
fn parse_tyfieldseq(i: &str) -> IResult<&str, Vec<TyField>> {
    let (i, fields) = many0(parse_tyfield_withcomma).parse(i)?;
    let (i, last_field) = opt(parse_tyfield).parse(i)?;
    let fields = fields
        .into_iter()
        .chain(last_field)
        .enumerate()
        .map(|(idx, (label, ty))| TyField {
            label: label.unwrap_or(idx.to_string()),
            ty,
        })
        .collect();
    Ok((i, fields))
}

/// <tyrecord> ::= "{" <tyinner> "}"
fn parse_tyrecord(i: &str) -> IResult<&str, Type> {
    let (i, fields) = delimited(
        preceded(multispace0, char('{')),
        preceded(multispace0, parse_tyfieldseq),
        preceded(multispace0, char('}')),
    )
    .parse(i)?;
    Ok((i, Type::TyRecord(fields)))
}

/// <tytagging>::= "<" <tyinner> ">"
fn parse_tytagging(i: &str) -> IResult<&str, Type> {
    let (i, fields) = delimited(
        preceded(multispace0, char('<')),
        preceded(multispace0, parse_tyfieldseq),
        preceded(multispace0, char('>')),
    )
    .parse(i)?;
    Ok((i, Type::TyTagging(fields)))
}

/// <tyatom> ::= <tyencl> | <tyunit> | <tybool> | <tyrecord> | <tytagging> | <tySelf>
fn parse_tyatom(i: &str) -> IResult<&str, Type> {
    preceded(
        multispace0,
        alt((
            map(tag("Bool"), |_| Type::Bool),
            map(tag("Unit"), |_| Type::Unit),
            map(tag("Self"), |_| Type::TySelf),
            parse_tytagging,
            parse_tyrecord,
            parse_tyencl,
        )),
    )
    .parse(i)
}

/// <tyencl> ::= "(" <ty> ")"
fn parse_tyencl(i: &str) -> IResult<&str, Type> {
    delimited(char('('), parse_type_space, char(')')).parse(i)
}

/// <tyarrsub> ::= "->" <ty>
fn parse_tyarrsub(i: &str) -> IResult<&str, Type> {
    let (i, _) = preceded(multispace0, tag("->")).parse(i)?;
    preceded(multispace0, parse_type_space).parse(i)
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
fn parse_type(i: &str) -> IResult<&str, Type> {
    parse_tyarr(i)
}

fn parse_type_space(i: &str) -> IResult<&str, Type> {
    let (i, t) = parse_type(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, t))
}

/// <pattagging> ::= <ty> ":::" <parse_labelorindexcase <Bool, Unit>:::0 true of | <Bool, Unit>:::0 b:Bool => true | <Bool, Unit>:::1 u:Unit => false> | <pattagging> <pat>
fn parse_pattagging(i: &str) -> IResult<&str, Pattern> {
    let (i, ty) = preceded(multispace0, parse_type_space).parse(i)?;
    let (i, _) = preceded(multispace0, tag(":::")).parse(i)?;
    let (i, label) = preceded(multispace0, parse_labelorindex).parse(i)?;
    let (i, patterns) = many0(preceded(multispace0, parse_pat)).parse(i)?;
    Ok((i, Pattern::Tagging(ty, label, patterns)))
}

/// <patfield> ::= <label> ":" <pat> | <pat>
fn parse_patfield_patwithlabel(i: &str) -> IResult<&str, (Option<String>, Pattern)> {
    let (i, label) = preceded(multispace0, parse_ident).parse(i)?;
    let (i, _) = preceded(multispace0, char(':')).parse(i)?;
    let (i, pat) = preceded(multispace0, parse_pat).parse(i)?;
    Ok((i, (Some(label), pat)))
}
fn parse_patfield_pat(i: &str) -> IResult<&str, (Option<String>, Pattern)> {
    let (i, pat) = parse_pat.parse(i)?;
    Ok((i, (None, pat)))
}
fn parse_patfield(i: &str) -> IResult<&str, (Option<String>, Pattern)> {
    alt((parse_patfield_patwithlabel, parse_patfield_pat)).parse(i)
}
fn parse_patfield_withcomma(i: &str) -> IResult<&str, (Option<String>, Pattern)> {
    let (i, p) = parse_patfield.parse(i)?;
    let (i, _) = char(',').parse(i)?;
    Ok((i, p))
}

/// <patfieldseq> ::= <patfield> "," <patfieldseq> | null
fn parse_patfieldseq(i: &str) -> IResult<&str, Vec<PatField>> {
    let (i, fields) = many0(parse_patfield_withcomma).parse(i)?;
    let (i, last_field) = opt(parse_patfield).parse(i)?;
    let fields = fields
        .into_iter()
        .chain(last_field)
        .enumerate()
        .map(|(idx, (label, pat))| PatField {
            label: label.unwrap_or(idx.to_string()),
            pat,
        })
        .collect();
    Ok((i, fields))
}

/// <patrecord> ::= "{" <patinner> "}"
fn parse_patrecord(i: &str) -> IResult<&str, Pattern> {
    let (i, fields) = delimited(
        preceded(multispace0, char('{')),
        preceded(multispace0, parse_patfieldseq),
        preceded(multispace0, char('}')),
    )
    .parse(i)?;
    Ok((i, Pattern::Record(fields)))
}

/// <patvar> ::= <bound> ":" <ty>
fn parse_patvar(i: &str) -> IResult<&str, Pattern> {
    let (i, label) = preceded(multispace0, parse_ident).parse(i)?;
    let (i, _) = preceded(multispace0, char(':')).parse(i)?;
    let (i, ty) = preceded(multispace0, parse_type).parse(i)?;
    Ok((i, Pattern::Var(label, ty)))
}

/// <pat> ::= <patvar> | <patrecord> | <pattagging>
fn parse_pat(i: &str) -> IResult<&str, Pattern> {
    preceded(
        multispace0,
        alt((parse_patvar, parse_pattagging, parse_patrecord)),
    )
    .parse(i)
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

// <var> ::= number | string
fn parse_varnum(i: &str) -> IResult<&str, Term> {
    map(parse_number, Term::Var).parse(i)
}
fn parse_varstr(i: &str) -> IResult<&str, Term> {
    let (i, s) = parse_ident(i)?;
    Ok((i, Term::TmpVar(s)))
}
/// <var> ::= number | string
fn parse_var(i: &str) -> IResult<&str, Term> {
    let (i, v) = preceded(multispace0, alt((parse_varnum, parse_varstr))).parse(i)?;
    Ok((i, v))
}

/// <tagging> ::= <ty> ":::" <labelorindex>
fn parse_tagging(i: &str) -> IResult<&str, Term> {
    let (i, ty) = preceded(multispace0, parse_type).parse(i)?;
    let (i, _) = preceded(multispace0, tag(":::")).parse(i)?;
    let (i, label) = preceded(multispace0, parse_labelorindex).parse(i)?;
    Ok((i, Term::Tagging(ty, label)))
}

/// <field> ::= <label> "=" <term> | <term>
fn parse_field_twithlabel(i: &str) -> IResult<&str, (Option<String>, Term)> {
    let (i, label) = preceded(multispace0, parse_ident).parse(i)?;
    let (i, _) = preceded(multispace0, char('=')).parse(i)?;
    let (i, term) = preceded(multispace0, parse_term_space).parse(i)?;
    Ok((i, (Some(label), term)))
}
fn parse_field_t(i: &str) -> IResult<&str, (Option<String>, Term)> {
    let (i, t) = parse_term_space.parse(i)?;
    Ok((i, (None, t)))
}
fn parse_field(i: &str) -> IResult<&str, (Option<String>, Term)> {
    alt((parse_field_twithlabel, parse_field_t)).parse(i)
}
fn parse_field_withcomma(i: &str) -> IResult<&str, (Option<String>, Term)> {
    let (i, p) = parse_field.parse(i)?;
    let (i, _) = char(',').parse(i)?;
    Ok((i, p))
}

// <fieldseq> ::= <field> "," <fieldseq> | null
fn parse_fieldseq(i: &str) -> IResult<&str, Vec<Field>> {
    let (i, fields) = many0(parse_field_withcomma).parse(i)?;
    let (i, last_field) = opt(parse_field).parse(i)?;
    let fields = fields
        .into_iter()
        .chain(last_field)
        .enumerate()
        .map(|(idx, (label, term))| Field {
            label: label.unwrap_or(idx.to_string()),
            term,
        })
        .collect();
    Ok((i, fields))
}

// <record> ::= "{" <inner> "}"
fn parse_record(i: &str) -> IResult<&str, Term> {
    let (i, fields) = delimited(
        preceded(multispace0, char('{')),
        preceded(multispace0, parse_fieldseq),
        preceded(multispace0, char('}')),
    )
    .parse(i)?;
    Ok((i, Term::Record(fields)))
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

/// <if> ::= "if" <term> "then" <term> "else" <term>
fn parse_if(i: &str) -> IResult<&str, Term> {
    let (i, t1) = preceded(tag("if"), parse_term).parse(i)?;
    let (i, _) = multispace0(i)?;
    let (i, t2) = preceded(tag("then"), parse_term).parse(i)?;
    let (i, _) = multispace0(i)?;
    let (i, t3) = preceded(tag("else"), parse_term).parse(i)?;
    Ok((i, Term::If(Box::new(t1), Box::new(t2), Box::new(t3))))
}

fn parse_branch(i: &str) -> IResult<&str, Branch> {
    let (i, _) = preceded(multispace0, char('|')).parse(i)?;
    let (i, pat) = preceded(multispace0, parse_pat).parse(i)?;
    let (i, _) = preceded(multispace0, tag("=>")).parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;
    Ok((i, Branch { pat, term: t }))
}
/// <branches> ::= "|" <pat> "=>" <term> <branches> | null
fn parse_branches(i: &str) -> IResult<&str, Vec<Branch>> {
    many1(parse_branch).parse(i)
}

// <case> ::= "case" <term> "of" <branches>
fn parse_case(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, tag("case")).parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;
    let (i, _) = preceded(multispace0, tag("of")).parse(i)?;
    let (i, branches) = preceded(multispace0, parse_branches).parse(i)?;
    Ok((i, Term::Case(Box::new(t), branches)))
}

/// <abs> ::= "\:" <ty> "." <term> | "\" <bound> ":" <ty> "." <term>
fn parse_abs(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(char('\\'), multispace0).parse(i)?;
    let (i, name) = opt(parse_ident).parse(i)?;
    let (i, _) = preceded(preceded(multispace0, char(':')), multispace0).parse(i)?;
    let (i, ty) = parse_type_space(i)?;
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

/// <atom> ::= <encl> | <abs> | <if> | <let> | <case> | <var> | <unit> | <true> | <false> | <record> | <tagging>
fn parse_atom(i: &str) -> IResult<&str, Term> {
    preceded(
        multispace0,
        alt((
            parse_case,
            parse_let,
            parse_if,
            parse_tagging,
            parse_record,
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

// <projection> ::= "." <labelorindex>
fn parse_projection(i: &str) -> IResult<&str, String> {
    preceded(
        multispace0,
        preceded(preceded(char('.'), multispace0), parse_labelorindex),
    )
    .parse(i)
}

// <postfix> ::= <atom> <projection> | <atom>
fn parse_postfix(i: &str) -> IResult<&str, Term> {
    let (i, first) = parse_atom.parse(i)?;
    let (i, rest) = many0(parse_projection).parse(i)?;
    let t = rest
        .into_iter()
        .fold(first, |acc, label| Term::Projection(Box::new(acc), label));
    Ok((i, t))
}

// <app> ::= <postfix> <app> | <postfix>
fn parse_app(i: &str) -> IResult<&str, Term> {
    let (i, first) = parse_postfix.parse(i)?;
    let (i, rest) = many0(parse_postfix).parse(i)?;
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
            Box::new(Term::Abs(Type::Unit, Box::new(t.shift(1).unwrap_or(t)))),
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
#[case(
    r"
case <Bool, Unit>:::0 true of
  | <Bool, Unit>:::0 b:Bool => b
  | <Bool, Unit>:::1 u:Unit => false
    ",
    Some(
        Term::Case(
            Box::new(Term::App(Box::new(Term::Tagging(Type::TyTagging([TyField {
                label: "0".to_string(),
                ty: Type::Bool
            }, TyField {
                label: "1".to_string(),
                ty: Type::Unit
            }].to_vec()), "0".to_string())), Box::new(Term::True))),
            vec![
                Branch {
                    pat: Pattern::Tagging(Type::TyTagging(vec![
                        TyField {
                            label: "0".to_string(),
                            ty: Type::Bool
                        },
                        TyField {
                            label: "1".to_string(),
                            ty: Type::Unit
                        }
                    ]), "0".to_string(), vec![Pattern::Var("b".to_string(), Type::Bool)]),
                    term: Term::TmpVar("b".to_string())
                },
                Branch {
                    pat: Pattern::Tagging(Type::TyTagging(vec![
                        TyField {
                            label: "0".to_string(),
                            ty: Type::Bool
                        },
                        TyField {
                            label: "1".to_string(),
                            ty: Type::Unit
                        }
                    ]), "1".to_string(), vec![Pattern::Var("u".to_string(), Type::Unit)]),
                    term: Term::False
                }
            ]
        )
    ))]
#[case(r"\", None)]
#[case(r"(", None)]
#[case(r")", None)]
#[case(r"()", None)]
#[case(r"\()", None)]
fn test_parse_term(#[case] input: &str, #[case] expected: Option<Term>) {
    println!("input: {input}");
    assert_eq!(parse(input).ok(), expected);
}
