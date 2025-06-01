use std::iter::once;

use crate::syntax::{
    pattern::{PTmpTag, PatField, Pattern},
    term::{Arm, Field, Tag, Term},
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
// Position-aware parsing imports
use crate::span::dummy_spanned;

fn reserved_check(ident: &str) -> bool {
    let rs = [
        "let", "letrec", "plet", "in", "if", "then", "else", "true", "false", "unit", "zero",
        "succ", "pred", "iszero", "case", "fix", "of", "type", "Unit", "Bool", "Nat", "Self",
    ];
    rs.iter().any(|s| *s == ident)
}

/// <ident> ::= <ident> (alphabet|digit) | alphabet
fn parse_ident(i: &str) -> IResult<&str, String> {
    let (i, s0) = preceded(multispace0, alt((alpha1, tag("_")))).parse(i)?;
    let (i, s) = many0(alt((alpha1, digit1, tag("_")))).parse(i)?;
    let s = once(s0).chain(s).fold("".to_string(), |acc, c| acc + c);
    if reserved_check(s.as_str()) {
        Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Fail,
        )))
    } else {
        Ok((i, s))
    }
}
fn parse_ident_reserved(i: &str) -> IResult<&str, String> {
    let (i, s0) = preceded(multispace0, alt((alpha1, tag("_")))).parse(i)?;
    let (i, s) = many0(alt((alpha1, digit1, tag("_")))).parse(i)?;
    let s = once(s0).chain(s).fold("".to_string(), |acc, c| acc + c);
    if reserved_check(s.as_str()) {
        Ok((i, s))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Fail,
        )))
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
fn parse_tyfieldseq(i: &str) -> IResult<&str, Vec<TyField>> {
    let (i, fields) = many0(parse_tyfield_withcomma).parse(i)?;
    let (i, last_field) = opt(parse_tyfield).parse(i)?;
    let fields = fields
        .into_iter()
        .chain(last_field)
        .enumerate()
        .map(|(idx, (label, ty))| TyField {
            label: label.unwrap_or(idx.to_string()),
            ty: dummy_spanned(ty),
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

/// <tyencl> ::= "(" <ty> ")"
fn parse_tyencl(i: &str) -> IResult<&str, Type> {
    delimited(char('('), parse_type_space, char(')')).parse(i)
}

/// <tyatom> ::= <tyencl> | <tyunit> | <tybool> | <tyvar> | <tyrecord> | <tytagging> | <tySelf>
fn parse_tyatom(i: &str) -> IResult<&str, Type> {
    preceded(
        multispace0,
        alt((
            map(parse_reserved("Nat"), |_| Type::Nat),
            map(parse_reserved("Bool"), |_| Type::Bool),
            map(parse_reserved("Unit"), |_| Type::Unit),
            map(parse_reserved("Self"), |_| Type::TySelf),
            parse_tytagging,
            parse_tyrecord,
            parse_tyencl,
            map(parse_ident, Type::TyVar),
        )),
    )
    .parse(i)
}

fn parse_typrodsub(i: &str) -> IResult<&str, Type> {
    let (i, _) = preceded(multispace0, tag("*")).parse(i)?;
    preceded(multispace0, parse_typrod).parse(i)
}
/// <typrod> ::= <tyatom> "*" <typrod> | <tyatom>
/// A*B => {0: A, 1: B}
fn parse_typrod(i: &str) -> IResult<&str, Type> {
    let (i, ty1) = preceded(multispace0, parse_tyatom).parse(i)?;
    let (i, tys) = many0(parse_typrodsub).parse(i)?;
    if tys.is_empty() {
        Ok((i, ty1))
    } else {
        Ok((
            i,
            Type::TyRecord(
                once(ty1)
                    .chain(tys)
                    .enumerate()
                    .map(|(idx, ty)| TyField {
                        label: idx.to_string(),
                        ty: dummy_spanned(ty),
                    })
                    .collect::<Vec<_>>(),
            ),
        ))
    }
}

/// <tyarrsub> ::= "->" <tyarr>
fn parse_tyarrsub(i: &str) -> IResult<&str, Type> {
    let (i, _) = preceded(multispace0, tag("->")).parse(i)?;
    preceded(multispace0, parse_tyarr).parse(i)
}
/// <tyarr> ::= <typrod> "->" <tyarr> | <typrod>
fn parse_tyarr(i: &str) -> IResult<&str, Type> {
    let (i, tyatom) = preceded(multispace0, parse_typrod).parse(i)?;
    let (i, rest) = many0(parse_tyarrsub).parse(i)?;

    let res = once(tyatom)
        .chain(rest)
        .rev()
        .fold(None, |acc, ty| match acc {
            None => Some(ty),
            Some(acc) => Some(Type::Arr(
                Box::new(dummy_spanned(ty)),
                Box::new(dummy_spanned(acc)),
            )),
        })
        .expect("expected Some because of once");
    Ok((i, res))
}

/// <tyvariant> ::= <tyvariant> <tyarr> | <label>
fn parse_tyvariant(i: &str) -> IResult<&str, TyField> {
    let (i, label) = preceded(multispace0, parse_ident).parse(i)?;
    let (i, args) = many0(preceded(multispace0, parse_tyarr)).parse(i)?;
    let ty = args.iter().fold(Type::TySelf, |acc, ty| {
        Type::Arr(
            Box::new(dummy_spanned(ty.clone())),
            Box::new(dummy_spanned(acc)),
        )
    });
    Ok((
        i,
        TyField {
            label,
            ty: dummy_spanned(ty),
        },
    ))
}
fn parse_tysumsub(i: &str) -> IResult<&str, TyField> {
    let (i, _) = preceded(multispace0, tag("+")).parse(i)?;
    preceded(multispace0, parse_tyvariant).parse(i)
}
/// <tysum> ::= <tyvariant> "+" <tysum> | <tyvariant> "+" <tyvariant>
fn parse_tysum(i: &str) -> IResult<&str, Type> {
    let (i, tyf1) = parse_tyvariant.parse(i)?;
    let (i, tyfs) = if let Type::Arr(_, _) = &tyf1.ty.v {
        many0(parse_tysumsub).parse(i)?
    } else {
        many1(parse_tysumsub).parse(i)?
    };
    Ok((
        i,
        Type::TyTagging(once(tyf1).chain(tyfs).collect::<Vec<_>>()),
    ))
}

/// <tysumorarr> ::= <tysum> | <tyarr>
fn parse_tysumorarr(i: &str) -> IResult<&str, Type> {
    alt((parse_tysum, parse_tyarr)).parse(i)
}

/// <ty> ::= <tysum>
fn parse_type(i: &str) -> IResult<&str, Type> {
    parse_tysumorarr.parse(i)
}

fn parse_type_space(i: &str) -> IResult<&str, Type> {
    let (i, ty) = parse_type.parse(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, ty))
}

/// <pattagging> ::= <ty> ":::" <parse_labelorindexcase <Bool, Unit>:::0 true of | <Bool, Unit>:::0 b:Bool => true | <Bool, Unit>:::1 u:Unit => false> | <pattagging> <pat>
fn parse_pattagging_args(i: &str) -> IResult<&str, PTmpTag> {
    let (i, ty) = preceded(multispace0, parse_type_space).parse(i)?;
    let (i, _) = preceded(multispace0, tag(":::")).parse(i)?;
    let (i, label) = preceded(multispace0, parse_labelorindex).parse(i)?;
    let (i, args) = many0(preceded(multispace0, parse_ident)).parse(i)?;
    Ok((
        i,
        PTmpTag {
            ty,
            label,
            nargs: args,
        },
    ))
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
    let pfs = fields
        .into_iter()
        .chain(last_field)
        .enumerate()
        .map(|(idx, (label, pat))| PatField {
            label: label.unwrap_or(idx.to_string()),
            pat,
        })
        .collect();
    Ok((i, pfs))
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
        alt((
            parse_patvar,
            parse_pattagging_args.map(Pattern::TmpTagging),
            parse_patrecord,
        )),
    )
    .parse(i)
}

/// <zero> ::= "zero"
fn parse_zero(i: &str) -> IResult<&str, Term> {
    let (i, _) = parse_reserved("zero").parse(i)?;
    Ok((i, Term::Zero))
}

/// <false> ::= "false"
fn parse_false(i: &str) -> IResult<&str, Term> {
    map(parse_reserved("false"), |_| Term::False).parse(i)
}

/// <true> ::= "true"
fn parse_true(i: &str) -> IResult<&str, Term> {
    map(parse_reserved("true"), |_| Term::True).parse(i)
}

// <unit> ::= "unit"
fn parse_unit(i: &str) -> IResult<&str, Term> {
    map(parse_reserved("unit"), |_| Term::Unit).parse(i)
}

// <var> ::= number | string
fn parse_varnum(i: &str) -> IResult<&str, Term> {
    map(parse_number, Term::Var).parse(i)
}
fn parse_varstr(i: &str) -> IResult<&str, Term> {
    let (i, s) = parse_ident.parse(i)?;
    Ok((i, Term::TmpVar(s)))
}
/// <var> ::= number | string
fn parse_var(i: &str) -> IResult<&str, Term> {
    let (i, v) = preceded(multispace0, alt((parse_varnum, parse_varstr))).parse(i)?;
    Ok((i, v))
}

/// <succ> ::= "succ" <term>
fn parse_succ(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, parse_reserved("succ")).parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;
    Ok((i, Term::Succ(Box::new(dummy_spanned(t)))))
}

/// <pred> ::= "pred" <term>
fn parse_pred(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, parse_reserved("pred")).parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;
    Ok((i, Term::Pred(Box::new(dummy_spanned(t)))))
}

fn parse_reserved(ident: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |i: &str| {
        let (i, s) = preceded(multispace0, parse_ident_reserved).parse(i)?;
        if s != ident {
            Err(nom::Err::Error(nom::error::Error::new(
                i,
                nom::error::ErrorKind::Fail,
            )))
        } else {
            Ok((i, ()))
        }
    }
}

/// <iszero> ::= "iszero" <term>
fn parse_iszero(i: &str) -> IResult<&str, Term> {
    let (i, _) = parse_reserved("iszero").parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;
    Ok((i, Term::IsZero(Box::new(dummy_spanned(t)))))
}

/// <tagging> ::= <ty> ":::" <labelorindex>
fn parse_tagging(i: &str) -> IResult<&str, Tag> {
    let (i, ty) = preceded(multispace0, parse_tyatom).parse(i)?;
    let (i, _) = preceded(multispace0, tag(":::")).parse(i)?;
    let (i, label) = preceded(multispace0, parse_labelorindex).parse(i)?;
    Ok((i, Tag { ty, label }))
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
            term: dummy_spanned(term),
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

fn parse_plet(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, parse_reserved("let")).parse(i)?;
    let (i, p) = preceded(multispace0, parse_pat).parse(i)?;
    let (i, _) = preceded(multispace0, tag("=")).parse(i)?;
    let (i, t1) = preceded(multispace0, parse_term).parse(i)?;
    let (i, _) = preceded(multispace0, parse_reserved("in")).parse(i)?;
    let (i, t2) = preceded(multispace0, parse_term).parse(i)?;
    let (t2_renamed, t1_renamed, p_renamed, _n) = t2.subst_pat(&t1, &p, 0).map_err(|e| {
        println!("subst_pat: {}", e);
        nom::Err::Error(nom::error::Error::new(i, nom::error::ErrorKind::Fail))
    })?;
    Ok((
        i,
        Term::Plet(
            p_renamed,
            Box::new(dummy_spanned(t1_renamed)),
            Box::new(dummy_spanned(t2_renamed)),
        ),
    ))
}

/// <let> ::= "let" <bound> "=" <term> "in" <term>
fn parse_let(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, parse_reserved("let")).parse(i)?;
    let (i, name) = preceded(multispace0, parse_ident).parse(i)?;
    let (i, _) = preceded(multispace0, tag("=")).parse(i)?;
    let (i, t1) = preceded(multispace0, parse_term).parse(i)?;
    let (i, _) = preceded(multispace0, parse_reserved("in")).parse(i)?;
    let (i, t2) = preceded(multispace0, parse_term).parse(i)?;
    let renamed = t2.subst_name(name.as_str());
    Ok((
        i,
        Term::Let(
            Box::new(dummy_spanned(t1)),
            Box::new(dummy_spanned(renamed)),
        ),
    ))
}

/// <if> ::= "if" <term> "then" <term> "else" <term>
fn parse_if(i: &str) -> IResult<&str, Term> {
    let (i, t1) = preceded(parse_reserved("if"), parse_term).parse(i)?;
    let (i, _) = multispace0(i)?;
    let (i, t2) = preceded(parse_reserved("then"), parse_term).parse(i)?;
    let (i, _) = multispace0(i)?;
    let (i, t3) = preceded(parse_reserved("else"), parse_term).parse(i)?;
    Ok((
        i,
        Term::If(
            Box::new(dummy_spanned(t1)),
            Box::new(dummy_spanned(t2)),
            Box::new(dummy_spanned(t3)),
        ),
    ))
}

fn parse_arm(i: &str) -> IResult<&str, Arm> {
    let (i, _) = preceded(multispace0, char('|')).parse(i)?;
    let (i, ptag) = preceded(multispace0, parse_pattagging_args)
        .parse(i)
        .inspect_err(|_| {
            println!("Error parsing case arm: expected <type>:::<label> <args...>");
        })?;
    let (i, _) = preceded(multispace0, tag("=>")).parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;

    let (t_renamed, ptag_renamed, _) = t.subst_ptag(&ptag, 0).map_err(|e| {
        println!("subst_ptag: {}", e);
        nom::Err::Error(nom::error::Error::new(i, nom::error::ErrorKind::Fail))
    })?;
    Ok((
        i,
        Arm {
            term: dummy_spanned(t_renamed),
            ptag: ptag_renamed,
        },
    ))
}
/// <arms> ::= "|" <pat> "=>" <term> <arms> | null
fn parse_arms(i: &str) -> IResult<&str, Vec<Arm>> {
    many1(parse_arm).parse(i)
}

// <case> ::= "case" <term> "of" <arms>
fn parse_case(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, parse_reserved("case")).parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;
    let (i, _) = preceded(multispace0, parse_reserved("of")).parse(i)?;
    let (i, arms) = preceded(multispace0, parse_arms).parse(i)?;
    Ok((i, Term::Case(Box::new(dummy_spanned(t)), arms)))
}

/// <lettype> ::= "type" <ident> "=" <type> "in" <term>
fn parse_lettype(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, parse_reserved("type")).parse(i)?;
    let (i, name) = preceded(multispace0, parse_ident).parse(i)?;
    let (i, _) = preceded(multispace0, tag("=")).parse(i)?;
    let (i, ty) = preceded(multispace0, parse_type_space).parse(i)?;
    let (i, _) = preceded(multispace0, parse_reserved("in")).parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;
    let renamed = t.subst_type_name(name.as_str(), &ty);
    Ok((i, renamed))
}

/// <fix> ::= "fix" <term>
fn parse_fix(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, parse_reserved("fix")).parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;
    Ok((i, Term::Fix(Box::new(dummy_spanned(t)))))
}

/// <letrec> ::= "letrec" <bound> ":" <ty> "=" <term> "in" <term>
fn parse_letrec(i: &str) -> IResult<&str, Term> {
    let (i, _) = preceded(multispace0, parse_reserved("letrec")).parse(i)?;
    let (i, name) = preceded(multispace0, parse_ident).parse(i)?;
    let (i, _) = preceded(multispace0, char(':')).parse(i)?;
    let (i, ty) = preceded(multispace0, parse_type_space).parse(i)?;
    let (i, _) = preceded(multispace0, tag("=")).parse(i)?;
    let (i, t1) = preceded(multispace0, parse_term).parse(i)?;
    let (i, _) = preceded(multispace0, parse_reserved("in")).parse(i)?;
    let (i, t2) = preceded(multispace0, parse_term).parse(i)?;
    let t1_renamed = t1.subst_name(name.as_str());
    let t2_renamed = t2.subst_name(name.as_str());
    Ok((
        i,
        Term::Let(
            Box::new(dummy_spanned(Term::Fix(Box::new(dummy_spanned(
                Term::Abs(ty, Box::new(dummy_spanned(t1_renamed))),
            ))))),
            Box::new(dummy_spanned(t2_renamed)),
        ),
    ))
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
            Ok((i, Term::Abs(ty, Box::new(dummy_spanned(renamed)))))
        }
        None => Ok((i, Term::Abs(ty, Box::new(dummy_spanned(t))))),
    }
}

/// <encl> ::= "(" <term> ")"
fn parse_encl(i: &str) -> IResult<&str, Term> {
    delimited(char('('), parse_term_space, char(')')).parse(i)
}

/// <atom> ::= <encl> | <abs> | <if> | <let> | <let> | <case> | <var> | <unit> | <true> | <false> | <record> | <tagging>
fn parse_atom(i: &str) -> IResult<&str, Term> {
    preceded(
        multispace0,
        alt((
            parse_letrec,
            parse_fix,
            parse_lettype,
            parse_case,
            parse_plet,
            parse_let,
            parse_if,
            parse_succ,
            parse_pred,
            parse_iszero,
            parse_tagging.map(Term::Tagging),
            parse_record,
            parse_zero,
            parse_false,
            parse_true,
            parse_unit,
            parse_abs,
            parse_var,
            parse_encl,
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
    let t = rest.into_iter().fold(first, |acc, label| {
        Term::Projection(Box::new(dummy_spanned(acc)), label)
    });
    Ok((i, t))
}

// <app> ::= <postfix> <app> | <postfix>
fn parse_app(i: &str) -> IResult<&str, Term> {
    let (i, first) = parse_postfix.parse(i)?;
    let (i, rest) = many0(parse_postfix).parse(i)?;
    let t = rest.into_iter().fold(first, |acc, t| {
        Term::App(Box::new(dummy_spanned(acc)), Box::new(dummy_spanned(t)))
    });
    Ok((i, t))
}

// <seq> ::= <app> ";" <seq> | <app>
fn parse_seq(i: &str) -> IResult<&str, Term> {
    let (i, first) = parse_app.parse(i)?;
    let (i, rest) = many0(preceded(multispace0, preceded(char(';'), parse_seq))).parse(i)?;
    let t = rest.into_iter().fold(first, |acc, t| {
        Term::App(
            Box::new(dummy_spanned(Term::Abs(
                Type::Unit,
                Box::new(dummy_spanned(t.shift(1).unwrap_or(t))),
            ))),
            Box::new(dummy_spanned(acc)),
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
        Err(format!(
            "\nparse error: input not fully consumed: {}, \nparsed= {}\n= {:?}",
            rest, t, t
        ))
    }
}

use rstest::rstest;

#[rstest]
#[case("1", Some(Term::Var(1)))]
#[case(
    r"\:Unit.unit ",
    Some(Term::Abs(Type::Unit, Box::new(dummy_spanned(Term::Unit))))
)]
#[case(
    r" unit ; true ",
    Some(Term::App(
        Box::new(dummy_spanned(Term::Abs(Type::Unit, Box::new(dummy_spanned(Term::True))))),
        Box::new(dummy_spanned(Term::Unit))
    ))
)]
#[case(
    r"(\:Bool.unit) false; unit; \:Unit.true",
    Some(Term::App(
        Box::new(dummy_spanned(Term::Abs(
            Type::Unit,
            Box::new(dummy_spanned(Term::App(
                Box::new(dummy_spanned(Term::Abs(
                    Type::Unit,
                    Box::new(dummy_spanned(Term::Abs(
                        Type::Unit,
                        Box::new(dummy_spanned(Term::True))
                    )))
                ))),
                Box::new(dummy_spanned(Term::Unit))
            )))
        ))),
        Box::new(dummy_spanned(Term::App(
            Box::new(dummy_spanned(Term::Abs(Type::Bool, Box::new(dummy_spanned(Term::Unit))))),
            Box::new(dummy_spanned(Term::False))
        )))
    ))
)]
#[case(
    r"   ( \ : Bool . 0 )   ",
    Some(Term::Abs(Type::Bool, Box::new(dummy_spanned(Term::Var(0)))))
)]
#[case(
    r"( \ :Bool. 0) 1",
    Some(Term::App(
        Box::new(dummy_spanned(Term::Abs(Type::Bool, Box::new(dummy_spanned(Term::Var(0)))))),
        Box::new(dummy_spanned(Term::Var(1)))
    ))
)]
#[case(
    r"(\ : Bool.0) ( \   : Bool . 0 )",
    Some(Term::App(
        Box::new(dummy_spanned(Term::Abs(Type::Bool, Box::new(dummy_spanned(Term::Var(0)))))),
        Box::new(dummy_spanned(Term::Abs(Type::Bool, Box::new(dummy_spanned(Term::Var(0))))))
    ))
)]
#[case(
    r"(\:Bool.0) (\:Bool.0) (\:Bool.0)",
    Some(Term::App(
        Box::new(dummy_spanned(Term::App(
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0)))
            ))),
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0)))
            )))
        ))),
        Box::new(dummy_spanned(Term::Abs(Type::Bool, Box::new(dummy_spanned(Term::Var(0))))))
    ))
)]
#[case(
    r"\:Bool.1\:Bool.0",
    Some(Term::Abs(
        Type::Bool,
        Box::new(dummy_spanned(Term::App(
            Box::new(dummy_spanned(Term::Var(1))),
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0)))
            )))
        )))
    ))
)]
#[case(r" true", Some(Term::True))]
#[case(r" false ", Some(Term::False))]
#[case(
    r"if true then false else true",
    Some(Term::If(
        Box::new(dummy_spanned(Term::True)),
        Box::new(dummy_spanned(Term::False)),
        Box::new(dummy_spanned(Term::True))
    ))
)]
#[case(
    r"if true then \:Bool.\:Bool.0 else \:Bool.\:Bool.1",
    Some(Term::If(
        Box::new(dummy_spanned(Term::True)),
        Box::new(dummy_spanned(Term::Abs(
            Type::Bool,
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0)))
            )))
        ))),
        Box::new(dummy_spanned(Term::Abs(
            Type::Bool,
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(1)))
            )))
        )))
    ))
)]
// \f:Bool->Bool.\x:Bool.if f x then false else true
#[case(
    r"\:Bool->Bool.\:Bool.if 1 0 then false else true",
    Some(Term::Abs(
        Type::Arr(
            Box::new(dummy_spanned(Type::Bool)),
            Box::new(dummy_spanned(Type::Bool))
        ),
        Box::new(dummy_spanned(Term::Abs(
            Type::Bool,
            Box::new(dummy_spanned(Term::If(
                Box::new(dummy_spanned(Term::App(
                    Box::new(dummy_spanned(Term::Var(1))),
                    Box::new(dummy_spanned(Term::Var(0)))
                ))),
                Box::new(dummy_spanned(Term::False)),
                Box::new(dummy_spanned(Term::True))
            ))),
        )))
    ))
)]
// realbool = \b:Bool->Bool->Bool.b true false
#[case(
    r"\:Bool->Bool->Bool.0 true false",
    Some(Term::Abs(
        Type::Arr(
            Box::new(dummy_spanned(Type::Bool)),
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Bool))
            ))),
        ),
        Box::new(dummy_spanned(Term::App(
            Box::new(dummy_spanned(Term::App(
                Box::new(dummy_spanned(Term::Var(0))),
                Box::new(dummy_spanned(Term::True))
            ))),
            Box::new(dummy_spanned(Term::False))
        )))
    ))
)]
// \a:(Bool->Bool)->Bool.a (\b:Bool.b)
#[case(
    r" \ : ( Bool -> Bool ) -> Bool . 0 ( \ : Bool . 0 )",
    Some(Term::Abs(
        Type::Arr(
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Bool))
            ))),
            Box::new(dummy_spanned(Type::Bool))
        ),
        Box::new(dummy_spanned(Term::App(
            Box::new(dummy_spanned(Term::Var(0))),
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0)))
            )))
        )))
    ))
)]
#[case(
    r"\:(Bool->Bool->Bool)->Bool->Bool.0",
    Some(Term::Abs(
        Type::Arr(
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Arr(
                    Box::new(dummy_spanned(Type::Bool)),
                    Box::new(dummy_spanned(Type::Bool))
                )))
            ))),
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Bool))
            )))
        ),
        Box::new(dummy_spanned(Term::Var(0)))
    ))
)]
#[case(
    r"
case <Bool->Self, Unit->Self>:::0 true of
  | <Bool->Self, Unit->Self>:::0 b => b
  | <Bool->Self, Unit->Self>:::1 u => false
    ",
    Some(Term::Case(
        Box::new(dummy_spanned(Term::App(
            Box::new(dummy_spanned(Term::Tagging(Tag {
                ty: Type::TyTagging(
                    [
                        TyField {
                            label: "0".to_string(),
                            ty: dummy_spanned(Type::Arr(
                                Box::new(dummy_spanned(Type::Bool)),
                                Box::new(dummy_spanned(Type::TySelf)),
                            )),
                        },
                        TyField {
                            label: "1".to_string(),
                            ty: dummy_spanned(Type::Arr(
                                Box::new(dummy_spanned(Type::Unit)),
                                Box::new(dummy_spanned(Type::TySelf)),
                            )),
                        },
                    ]
                    .to_vec(),
                ),
                label: "0".to_string(),
            }))),
            Box::new(dummy_spanned(Term::True)),
        ))),
        vec![
            Arm {
                ptag: PTmpTag {
                    ty: Type::TyTagging(vec![
                        TyField {
                            label: "0".to_string(),
                            ty: dummy_spanned(Type::Arr(
                                Box::new(dummy_spanned(Type::Bool)),
                                Box::new(dummy_spanned(Type::TySelf)),
                            )),
                        },
                        TyField {
                            label: "1".to_string(),
                            ty: dummy_spanned(Type::Arr(
                                Box::new(dummy_spanned(Type::Unit)),
                                Box::new(dummy_spanned(Type::TySelf)),
                            )),
                        },
                    ]),
                    label: "0".to_string(),
                    nargs: vec!["0".to_string()],
                },
                term: dummy_spanned(Term::Var(0)),
            },
            Arm {
                ptag: PTmpTag {
                    ty: Type::TyTagging(vec![
                        TyField {
                            label: "0".to_string(),
                            ty: dummy_spanned(Type::Arr(
                                Box::new(dummy_spanned(Type::Bool)),
                                Box::new(dummy_spanned(Type::TySelf)),
                            )),
                        },
                        TyField {
                            label: "1".to_string(),
                            ty: dummy_spanned(Type::Arr(
                                Box::new(dummy_spanned(Type::Unit)),
                                Box::new(dummy_spanned(Type::TySelf)),
                            )),
                        },
                    ]),
                    label: "1".to_string(),
                    nargs: vec!["0".to_string()],
                },
                term: dummy_spanned(Term::False),
            },
        ],
    ))
)]
#[case(
    r"let x = true in x",
    Some(Term::Let(
        Box::new(dummy_spanned(Term::True)),
        Box::new(dummy_spanned(Term::Var(0)))
    ))
)]
#[case(
    r"let x:Bool = true in x",
    Some(Term::Plet(
        Pattern::Var("0".to_string(), Type::Bool),
        Box::new(dummy_spanned(Term::True)),
        Box::new(dummy_spanned(Term::Var(0)))
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
