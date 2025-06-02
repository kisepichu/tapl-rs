use std::iter::once;

use crate::{
    parser::utils::{chmax_err, update_err, with_pos},
    span::{ErrorWithPos, Prg, Span, Spanned},
    syntax::{
        pattern::{PTmpTag, PatField, Pattern},
        term::{Arm, Field, Tag, Term},
        r#type::{TyField, Type},
    },
};
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::preceded,
};
mod utils;

fn reserved_check(ident: &str) -> bool {
    let rs = [
        "let", "letrec", "plet", "in", "if", "then", "else", "true", "false", "unit", "zero",
        "succ", "pred", "iszero", "case", "fix", "of", "type", "Unit", "Bool", "Nat", "Self",
    ];
    rs.iter().any(|s| *s == ident)
}

/// <ident> ::= <ident> (alphabet|digit) | alphabet
fn parse_ident_span(i: Span) -> IResult<Span, Prg<String>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, s0) = preceded(multispace0, alt((alpha1, tag("_")))).parse(i)?;
    let (i, s) = many0(alt((alpha1, digit1, tag("_")))).parse(i)?;
    let s = once(s0.fragment())
        .chain(s.iter().map(|x| x.fragment()))
        .fold("".to_string(), |acc, c| acc + c);
    if reserved_check(s.as_str()) {
        Err(nom::Err::Error(ErrorWithPos {
            message: format!("reserved word '{}' used as identifier", s),
            level: 20,
            kind: Some(nom::error::ErrorKind::Fail),
            line: i.location_line(),
            column: i.get_utf8_column(),
        }))
    } else {
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
}
fn parse_reserved_span(ident: &'static str) -> impl Fn(Span) -> IResult<Span, (), ErrorWithPos> {
    move |i: Span| {
        let (i, s0) = preceded(multispace0, alt((alpha1, tag("_")))).parse(i)?;
        let (i, s) = many0(alt((alpha1, digit1, tag("_")))).parse(i)?;

        let parsed = once(s0.fragment())
            .chain(s.iter().map(|x| x.fragment()))
            .fold("".to_string(), |acc, c| acc + c);
        if parsed != ident {
            Err(nom::Err::Error(ErrorWithPos {
                message: format!("expected ident '{}'", ident),
                level: 90,
                kind: Some(nom::error::ErrorKind::Fail),
                line: i.location_line(),
                column: i.get_utf8_column(),
            }))
        } else {
            Ok((i, ()))
        }
    }
}

fn parse_number_tostring(i: Span) -> IResult<Span, Prg<String>, ErrorWithPos> {
    with_pos(map_res(digit1, |s: Span| {
        s.fragment().parse::<usize>().map(|n| n.to_string())
    }))
    .parse(i)
}

fn parse_labelorindex_span(i: Span) -> IResult<Span, Prg<String>, ErrorWithPos> {
    alt((parse_ident_span, parse_number_tostring)).parse(i)
}

// <tyfield> ::= <label> ":" <ty> | <ty>
fn parse_tyfield_tywithlabel(i: Span) -> IResult<Span, (Option<String>, Prg<Type>), ErrorWithPos> {
    let (i, label) = preceded(multispace0, parse_ident_span).parse(i)?;
    let (i, _) = preceded(multispace0, char(':')).parse(i)?;
    let (i, ty) = preceded(multispace0, parse_type_space).parse(i)?;
    Ok((i, (Some(label.st.v), ty)))
}
fn parse_tyfield_ty(i: Span) -> IResult<Span, (Option<String>, Prg<Type>), ErrorWithPos> {
    let (i, ty) = parse_type_space.parse(i)?;
    Ok((i, (None, ty)))
}
fn parse_tyfield(i: Span) -> IResult<Span, (Option<String>, Prg<Type>), ErrorWithPos> {
    alt((parse_tyfield_tywithlabel, parse_tyfield_ty)).parse(i)
}
fn parse_tyfield_withcomma(i: Span) -> IResult<Span, (Option<String>, Prg<Type>), ErrorWithPos> {
    let (i, p) = parse_tyfield.parse(i)?;
    let (i, _) = char(',').parse(i)?;
    Ok((i, p))
}
fn parse_tyfieldseq(i: Span) -> IResult<Span, Vec<TyField>, ErrorWithPos> {
    let (i, fields) = many0(parse_tyfield_withcomma).parse(i)?;
    let (i, last_field) = opt(parse_tyfield).parse(i)?;
    let fields = fields
        .into_iter()
        .chain(last_field)
        .enumerate()
        .map(|(idx, (label, ty))| TyField {
            label: label.unwrap_or(idx.to_string()),
            ty: ty.st,
        })
        .collect();
    Ok((i, fields))
}

/// <tyrecord> ::= "{" <tyinner> "}"
fn parse_tyrecord(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(multispace0, char('{')).parse(i)?;
    let (i, fields) = preceded(multispace0, parse_tyfieldseq).parse(i)?;
    let (i, _) = preceded(multispace0, char('}')).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Type::TyRecord(fields),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: None,
        },
    ))
}

/// <tytagging>::= "<" <tyinner> ">"
fn parse_tytagging(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(multispace0, char('<')).parse(i)?;
    let (i, fields) = preceded(multispace0, parse_tyfieldseq).parse(i)?;
    let (i, _) = preceded(multispace0, char('>')).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Type::TyTagging(fields),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: None,
        },
    ))
}

/// <tyencl> ::= "(" <ty> ")"
fn parse_tyencl(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, _) = char('(').parse(i)?;
    let (i, t) = update_err("type expected", 20, parse_type_space).parse(i)?;
    let (i, _) = chmax_err(
        &t.lasterr,
        update_err("')' expected", 50, with_pos(char(')'))),
    )
    .parse(i)?;
    Ok((i, t))
}

/// <tyatom> ::= <tyencl> | <tyunit> | <tybool> | <tyvar> | <tyrecord> | <tytagging> | <tySelf>
fn parse_tyatom(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    preceded(
        multispace0,
        alt((
            with_pos(map(tag("Nat"), |_| Type::Nat)),
            with_pos(map(tag("Bool"), |_| Type::Bool)),
            with_pos(map(tag("Unit"), |_| Type::Unit)),
            with_pos(map(tag("Self"), |_| Type::TySelf)),
            parse_tytagging,
            parse_tyrecord,
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

fn parse_typrodsub(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, _) = preceded(multispace0, tag("*")).parse(i)?;
    preceded(multispace0, parse_typrod).parse(i)
}
/// <typrod> ::= <tyatom> "*" <typrod> | <tyatom>
/// A*B => {0: A, 1: B}
fn parse_typrod(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, ty1) = preceded(multispace0, parse_tyatom).parse(i)?;
    let (i, tys) = many0(parse_typrodsub).parse(i)?;
    if tys.is_empty() {
        Ok((i, ty1))
    } else {
        let start_pos = (ty1.st.start, ty1.st.line, ty1.st.column);
        Ok((
            i,
            Prg {
                st: Spanned {
                    v: Type::TyRecord(
                        once(ty1.st)
                            .chain(tys.into_iter().map(|t| t.st))
                            .enumerate()
                            .map(|(idx, ty)| TyField {
                                label: idx.to_string(),
                                ty,
                            })
                            .collect::<Vec<_>>(),
                    ),
                    start: start_pos.0,
                    line: start_pos.1,
                    column: start_pos.2,
                },
                lasterr: ty1.lasterr,
            },
        ))
    }
}

/// <tyarrsub> ::= "->" <tyarr>
fn parse_tyarrsub(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, _) = preceded(multispace0, tag("->")).parse(i)?;
    preceded(multispace0, parse_tyarr).parse(i)
}
/// <tyarr> ::= <typrod> "->" <tyarr> | <typrod>
fn parse_tyarr(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, tyatom) = preceded(multispace0, parse_typrod).parse(i)?;
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

/// <tyvariant> ::= <tyvariant> <tyarr> | <label>
fn parse_tyvariant(i: Span) -> IResult<Span, TyField, ErrorWithPos> {
    let (i, label) = preceded(multispace0, parse_ident_span).parse(i)?;
    let (i, args) = many0(preceded(multispace0, parse_tyarr)).parse(i)?;
    let start_pos = if args.is_empty() {
        (i.location_offset(), i.location_line(), i.get_utf8_column())
    } else {
        (args[0].st.start, args[0].st.line, args[0].st.column)
    };
    let ty = args.iter().fold(Type::TySelf, |acc, ty| {
        Type::Arr(
            Box::new(ty.st.clone()),
            Box::new(Spanned {
                v: acc,
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            }),
        )
    });
    Ok((
        i,
        TyField {
            label: label.st.v,
            ty: Spanned {
                v: ty,
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
        },
    ))
}
fn parse_tysumsub(i: Span) -> IResult<Span, TyField, ErrorWithPos> {
    let (i, _) = preceded(multispace0, tag("+")).parse(i)?;
    preceded(multispace0, parse_tyvariant).parse(i)
}
/// <tysum> ::= <tyvariant> "+" <tysum> | <tyvariant> "+" <tyvariant>
fn parse_tysum(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, tyf1) = parse_tyvariant.parse(i)?;
    let (i, tyfs) = if let Type::Arr(_, _) = &tyf1.ty.v {
        many0(parse_tysumsub).parse(i)?
    } else {
        many1(parse_tysumsub).parse(i)?
    };
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Type::TyTagging(once(tyf1).chain(tyfs).collect::<Vec<_>>()),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: None,
        },
    ))
}

/// <tysumorarr> ::= <tysum> | <tyarr>
fn parse_tysumorarr(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    alt((parse_tysum, parse_tyarr)).parse(i)
}

/// <ty> ::= <tysum>
fn parse_type(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    parse_tysumorarr.parse(i)
}

fn parse_type_space(i: Span) -> IResult<Span, Prg<Type>, ErrorWithPos> {
    let (i, ty) = parse_type.parse(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, ty))
}

/// <pattagging> ::= <ty> ":::" <parse_labelorindexcase <Bool, Unit>:::0 true of | <Bool, Unit>:::0 b:Bool => true | <Bool, Unit>:::1 u:Unit => false> | <pattagging> <pat>
fn parse_pattagging_args(i: Span) -> IResult<Span, Prg<PTmpTag>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, ty) = preceded(multispace0, parse_type_space).parse(i)?;
    let (i, _) = preceded(multispace0, tag(":::")).parse(i)?;
    let (i, label) = preceded(multispace0, parse_labelorindex_span).parse(i)?;
    let (i, args) = many0(preceded(multispace0, parse_ident_span)).parse(i)?;
    let args_values: Vec<String> = args.into_iter().map(|arg| arg.st.v).collect();
    Ok((
        i,
        Prg {
            st: Spanned {
                v: PTmpTag {
                    ty: ty.st.v,
                    label: label.st.v,
                    nargs: args_values,
                },
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: ty.lasterr.or(label.lasterr),
        },
    ))
}

/// <patfield> ::= <label> ":" <pat> | <pat>
fn parse_patfield_patwithlabel(
    i: Span,
) -> IResult<Span, (Option<String>, Prg<Pattern>), ErrorWithPos> {
    let (i, label) = preceded(multispace0, parse_ident_span).parse(i)?;
    let (i, _) = preceded(multispace0, char(':')).parse(i)?;
    let (i, pat) = preceded(multispace0, parse_pat).parse(i)?;
    Ok((i, (Some(label.st.v), pat)))
}
fn parse_patfield_pat(i: Span) -> IResult<Span, (Option<String>, Prg<Pattern>), ErrorWithPos> {
    let (i, pat) = parse_pat.parse(i)?;
    Ok((i, (None, pat)))
}
fn parse_patfield(i: Span) -> IResult<Span, (Option<String>, Prg<Pattern>), ErrorWithPos> {
    alt((parse_patfield_patwithlabel, parse_patfield_pat)).parse(i)
}
fn parse_patfield_withcomma(
    i: Span,
) -> IResult<Span, (Option<String>, Prg<Pattern>), ErrorWithPos> {
    let (i, p) = parse_patfield.parse(i)?;
    let (i, _) = char(',').parse(i)?;
    Ok((i, p))
}

/// <patfieldseq> ::= <patfield> "," <patfieldseq> | null
fn parse_patfieldseq(i: Span) -> IResult<Span, Prg<Vec<PatField>>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, fields) = many0(parse_patfield_withcomma).parse(i)?;
    let (i, last_field) = opt(parse_patfield).parse(i)?;
    let pfs = fields
        .into_iter()
        .chain(last_field)
        .enumerate()
        .map(|(idx, (label, pat))| PatField {
            label: label.unwrap_or(idx.to_string()),
            pat: pat.st.v,
        })
        .collect();
    Ok((
        i,
        Prg {
            st: Spanned {
                v: pfs,
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: None,
        },
    ))
}

/// <patrecord> ::= "{" <patinner> "}"
fn parse_patrecord(i: Span) -> IResult<Span, Prg<Pattern>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(multispace0, char('{')).parse(i)?;
    let (i, fields) = preceded(multispace0, parse_patfieldseq).parse(i)?;
    let (i, _) = preceded(multispace0, char('}')).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Pattern::Record(fields.st.v),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: fields.lasterr,
        },
    ))
}

/// <patvar> ::= <bound> ":" <ty>
fn parse_patvar(i: Span) -> IResult<Span, Prg<Pattern>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, label) = preceded(multispace0, parse_ident_span).parse(i)?;
    let (i, _) = preceded(multispace0, char(':')).parse(i)?;
    let (i, ty) = preceded(multispace0, parse_type).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Pattern::Var(label.st.v, ty.st.v),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: label.lasterr.or(ty.lasterr),
        },
    ))
}

/// <pat> ::= <patvar> | <patrecord> | <pattagging>
fn parse_pat(i: Span) -> IResult<Span, Prg<Pattern>, ErrorWithPos> {
    preceded(
        multispace0,
        alt((
            parse_patvar,
            map(parse_pattagging_args, |ptag| Prg {
                st: Spanned {
                    v: Pattern::TmpTagging(ptag.st.v),
                    start: ptag.st.start,
                    line: ptag.st.line,
                    column: ptag.st.column,
                },
                lasterr: ptag.lasterr,
            }),
            parse_patrecord,
        )),
    )
    .parse(i)
}

/// <zero> ::= "zero"
fn parse_zero(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    with_pos(map(parse_reserved_span("zero"), |_| Term::Zero)).parse(i)
}

/// <false> ::= "false"
fn parse_false(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    with_pos(map(parse_reserved_span("false"), |_| Term::False)).parse(i)
}

/// <true> ::= "true"
fn parse_true(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    with_pos(map(parse_reserved_span("true"), |_| Term::True)).parse(i)
}

// <unit> ::= "unit"
fn parse_unit(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    with_pos(map(parse_reserved_span("unit"), |_| Term::Unit)).parse(i)
}

// <var> ::= number | string
fn parse_varnum(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (_i, _check) = alphanumeric1.parse(i)?;
    with_pos(map_res(digit1, |s: Span| {
        s.fragment().parse::<usize>().map(Term::Var)
    }))
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
/// <var> ::= number | string
fn parse_var(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    preceded(multispace0, alt((parse_varnum, parse_varstr))).parse(i)
}

/// <succ> ::= "succ" <term>
fn parse_succ(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = parse_reserved_span("succ").parse(i)?;
    let (i, t) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::Succ(Box::new(t.st)),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: t.lasterr,
        },
    ))
}

/// <pred> ::= "pred" <term>
fn parse_pred(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = parse_reserved_span("pred").parse(i)?;
    let (i, t) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::Pred(Box::new(t.st)),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: t.lasterr,
        },
    ))
}

/// <iszero> ::= "iszero" <term>
fn parse_iszero(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = parse_reserved_span("iszero").parse(i)?;
    let (i, t) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::IsZero(Box::new(t.st)),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: t.lasterr,
        },
    ))
}

/// <tagging> ::= <ty> ":::" <labelorindex>
fn parse_tagging(i: Span) -> IResult<Span, Tag, ErrorWithPos> {
    let (i, ty) = preceded(multispace0, parse_tyatom).parse(i)?;
    let (i, _) = preceded(multispace0, tag(":::")).parse(i)?;
    let (i, label) = preceded(multispace0, parse_labelorindex_span).parse(i)?;
    Ok((
        i,
        Tag {
            ty: ty.st.v,
            label: label.st.v,
        },
    ))
}

/// <field> ::= <label> "=" <term> | <term>
fn parse_field_twithlabel(i: Span) -> IResult<Span, (Option<String>, Prg<Term>), ErrorWithPos> {
    let (i, label) = preceded(multispace0, parse_ident_span).parse(i)?;
    let (i, _) = preceded(multispace0, char('=')).parse(i)?;
    let (i, term) = preceded(multispace0, parse_term_space).parse(i)?;
    Ok((i, (Some(label.st.v), term)))
}
fn parse_field_t(i: Span) -> IResult<Span, (Option<String>, Prg<Term>), ErrorWithPos> {
    let (i, t) = parse_term_space.parse(i)?;
    Ok((i, (None, t)))
}
fn parse_field(i: Span) -> IResult<Span, (Option<String>, Prg<Term>), ErrorWithPos> {
    alt((parse_field_twithlabel, parse_field_t)).parse(i)
}
fn parse_field_withcomma(i: Span) -> IResult<Span, (Option<String>, Prg<Term>), ErrorWithPos> {
    let (i, p) = parse_field.parse(i)?;
    let (i, _) = char(',').parse(i)?;
    Ok((i, p))
}

// <fieldseq> ::= <field> "," <fieldseq> | null
fn parse_fieldseq(i: Span) -> IResult<Span, Vec<Field>, ErrorWithPos> {
    let (i, fields) = many0(parse_field_withcomma).parse(i)?;
    let (i, last_field) = opt(parse_field).parse(i)?;
    let fields = fields
        .into_iter()
        .chain(last_field)
        .enumerate()
        .map(|(idx, (label, term))| Field {
            label: label.unwrap_or(idx.to_string()),
            term: term.st,
        })
        .collect();
    Ok((i, fields))
}

// <record> ::= "{" <inner> "}"
fn parse_record(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(multispace0, char('{')).parse(i)?;
    let (i, fields) = preceded(multispace0, parse_fieldseq).parse(i)?;
    let (i, _) = preceded(multispace0, char('}')).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::Record(fields),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: None,
        },
    ))
}

fn parse_plet(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = parse_reserved_span("let").parse(i)?;
    let (i, p) = update_err("pattern expected", 20, preceded(multispace0, parse_pat)).parse(i)?;
    let (i, _) = preceded(multispace0, tag("=")).parse(i)?;
    let (i, t1) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    let (i, _) = parse_reserved_span("in").parse(i)?;
    let (i, t2) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    let (t2_renamed, t1_renamed, p_renamed, _n) =
        t2.st.v.subst_pat(&t1.st.v, &p.st.v, 0).map_err(|e| {
            nom::Err::Error(ErrorWithPos {
                message: format!("subst_pat: {}", e),
                level: 90,
                kind: Some(nom::error::ErrorKind::Fail),
                line: i.location_line(),
                column: i.get_utf8_column(),
            })
        })?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::Plet(
                    Spanned {
                        v: p_renamed,
                        start: p.st.start,
                        line: p.st.line,
                        column: p.st.column,
                    },
                    Box::new(Spanned {
                        v: t1_renamed,
                        start: t1.st.start,
                        line: t1.st.line,
                        column: t1.st.column,
                    }),
                    Box::new(Spanned {
                        v: t2_renamed,
                        start: t2.st.start,
                        line: t2.st.line,
                        column: t2.st.column,
                    }),
                ),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: p.lasterr.or(t1.lasterr).or(t2.lasterr),
        },
    ))
}

/// <let> ::= "let" <bound> "=" <term> "in" <term>
fn parse_let(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = parse_reserved_span("let").parse(i)?;
    let (i, name) = update_err(
        "identifier expected",
        50,
        preceded(multispace0, parse_ident_span),
    )
    .parse(i)?;
    let (i, _) = preceded(multispace0, tag("=")).parse(i)?;
    let (i, t1) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    let (i, _) = parse_reserved_span("in").parse(i)?;
    let (i, t2) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    let renamed = t2.st.v.subst_name(name.st.v.as_str());
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::Let(
                    Box::new(t1.st),
                    Box::new(Spanned {
                        v: renamed,
                        start: t2.st.start,
                        line: t2.st.line,
                        column: t2.st.column,
                    }),
                ),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: name.lasterr.or(t1.lasterr).or(t2.lasterr),
        },
    ))
}

/// <if> ::= "if" <term> "then" <term> "else" <term>
fn parse_if(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = parse_reserved_span("if").parse(i)?;
    let (i, t1) =
        update_err("condition expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    let (i, _) = parse_reserved_span("then").parse(i)?;
    let (i, t2) = update_err(
        "then branch expected",
        20,
        preceded(multispace0, parse_term),
    )
    .parse(i)?;
    let (i, _) = parse_reserved_span("else").parse(i)?;
    let (i, t3) = chmax_err(
        &t1.lasterr.or(t2.lasterr),
        update_err(
            "else branch expected",
            20,
            preceded(multispace0, parse_term),
        ),
    )
    .parse(i)?;

    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::If(Box::new(t1.st), Box::new(t2.st), Box::new(t3.st)),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: t3.lasterr,
        },
    ))
}

fn parse_arm(i: Span) -> IResult<Span, Prg<Arm>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(multispace0, char('|')).parse(i)?;
    let (i, ptag) = preceded(multispace0, parse_pattagging_args)
        .parse(i)
        .map_err(|_| {
            nom::Err::Error(ErrorWithPos {
                message: "Error parsing case arm: expected <type>:::<label> <args...>".to_string(),
                level: 90,
                kind: Some(nom::error::ErrorKind::Fail),
                line: i.location_line(),
                column: i.get_utf8_column(),
            })
        })?;
    let (i, _) = preceded(multispace0, tag("=>")).parse(i)?;
    let (i, t) = preceded(multispace0, parse_term).parse(i)?;

    let (t_renamed, ptag_renamed, _) = t.st.v.subst_ptag(&ptag.st.v, 0).map_err(|e| {
        nom::Err::Error(ErrorWithPos {
            message: format!("subst_ptag: {}", e),
            level: 90,
            kind: Some(nom::error::ErrorKind::Fail),
            line: i.location_line(),
            column: i.get_utf8_column(),
        })
    })?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Arm {
                    term: Spanned {
                        v: t_renamed,
                        start: t.st.start,
                        line: t.st.line,
                        column: t.st.column,
                    },
                    ptag: ptag_renamed,
                },
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: ptag.lasterr.or(t.lasterr),
        },
    ))
}
/// <arms> ::= "|" <pat> "=>" <term> <arms> | null
fn parse_arms(i: Span) -> IResult<Span, Prg<Vec<Arm>>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, arms) = many1(parse_arm).parse(i)?;
    let arms_values: Vec<Arm> = arms.into_iter().map(|arm| arm.st.v).collect();
    Ok((
        i,
        Prg {
            st: Spanned {
                v: arms_values,
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: None,
        },
    ))
}

// <case> ::= "case" <term> "of" <arms>
fn parse_case(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = parse_reserved_span("case").parse(i)?;
    let (i, t) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    let (i, _) = parse_reserved_span("of").parse(i)?;
    let (i, arms) =
        update_err("case arms expected", 20, preceded(multispace0, parse_arms)).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::Case(Box::new(t.st), arms.st.v),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: t.lasterr.or(arms.lasterr),
        },
    ))
}

/// <lettype> ::= "type" <ident> "=" <type> "in" <term>
fn parse_lettype(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (i, _) = parse_reserved_span("type").parse(i)?;
    let (i, name) = update_err(
        "identifier expected",
        20,
        preceded(multispace0, parse_ident_span),
    )
    .parse(i)?;
    let (i, _) = preceded(multispace0, tag("=")).parse(i)?;
    let (i, ty) =
        update_err("type expected", 20, preceded(multispace0, parse_type_space)).parse(i)?;
    let (i, _) = parse_reserved_span("in").parse(i)?;
    let (i, t) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    let renamed = t.st.v.subst_type_name(name.st.v.as_str(), &ty.st.v);
    Ok((
        i,
        Prg {
            st: Spanned {
                v: renamed,
                start: t.st.start,
                line: t.st.line,
                column: t.st.column,
            },
            lasterr: name.lasterr.or(ty.lasterr).or(t.lasterr),
        },
    ))
}

/// <fix> ::= "fix" <term>
fn parse_fix(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = parse_reserved_span("fix").parse(i)?;
    let (i, t) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::Fix(Box::new(t.st)),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: t.lasterr,
        },
    ))
}

/// <letrec> ::= "letrec" <bound> ":" <ty> "=" <term> "in" <term>
fn parse_letrec(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = parse_reserved_span("letrec").parse(i)?;
    let (i, name) = update_err(
        "identifier expected",
        20,
        preceded(multispace0, parse_ident_span),
    )
    .parse(i)?;
    let (i, _) = preceded(multispace0, char(':')).parse(i)?;
    let (i, ty) =
        update_err("type expected", 20, preceded(multispace0, parse_type_space)).parse(i)?;
    let (i, _) = preceded(multispace0, tag("=")).parse(i)?;
    let (i, t1) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    let (i, _) = parse_reserved_span("in").parse(i)?;
    let (i, t2) = update_err("term expected", 20, preceded(multispace0, parse_term)).parse(i)?;
    let t1_renamed = t1.st.v.subst_name(name.st.v.as_str());
    let t2_renamed = t2.st.v.subst_name(name.st.v.as_str());
    Ok((
        i,
        Prg {
            st: Spanned {
                v: Term::Let(
                    Box::new(Spanned {
                        v: Term::Fix(Box::new(Spanned {
                            v: Term::Abs(
                                ty.st.v,
                                Box::new(Spanned {
                                    v: t1_renamed,
                                    start: t1.st.start,
                                    line: t1.st.line,
                                    column: t1.st.column,
                                }),
                            ),
                            start: ty.st.start,
                            line: ty.st.line,
                            column: ty.st.column,
                        })),
                        start: ty.st.start,
                        line: ty.st.line,
                        column: ty.st.column,
                    }),
                    Box::new(Spanned {
                        v: t2_renamed,
                        start: t2.st.start,
                        line: t2.st.line,
                        column: t2.st.column,
                    }),
                ),
                start: start_pos.0,
                line: start_pos.1,
                column: start_pos.2,
            },
            lasterr: name.lasterr.or(ty.lasterr).or(t1.lasterr).or(t2.lasterr),
        },
    ))
}

/// <abs> ::= "\:" <ty> "." <term> | "\" <bound> ":" <ty> "." <term>
fn parse_abs(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let start_pos = (i.location_offset(), i.location_line(), i.get_utf8_column());
    let (i, _) = preceded(char('\\'), multispace0).parse(i)?;
    let (i, name) = opt(parse_ident_span).parse(i)?;
    let (i, _) = preceded(preceded(multispace0, char(':')), multispace0).parse(i)?;
    let (i, ty) = update_err("type expected", 20, parse_type_space).parse(i)?;
    let (i, _) = chmax_err(
        &ty.lasterr,
        update_err("'.' expected", 50, with_pos(char('.'))),
    )
    .parse(i)?;
    let (i, t) = chmax_err(&ty.lasterr, update_err("term expected", 20, parse_term)).parse(i)?;

    let result = match name.clone() {
        Some(name) => {
            let renamed = t.st.v.subst_name(&name.st.v);
            Term::Abs(
                ty.st.v,
                Box::new(Spanned {
                    v: renamed,
                    start: t.st.start,
                    line: t.st.line,
                    column: t.st.column,
                }),
            )
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
            lasterr: name
                .as_ref()
                .and_then(|n| n.lasterr.clone())
                .or(ty.lasterr)
                .or(t.lasterr),
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

/// <atom> ::= <encl> | <abs> | <if> | <let> | <let> | <case> | <var> | <unit> | <true> | <false> | <record> | <tagging>
fn parse_atom(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    preceded(
        multispace0,
        alt((
            parse_letrec,
            parse_fix,
            parse_lettype,
            parse_case,
            parse_plet, // "let" pattern-based let
            parse_let,  // "let" simple let
            parse_if,
            parse_succ,
            parse_pred,
            parse_iszero,
            with_pos(map(parse_tagging, Term::Tagging)),
            parse_record,
            parse_zero,
            parse_false,
            parse_true,
            parse_unit,
            parse_abs,
            parse_encl,
            parse_var, // Must be last to avoid conflicting with reserved words
        )),
    )
    .parse(i)
}

// <projection> ::= "." <labelorindex>
fn parse_projection(i: Span) -> IResult<Span, String, ErrorWithPos> {
    let (i, label) = preceded(
        multispace0,
        preceded(preceded(char('.'), multispace0), parse_labelorindex_span),
    )
    .parse(i)?;
    Ok((i, label.st.v))
}

// <postfix> ::= <atom> <projection> | <atom>
fn parse_postfix(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (i, first) = parse_atom.parse(i)?;
    let (i, rest) = many0(parse_projection).parse(i)?;
    let lasterr = first.lasterr.clone();
    let t = rest.into_iter().fold(first.st, |acc, label| Spanned {
        v: Term::Projection(Box::new(acc.clone()), label),
        start: acc.start,
        line: acc.line,
        column: acc.column,
    });
    Ok((i, Prg { st: t, lasterr }))
}

// <app> ::= <postfix> <app> | <postfix>
fn parse_app(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (i, first) = parse_postfix.parse(i)?;
    let (i, rest) = many0(parse_postfix).parse(i)?;
    if let Err(lasterr) = parse_postfix.parse(i) {
        match lasterr {
            nom::Err::Error(lasterr) => {
                let st = rest.into_iter().fold(first.st, |acc, arg| Spanned {
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

// <seq> ::= <app> ";" <seq> | <app>
fn parse_seq(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (i, first) = parse_app.parse(i)?;
    let (i, rest) = many0(preceded(multispace0, preceded(char(';'), parse_seq))).parse(i)?;
    let lasterr = first.lasterr.clone();
    let t = rest.into_iter().fold(first.st, |acc, t| Spanned {
        v: Term::App(
            Box::new(Spanned {
                v: Term::Abs(
                    Type::Unit,
                    Box::new(Spanned {
                        v: t.st.v.shift(1).unwrap_or(t.st.v),
                        start: t.st.start,
                        line: t.st.line,
                        column: t.st.column,
                    }),
                ),
                start: acc.start,
                line: acc.line,
                column: acc.column,
            }),
            Box::new(acc.clone()),
        ),
        start: acc.start,
        line: acc.line,
        column: acc.column,
    });
    Ok((i, Prg { st: t, lasterr }))
}

// <term> ::= <seq>
fn parse_term(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    preceded(multispace0, parse_seq).parse(i)
}

fn parse_term_space(i: Span) -> IResult<Span, Prg<Term>, ErrorWithPos> {
    let (i, t) = parse_term(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, t))
}

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

pub fn display_position(input: &str, line: u32, column: usize) -> String {
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
    result.push('^');
    result
}

// Wrapper for position-aware parsing (will be implemented later)
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

// Helper function to convert from string to Span and parse with position info
pub fn parse_spanned_and_render_err(input: &str) -> Result<Spanned<Term>, (String, String)> {
    match parse_spanned(input) {
        Ok(term) => Ok(term),
        Err(err) => {
            let error_display = display_position(input, err.line, err.column);
            Err((err.to_string(), error_display))
        }
    }
}

mod tests {
    use rstest::rstest;

    #[allow(unused)]
    use crate::{
        parser::parse,
        span::{Spanned, dummy_spanned},
        syntax::{
            pattern::{PTmpTag, PatField, Pattern},
            term::{Arm, Field, Tag, Term},
            r#type::{TyField, Type},
        },
    };

    #[allow(unused)]
    // Helper function to create Spanned<T> for testing
    fn spanned<T>(value: T) -> Spanned<T> {
        Spanned {
            v: value,
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
            Term::Abs(ty, t) => Term::Abs(
                extract_type_structure(ty),
                Box::new(spanned(extract_term_structure(&t.v))),
            ),
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
            Term::Zero => Term::Zero,
            Term::Succ(t) => Term::Succ(Box::new(spanned(extract_term_structure(&t.v)))),
            Term::Pred(t) => Term::Pred(Box::new(spanned(extract_term_structure(&t.v)))),
            Term::IsZero(t) => Term::IsZero(Box::new(spanned(extract_term_structure(&t.v)))),
            Term::Unit => Term::Unit,
            Term::Record(fields) => Term::Record(
                fields
                    .iter()
                    .map(|field| Field {
                        label: field.label.clone(),
                        term: spanned(extract_term_structure(&field.term.v)),
                    })
                    .collect(),
            ),
            Term::Projection(t, label) => Term::Projection(
                Box::new(spanned(extract_term_structure(&t.v))),
                label.clone(),
            ),
            Term::Let(t1, t2) => Term::Let(
                Box::new(spanned(extract_term_structure(&t1.v))),
                Box::new(spanned(extract_term_structure(&t2.v))),
            ),
            Term::Plet(p, t1, t2) => Term::Plet(
                spanned(extract_pattern_structure(&p.v)),
                Box::new(spanned(extract_term_structure(&t1.v))),
                Box::new(spanned(extract_term_structure(&t2.v))),
            ),
            Term::Tagging(tag) => Term::Tagging(Tag {
                ty: extract_type_structure(&tag.ty),
                label: tag.label.clone(),
            }),
            Term::Case(t, arms) => Term::Case(
                Box::new(spanned(extract_term_structure(&t.v))),
                arms.iter()
                    .map(|arm| Arm {
                        term: spanned(extract_term_structure(&arm.term.v)),
                        ptag: extract_ptmptag_structure(&arm.ptag),
                    })
                    .collect(),
            ),
            Term::Fix(t) => Term::Fix(Box::new(spanned(extract_term_structure(&t.v)))),
        }
    }

    #[allow(unused)]
    // Helper function to extract just the type structure, ignoring position info
    fn extract_type_structure(ty: &Type) -> Type {
        match ty {
            Type::Bool => Type::Bool,
            Type::Nat => Type::Nat,
            Type::Unit => Type::Unit,
            Type::TySelf => Type::TySelf,
            Type::TyVar(s) => Type::TyVar(s.clone()),
            Type::Arr(t1, t2) => Type::Arr(
                Box::new(spanned(extract_type_structure(&t1.v))),
                Box::new(spanned(extract_type_structure(&t2.v))),
            ),
            Type::TyRecord(fields) => Type::TyRecord(
                fields
                    .iter()
                    .map(|field| TyField {
                        label: field.label.clone(),
                        ty: spanned(extract_type_structure(&field.ty.v)),
                    })
                    .collect(),
            ),
            Type::TyTagging(fields) => Type::TyTagging(
                fields
                    .iter()
                    .map(|field| TyField {
                        label: field.label.clone(),
                        ty: spanned(extract_type_structure(&field.ty.v)),
                    })
                    .collect(),
            ),
        }
    }

    #[allow(unused)]
    // Helper function to extract just the PTmpTag structure, ignoring position info
    fn extract_ptmptag_structure(ptag: &PTmpTag) -> PTmpTag {
        PTmpTag {
            ty: extract_type_structure(&ptag.ty),
            label: ptag.label.clone(),
            nargs: ptag.nargs.clone(),
        }
    }

    #[allow(unused)]
    // Helper function to extract just the pattern structure, ignoring position info
    fn extract_pattern_structure(pat: &Pattern) -> Pattern {
        match pat {
            Pattern::Var(s, ty) => Pattern::Var(s.clone(), extract_type_structure(ty)),
            Pattern::Record(fields) => Pattern::Record(
                fields
                    .iter()
                    .map(|field| PatField {
                        label: field.label.clone(),
                        pat: extract_pattern_structure(&field.pat),
                    })
                    .collect(),
            ),
            Pattern::TmpTagging(ptag) => Pattern::TmpTagging(extract_ptmptag_structure(ptag)),
        }
    }

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
                Box::new(dummy_spanned(Term::Abs(
                    Type::Bool,
                    Box::new(dummy_spanned(Term::Unit))
                ))),
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
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0)))
            ))),
            Box::new(dummy_spanned(Term::Var(1)))
        ))
    )]
    #[case(
        r"(\ : Bool.0) (\   : Bool . 0 )",
        Some(Term::App(
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0)))
            ))),
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0)))
            )))
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
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0)))
            )))
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
        dummy_spanned(Pattern::Var("0".to_string(), Type::Bool)),
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
        let result = parse(input).ok().map(|term| extract_term_structure(&term));
        assert_eq!(result, expected);
    }
}
