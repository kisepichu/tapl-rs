use std::iter::once;

use crate::{
    span::{ErrorWithPos, Spanned, dummy_spanned},
    syntax::{
        context::Context,
        pattern::{PTmpTag, Pattern},
        pattype::PatType,
        term::{Tag, Term},
        r#type::{TyField, Type},
    },
};

// Helper function to compare types ignoring position information
fn types_equal_ignore_pos(ty1: &Type, ty2: &Type) -> bool {
    match (ty1, ty2) {
        (Type::Bool, Type::Bool) => true,
        (Type::Nat, Type::Nat) => true,
        (Type::Unit, Type::Unit) => true,
        (Type::TySelf, Type::TySelf) => true,
        (Type::TyVar(s1), Type::TyVar(s2)) => s1 == s2,
        (Type::Arr(t1a, t1b), Type::Arr(t2a, t2b)) => {
            types_equal_ignore_pos(&t1a.v, &t2a.v) && types_equal_ignore_pos(&t1b.v, &t2b.v)
        }
        (Type::TyRecord(fields1), Type::TyRecord(fields2)) => {
            fields1.len() == fields2.len()
                && fields1.iter().zip(fields2.iter()).all(|(f1, f2)| {
                    f1.label == f2.label && types_equal_ignore_pos(&f1.ty.v, &f2.ty.v)
                })
        }
        (Type::TyTagging(fields1), Type::TyTagging(fields2)) => {
            fields1.len() == fields2.len()
                && fields1.iter().zip(fields2.iter()).all(|(f1, f2)| {
                    f1.label == f2.label && types_equal_ignore_pos(&f1.ty.v, &f2.ty.v)
                })
        }
        _ => false,
    }
}

pub fn type_of(ctx: &Context, t: &Spanned<Term>) -> Result<Type, ErrorWithPos> {
    match &t.v {
        Term::Var(xn) => {
            // println!("ctx = \n{}", ctx);
            match ctx.get(*xn) {
                Some(ty) => Ok(ty.clone()),
                None => Err(ErrorWithPos {
                    message: format!("type check failed: {}\n: unbound variable {}", t.v, xn),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                }),
            }
        }
        Term::TmpVar(s) => Err(ErrorWithPos {
            message: format!("type check failed: {}\n: undefined variable: {}", t.v, s),
            level: 80,
            kind: None,
            line: t.line,
            column: t.column,
        }),
        Term::Abs(ty1, t2) => {
            let ctx_ = ctx.clone().shift_and_push0(ty1.clone());
            let ty2 = type_of(&ctx_, t2)?;
            Ok(Type::Arr(
                Box::new(dummy_spanned(ty1.clone())),
                Box::new(dummy_spanned(ty2.clone())),
            ))
        }
        Term::App(t1, t2) => {
            let ty1 = type_of(ctx, t1)?;
            let ty2 = type_of(ctx, t2)?;
            match ty1 {
                Type::Arr(ty11, ty12) => {
                    if types_equal_ignore_pos(&ty11.v, &ty2) {
                        Ok(ty12.v.clone())
                    } else {
                        let t1_str = t1.v.to_string();
                        let t1_str = if t1_str.len() <= 20 {
                            t1_str
                        } else {
                            t1_str[..20].to_string() + "..."
                        };
                        Err(ErrorWithPos {
                            message: format!(
                                "type check failed: {}\ntype of argument to the term {} is incorrect:\n  expected: {}, found: {}: {}",
                                t.v, t1_str, ty11.v, t2.v, ty2
                            ),
                            level: 90,
                            kind: None,
                            line: t.line,
                            column: t.column,
                        })
                    }
                }
                _ => Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected arrow type, but found {}: {}",
                        t.v, t1.v, ty1
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                }),
            }
        }
        Term::Unit => Ok(Type::Unit),
        Term::True => Ok(Type::Bool),
        Term::False => Ok(Type::Bool),
        Term::Zero => Ok(Type::Nat),
        Term::Succ(t1) => {
            let ty1 = type_of(ctx, t1)?;
            if types_equal_ignore_pos(&ty1, &Type::Nat) {
                Ok(Type::Nat)
            } else {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected Nat type, but found {}: {}",
                        t.v, t1.v, ty1
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                })
            }
        }
        Term::Pred(t1) => {
            let ty1 = type_of(ctx, t1)?;
            if types_equal_ignore_pos(&ty1, &Type::Nat) {
                Ok(Type::Nat)
            } else {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected Nat type, but found {}: {}",
                        t.v, t1.v, ty1
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                })
            }
        }
        Term::IsZero(t1) => {
            let ty1 = type_of(ctx, t1)?;
            if types_equal_ignore_pos(&ty1, &Type::Nat) {
                Ok(Type::Bool)
            } else {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected Nat type, but found {}: {}",
                        t.v, t1.v, ty1
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                })
            }
        }
        Term::Record(fields) => {
            let tyfields = fields
                .iter()
                .map(|field| {
                    let ty = type_of(ctx, &field.term)?;
                    Ok(TyField {
                        label: field.label.clone(),
                        ty: dummy_spanned(ty),
                    })
                })
                .collect::<Result<Vec<_>, ErrorWithPos>>()?;
            Ok(Type::TyRecord(tyfields))
        }
        Term::Projection(t1, label) => {
            let ty1 = type_of(ctx, t1)?;
            if let Type::TyRecord(fields) = ty1.clone() {
                fields
                    .iter()
                    .find(|field| field.label == *label)
                    .map(|field| field.ty.v.clone())
                    .ok_or_else(|| ErrorWithPos {
                        message: format!(
                            "type check failed: {}\n  field {} not found in record type {}",
                            t.v, label, ty1
                        ),
                        level: 90,
                        kind: None,
                        line: t.line,
                        column: t.column,
                    })
            } else {
                // println!("ctx = \n{}", ctx);

                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected record type, but found {}: {}",
                        t.v, t1.v, ty1
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                })
            }
        }
        Term::If(t1, t2, t3) => {
            let ty1 = type_of(ctx, t1)?;
            let ty2 = type_of(ctx, t2)?;
            let ty3 = type_of(ctx, t3)?;
            if types_equal_ignore_pos(&ty1, &Type::Bool) && types_equal_ignore_pos(&ty2, &ty3) {
                Ok(ty2)
            } else if !types_equal_ignore_pos(&ty2, &ty3) {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  arms of conditional have different types:\n  {}, {}",
                        t.v, ty2, ty3
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                })
            } else {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected Bool, but found {}: {}",
                        t.v, t1.v, ty1
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                })
            }
        }
        Term::Let(t1, t2) => {
            let ty1 = type_of(ctx, t1)?;
            let ctx_ = ctx.clone().shift_and_push0(ty1);
            type_of(&ctx_, t2)
        }
        Term::Plet(p, t1, t2) => {
            let ty1 = type_of(ctx, t1)?;
            let pty = pat_type_of(ctx, &p.v)?;
            let ctx_ = pty.context.clone();
            if !types_equal_ignore_pos(&pty.ty, &ty1) {
                return Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  pattern type {} does not match term type {}",
                        t.v, pty.ty, ty1
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                });
            }
            type_of(&ctx_, t2)
        }
        Term::Tagging(Tag { ty, label }) => {
            if let Type::TyTagging(tyfields) = ty {
                let ty_ = tyfields
                    .iter()
                    .find(|field| field.label == *label)
                    .map(|field| field.ty.clone())
                    .ok_or_else(|| ErrorWithPos {
                        message: format!(
                            "type check failed: {}\n  tag {} not found in tagging type {}",
                            t.v, label, ty
                        ),
                        level: 90,
                        kind: None,
                        line: t.line,
                        column: t.column,
                    })?;
                fn replace_self(ty: &Type, replace: &Type) -> Type {
                    match ty {
                        Type::TySelf => replace.clone(),
                        Type::Arr(t1, t2) => Type::Arr(
                            Box::new(dummy_spanned(replace_self(&t1.v, replace))),
                            Box::new(dummy_spanned(replace_self(&t2.v, replace))),
                        ),
                        Type::TyRecord(fields) => Type::TyRecord(
                            fields
                                .iter()
                                .map(|f| TyField {
                                    label: f.label.clone(),
                                    ty: dummy_spanned(replace_self(&f.ty.v, replace)),
                                })
                                .collect(),
                        ),
                        _ => ty.clone(),
                    }
                }
                Ok(replace_self(&ty_.v, ty))
            } else {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected tagging type, but found {}",
                        t.v, ty
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                })
            }
        }
        Term::Case(t_case, arms) => {
            let tyt = type_of(ctx, t_case)?;
            match tyt.clone() {
                Type::TyTagging(tyfields) => {
                    // none when arms.len() == 0
                    let mut t_: Option<(Term, Type)> = None;
                    for (arm, tyf) in arms.iter().zip(tyfields) {
                        let ptyarm = pat_type_of(ctx, &Pattern::TmpTagging(arm.ptag.clone()))?;

                        {
                            if arm.ptag.label != tyf.label {
                                return Err(ErrorWithPos {
                                    message: format!(
                                        "type check failed: {}\n, case expression currently requires exact ordering of labels.\n  expected {}, but found {}",
                                        t.v, tyf.label, arm.ptag.label
                                    ),
                                    level: 90,
                                    kind: None,
                                    line: t.line,
                                    column: t.column,
                                });
                            }
                        }
                        if !types_equal_ignore_pos(&ptyarm.ty, &tyt) {
                            return Err(ErrorWithPos {
                                message: format!(
                                    "type check failed: {}\n  pattern type {} does not match term type {}",
                                    t.v, ptyarm.ty, tyt
                                ),
                                level: 90,
                                kind: None,
                                line: t.line,
                                column: t.column,
                            });
                        }
                        let ctx_ = ptyarm.context.clone();
                        let tyarm = type_of(&ctx_, &arm.term)?;

                        if t_.is_none() {
                            t_ = Some((arm.term.v.clone(), tyarm));
                        } else if let Some((_, tyt_)) = &t_ {
                            if !types_equal_ignore_pos(tyt_, &tyarm) {
                                return Err(ErrorWithPos {
                                    message: format!(
                                        "type check failed: {}\n  arms of case expression have different types:\n  {}: {},\n  {}: {}",
                                        t.v,
                                        if let Some((tt_, _)) = &t_ {
                                            tt_
                                        } else {
                                            panic!(
                                                "unreachable because arms.len() == 0 && 2 <= arms.len()"
                                            )
                                        },
                                        t_.clone().expect("in let Some block").1,
                                        arm.term.v,
                                        tyarm
                                    ),
                                    level: 90,
                                    kind: None,
                                    line: t.line,
                                    column: t.column,
                                });
                            }
                        }
                    }
                    Ok(t_.expect("in let Some block").1)
                }
                _ => Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  case expressions currently only support tagging type.\n  expected tagging type, but found {}",
                        t.v, tyt
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                }),
            }
        }
        Term::Fix(t1) => {
            let ty1 = type_of(ctx, t1)?;
            if let Type::Arr(ty11, ty12) = ty1 {
                if types_equal_ignore_pos(&ty11.v, &ty12.v) {
                    Ok(ty11.v.clone())
                } else {
                    Err(ErrorWithPos {
                        message: format!(
                            "type check failed: {}\n  expected arrow type with same input and output types, but found {}: {}",
                            t.v,
                            t1.v,
                            Type::Arr(ty11, ty12)
                        ),
                        level: 90,
                        kind: None,
                        line: t.line,
                        column: t.column,
                    })
                }
            } else {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected arrow type, but found {}: {}",
                        t.v, t1.v, ty1
                    ),
                    level: 90,
                    kind: None,
                    line: t.line,
                    column: t.column,
                })
            }
        }
    }
}

fn expand_self(ty: &Type, tyself: &Type) -> Type {
    match ty {
        Type::Arr(t1, t2) => Type::Arr(
            Box::new(dummy_spanned(expand_self(&t1.v, tyself))),
            Box::new(dummy_spanned(expand_self(&t2.v, tyself))),
        ),
        Type::TyRecord(fields) => Type::TyRecord(
            fields
                .iter()
                .map(|f| TyField {
                    label: f.label.clone(),
                    ty: dummy_spanned(expand_self(&f.ty.v, tyself)),
                })
                .collect(),
        ),
        Type::TySelf => tyself.clone(),
        _ => ty.clone(),
    }
}

fn pat_type_of(ctx: &Context, p: &Pattern) -> Result<PatType, ErrorWithPos> {
    match p {
        Pattern::Var(_x, ty) => {
            let ctx_ = ctx.clone().shift_and_push0(ty.clone());
            Ok(PatType {
                ty: ty.clone(),
                add: 1,
                context: ctx_,
            })
        }
        Pattern::Record(pfs) => {
            // let mut tyfields: Vec<TyField> = vec![];
            // let mut add = 0;
            // let mut ctx_ = ctx.clone();
            // let mut ctx_inner = Context::default();
            // for pf in pfs {
            //     let pty = pat_type_of(&ctx_inner, &pf.pat)?;
            //     ctx_inner = pty.context;
            //     add += pty.add;

            //     tyfields.push(TyField {
            //         label: pf.label.clone(),
            //         ty: pty.ty.clone(),
            //     });
            //     ctx_ = ctx_.shift_and_push0(pty.ty);
            //     add += 1;
            // }
            // ctx_ = ctx_.concat(ctx_inner);

            // Ok(PatType {
            //     ty: Type::TyRecord(tyfields),
            //     add,
            //     context: ctx_,
            // })

            let (tyf_r, add, ctx_, ctx_inner) = pfs.iter().try_rfold(
                (vec![], 0, ctx.clone(), Context::default()),
                |(mut acc_tyfs, acc_add, acc_ctx, acc_ctx_inner), pf| {
                    let pty = pat_type_of(&acc_ctx_inner, &pf.pat)?;
                    acc_tyfs.push(TyField {
                        label: pf.label.clone(),
                        ty: dummy_spanned(pty.ty.clone()),
                    });
                    Ok::<_, ErrorWithPos>((
                        acc_tyfs,
                        acc_add + 1 + pty.add,
                        acc_ctx.shift_and_push0(pty.ty),
                        if matches!(pf.pat, Pattern::Var(_, _)) {
                            acc_ctx_inner
                        } else {
                            pty.context
                        },
                    ))
                },
            )?;
            let tyf = tyf_r.iter().rev().cloned().collect::<Vec<_>>();

            // println!(
            //     "------\nctx:\n{},\nctx_inner:\n{},\nctx_:\n{}",
            //     ctx, ctx_inner, ctx_
            // );

            Ok(PatType {
                ty: Type::TyRecord(tyf),
                add,
                context: ctx.clone().concat(ctx_inner).concat(ctx_.clone()),
            })
        }
        Pattern::TmpTagging(PTmpTag { ty, label, nargs }) => {
            if let Type::TyTagging(tyfields) = ty {
                let ty0 = tyfields
                    .iter()
                    .find(|field| field.label == *label)
                    .map(|field| field.ty.v.clone())
                    .ok_or_else(|| ErrorWithPos {
                        message: format!(
                            "type check failed: {}\n  tag {} not found in tagging type {}",
                            p, label, ty
                        ),
                        level: 90,
                        kind: None,
                        line: 1,
                        column: 1,
                    })?;

                let mut pty = PatType {
                    ty: ty0.clone(),
                    add: 0,
                    context: ctx.clone(),
                };

                fn tyarr_to_vec(ty: Type) -> Vec<Type> {
                    match ty {
                        Type::Arr(l, r) => once(l.v).chain(tyarr_to_vec(r.v)).collect(),
                        ty => vec![ty],
                    }
                }
                let n = nargs.len();
                let tyargs = tyarr_to_vec(ty0.clone());
                let mut ctx_ = ctx.clone();
                for (i, _tya) in (0..n).rev().zip(tyargs) {
                    if let Type::Arr(ty1, ty2) = pty.ty {
                        let ty1_ = expand_self(&ty1.v, ty);
                        let ptyarg = pat_type_of(&ctx_, &Pattern::Var(i.to_string(), ty1_))?;
                        ctx_ = ptyarg.context;
                        pty = PatType {
                            ty: ty2.v.clone(),
                            add: pty.add + ptyarg.add,
                            context: ctx_.clone(),
                        };
                    } else {
                        return Err(ErrorWithPos {
                            message: format!(
                                "type check failed: {}\n  expected arrow type, but found {}",
                                p, ty
                            ),
                            level: 90,
                            kind: None,
                            line: 1,
                            column: 1,
                        });
                    }
                }
                if types_equal_ignore_pos(&pty.ty, &Type::TySelf) {
                    pty.ty = ty.clone();
                } else {
                    return Err(ErrorWithPos {
                        message: format!(
                            "type check failed: {}\n  number of arguments did not match",
                            p,
                        ),
                        level: 90,
                        kind: None,
                        line: 1,
                        column: 1,
                    });
                }
                Ok(pty)
            } else {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected tagging type, but found {}",
                        p, ty
                    ),
                    level: 90,
                    kind: None,
                    line: 1,
                    column: 1,
                })
            }
        }
    }
}

mod tests {
    use rstest::rstest;

    #[allow(unused)]
    use crate::{
        span::dummy_spanned,
        syntax::{
            context::Context,
            r#type::{TyField, Type},
        },
        typing::type_of,
    };

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
                Box::new(dummy_spanned(extract_type_structure(&t1.v))),
                Box::new(dummy_spanned(extract_type_structure(&t2.v))),
            ),
            Type::TyRecord(fields) => Type::TyRecord(
                fields
                    .iter()
                    .map(|field| TyField {
                        label: field.label.clone(),
                        ty: dummy_spanned(extract_type_structure(&field.ty.v)),
                    })
                    .collect(),
            ),
            Type::TyTagging(fields) => Type::TyTagging(
                fields
                    .iter()
                    .map(|field| TyField {
                        label: field.label.clone(),
                        ty: dummy_spanned(extract_type_structure(&field.ty.v)),
                    })
                    .collect(),
            ),
        }
    }

    #[rstest]
    #[case(r"unit", Some(Type::Unit))]
    #[case(r"unit;0", None)]
    #[case(r"(\:Bool. ( unit ; 0 ) ) true", Some(Type::Bool))]
    #[case(r"(\:Bool.(unit;unit;0))true", Some(Type::Bool))]
    #[case(r"true", Some(Type::Bool))]
    #[case(r"false", Some(Type::Bool))]
    #[case(
    r"{b=(\x:Bool.x)false, if true then unit else unit}",
    Some(Type::TyRecord(vec![
        TyField {
            label: "b".to_string(),
            ty: dummy_spanned(Type::Bool),
        },
        TyField {
            label: "1".to_string(),
            ty: dummy_spanned(Type::Unit),
        },
    ]))
)]
    #[case(
        r"\:Bool.0",
        Some(Type::Arr(
            Box::new(dummy_spanned(Type::Bool)),
            Box::new(dummy_spanned(Type::Bool))
        ))
    )]
    #[case(
        r"\:Bool.\:Bool.0",
        Some(Type::Arr(
            Box::new(dummy_spanned(Type::Bool)),
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Bool))
            )))
        ))
    )]
    #[case(
        r"\:Bool->Bool.0",
        Some(Type::Arr(
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Bool))
            ))),
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Bool))
            ))),
        ))
    )]
    #[case(r"if true then false else true", Some(Type::Bool))]
    #[case(r"if true then false else 1", None)]
    #[case(r"if 1 then false else true", None)]
    #[case(
        r"(\:Bool.if 0 then (\:Bool.\:Bool.1) else (\:Bool.\:Bool.0)) true",
        Some(Type::Arr(
            Box::new(dummy_spanned(Type::Bool)),
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Bool))
            ))),
        ))
    )]
    #[case(r"(\:(Bool->Bool)->Bool.0) \:Bool.0", None)]
    #[case(
        r"(\:(Bool->Bool)->Bool.0) \:Bool->Bool.0 true",
        Some(Type::Arr(
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Bool))
            ))),
            Box::new(dummy_spanned(Type::Bool))
        ))
    )]
    #[case(
        r"
(\:Bool->Bool. \:Bool->Bool.
    if 0 true then
        if 1 true then false else true
    else
        if 1 true then true else false
)
(\:Bool.0)
\:Bool.if 0 then false else true",
        Some(Type::Bool)
    )]
    #[case(
        r"
(\:Bool->Bool. \:Bool->Bool.
    if 0 true then
        if 1 true then false else true
    else
        if 1 true then true else false
)
(\:Bool->Bool.0)
\:Bool.if 0 then false else true",
        None
    )]
    #[case(
        r"
(\:Bool->Bool. \:Bool->Bool.
    if 0 true then
        if 1 true then 0 else 1
    else
        if 1 true then 1 else 0
)
(\:Bool->Bool.0)
\:Bool.if 0 then false else true",
        None
    )]
    #[case(
        r"
(\:Bool->Bool. \:Bool->Bool.
    if 0 true then
        1
    else
        2
)
(\:Bool.0)
\:Bool.if 0 then false else true",
        None
    )]
    fn test_type_of(#[case] input: &str, #[case] expected: Option<Type>) {
        use crate::parser;

        let ctx = Context::default();
        let t = parser::parse_spanned(input).unwrap();
        let ty = type_of(&ctx, &t);
        match expected {
            Some(ty2) => {
                let result_ty = ty.unwrap();
                let extracted_result = extract_type_structure(&result_ty);
                assert_eq!(extracted_result, ty2);
            }
            None => assert!(ty.is_err()),
        }
    }
}
