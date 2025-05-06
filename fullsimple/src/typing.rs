use rstest::rstest;

use crate::syntax::{
    context::Context,
    pattern::Pattern,
    pattype::PatType,
    term::Term,
    r#type::{TyField, Type},
};

#[allow(dead_code)]
pub fn type_of(ctx: &Context, t: &Term) -> Result<Type, String> {
    match t {
        Term::Var(xn) => match ctx.get(*xn) {
            Some(ty) => Ok(ty.clone()),
            None => Err(format!(
                "type check failed: {}\n: unbound variable {}",
                t, xn
            )),
        },
        Term::TmpVar(s) => Err(format!(
            "type check failed: {}\n: undefined variable: {}",
            t, s
        )),
        Term::Abs(ty1, t2) => {
            let ctx_ = ctx.clone().shift_and_push0(ty1.clone());
            let ty2 = type_of(&ctx_, t2)?;
            Ok(Type::Arr(Box::new(ty1.clone()), Box::new(ty2.clone())))
        }
        Term::App(t1, t2) => {
            let ty1 = type_of(ctx, t1)?;
            let ty2 = type_of(ctx, t2)?;
            match ty1 {
                Type::Arr(ty11, ty12) => {
                    if *ty11 == ty2 {
                        Ok(*ty12.clone())
                    } else {
                        let t1 = t1.to_string();
                        let t1 = if t1.len() <= 20 {
                            t1
                        } else {
                            t1[..20].to_string() + "..."
                        };
                        Err(format!(
                            "type check failed: {}\ntype of argument to the term {} is incorrect:\n  expected: {}, found: {}: {}",
                            t, t1, ty11, t2, ty2
                        ))
                    }
                }
                _ => Err(format!(
                    "type check failed: {}\n  expected arrow type, but found {}: {}",
                    t, t1, ty1
                )),
            }
        }
        Term::Unit => Ok(Type::Unit),
        Term::True => Ok(Type::Bool),
        Term::False => Ok(Type::Bool),
        Term::Record(fields) => {
            let tyfields = fields
                .iter()
                .map(|field| {
                    let ty = type_of(ctx, &field.term)?;
                    Ok(TyField {
                        label: field.label.clone(),
                        ty,
                    })
                })
                .collect::<Result<Vec<_>, String>>()?;
            Ok(Type::TyRecord(tyfields))
        }
        Term::Projection(t1, label) => {
            let ty1 = type_of(ctx, t1)?;
            if let Type::TyRecord(fields) = ty1.clone() {
                fields
                    .iter()
                    .find(|field| field.label == *label)
                    .map(|field| field.ty.clone())
                    .ok_or_else(|| {
                        format!(
                            "type check failed: {}\n  field {} not found in record type {}",
                            t, label, ty1
                        )
                    })
            } else {
                Err(format!(
                    "type check failed: {}\n  expected record type, but found {}: {}",
                    t, t1, ty1
                ))
            }
        }
        Term::If(t1, t2, t3) => {
            let ty1 = type_of(ctx, t1)?;
            let ty2 = type_of(ctx, t2)?;
            let ty3 = type_of(ctx, t3)?;
            if ty1 == Type::Bool && ty2 == ty3 {
                Ok(ty2)
            } else if ty2 != ty3 {
                Err(format!(
                    "type check failed: {}\n  arms of conditional have different types:\n  {}, {}",
                    t, ty2, ty3
                ))
            } else {
                Err(format!(
                    "type check failed: {}\n  expected boolean type, but found {}: {}",
                    t, t1, ty1
                ))
            }
        }
        Term::Let(t1, t2) => {
            let ty1 = type_of(ctx, t1)?;
            let ctx_ = ctx.clone().shift_and_push0(ty1);
            type_of(&ctx_, t2)
        }
        Term::Plet(_p, _t1, _t2) => {
            todo!()
        }
        Term::Tagging(_ty, _label) => {
            todo!()
        }
        Term::Case(_t1, _bs) => {
            todo!()
        }
    }
}

fn pat_type_of(ctx: &Context, p: &Pattern) -> Result<PatType, String> {
    match p {
        Pattern::Var(xs, ty) => {
            let ctx_ = ctx.clone().shift_and_push0(ty.clone());
            Ok(PatType {
                ty: ty.clone(),
                add: 1,
                context: ctx_,
            })
        }
        Pattern::Record(patfields) => {
            let mut tyfields: Vec<TyField> = vec![];
            let mut add = 0;
            let mut ctx_ = ctx.clone();
            for pf in patfields {
                let ty = pat_type_of(&ctx_, &pf.pat)?;
                tyfields.push(TyField {
                    label: pf.label.clone(),
                    ty: ty.ty.clone(),
                });
                ctx_ = ctx_.shift_and_push0(ty.ty);
                add += ty.add;
            }
            Ok(PatType {
                ty: Type::TyRecord(tyfields),
                add,
                context: ctx_,
            })
        }
        Pattern::Tagging(_ty, _label, _ps) => {
            todo!()
        }
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
            ty: Type::Bool,
        },
        TyField {
            label: "1".to_string(),
            ty: Type::Unit,
        },
    ]))
)]
#[case(
    r"\:Bool.0",
    Some(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
)]
#[case(
    r"\:Bool.\:Bool.0",
    Some(Type::Arr(
        Box::new(Type::Bool),
        Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
    ))
)]
#[case(
    r"\:Bool->Bool.0",
    Some(Type::Arr(
        Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
        Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
    ))
)]
#[case(r"if true then false else true", Some(Type::Bool))]
#[case(r"if true then false else 1", None)]
#[case(r"if 1 then false else true", None)]
#[case(
    r"(\:Bool.if 0 then (\:Bool.\:Bool.1) else (\:Bool.\:Bool.0)) true",
    Some(Type::Arr(
        Box::new(Type::Bool),
        Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
    ))
)]
#[case(r"(\:(Bool->Bool)->Bool.0) \:Bool.0", None)]
#[case(
    r"(\:(Bool->Bool)->Bool.0) \:Bool->Bool.0 true",
    Some(Type::Arr(
        Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
        Box::new(Type::Bool)
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
    let t = parser::parse(input).unwrap();
    let ty = type_of(&ctx, &t);
    match expected {
        Some(ty2) => assert_eq!(ty.unwrap(), ty2),
        None => assert!(ty.is_err()),
    }
}
