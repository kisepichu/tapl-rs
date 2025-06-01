use rstest::rstest;

use crate::{
    span::{ErrorWithPos, Spanned},
    syntax::{context::Context, term::Term, r#type::Type},
};

#[allow(dead_code)]
pub fn type_of(ctx: &Context, t: &Term) -> Result<Type, String> {
    match t {
        Term::Var(x) => match ctx.get(*x) {
            Some(ty) => Ok(ty.clone()),
            None => Err(format!(
                "type check failed: {}\n: unbound variable {}",
                t, x
            )),
        },
        Term::Abs(ty, t2) => {
            let ctx = ctx.clone().push(ty.clone());
            let ty2 = type_of(&ctx, &t2.v)?;
            Ok(Type::Arr(Box::new(ty.clone()), Box::new(ty2.clone())))
        }
        Term::App(t1, t2) => {
            let ty1 = type_of(ctx, &t1.v)?;
            let ty2 = type_of(ctx, &t2.v)?;
            match ty1 {
                Type::Arr(ty11, ty12) => {
                    if *ty11 == ty2 {
                        Ok(*ty12.clone())
                    } else {
                        let t1_str = t1.v.to_string();
                        let t1_str = if t1_str.len() <= 20 {
                            t1_str
                        } else {
                            t1_str[..20].to_string() + "..."
                        };
                        Err(format!(
                            "type check failed: {}\ntype of argument to the term {} is incorrect:\n  expected: {}, found: {}: {}",
                            t, t1_str, ty11, t2.v, ty2
                        ))
                    }
                }
                _ => Err(format!(
                    "type check failed: {}\n  expected arrow type, but found {}: {}",
                    t, t1.v, ty1
                )),
            }
        }
        Term::True => Ok(Type::Bool),
        Term::False => Ok(Type::Bool),
        Term::If(t1, t2, t3) => {
            let ty1 = type_of(ctx, &t1.v)?;
            let ty2 = type_of(ctx, &t2.v)?;
            let ty3 = type_of(ctx, &t3.v)?;
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
                    t, t1.v, ty1
                ))
            }
        }
    }
}

pub fn type_of_spanned(ctx: &Context, t: &Spanned<Term>) -> Result<Type, ErrorWithPos> {
    match &t.v {
        Term::Var(x) => match ctx.get(*x) {
            Some(ty) => Ok(ty.clone()),
            None => Err(ErrorWithPos {
                message: format!("type check failed: {}\n: unbound variable {}", t.v, x),
                level: 100,
                kind: None,
                line: t.line,
                column: t.column,
            }),
        },
        Term::Abs(ty, t2) => {
            let ctx = ctx.clone().push(ty.clone());
            let ty2 = type_of_spanned(&ctx, t2)?;
            Ok(Type::Arr(Box::new(ty.clone()), Box::new(ty2.clone())))
        }
        Term::App(t1, t2) => {
            let ty1 = type_of_spanned(ctx, t1)?;
            let ty2 = type_of_spanned(ctx, t2)?;
            match ty1 {
                Type::Arr(ty11, ty12) => {
                    if *ty11 == ty2 {
                        Ok(*ty12.clone())
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
                                t.v, t1_str, ty11, t2.v, ty2
                            ),
                            level: 100,
                            kind: None,
                            line: t2.line,
                            column: t2.column,
                        })
                    }
                }
                _ => Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected arrow type, but found {}: {}",
                        t.v, t1.v, ty1
                    ),
                    level: 100,
                    kind: None,
                    line: t1.line,
                    column: t1.column,
                }),
            }
        }
        Term::True => Ok(Type::Bool),
        Term::False => Ok(Type::Bool),
        Term::If(t1, t2, t3) => {
            let ty1 = type_of_spanned(ctx, t1)?;
            let ty2 = type_of_spanned(ctx, t2)?;
            let ty3 = type_of_spanned(ctx, t3)?;
            if ty1 == Type::Bool && ty2 == ty3 {
                Ok(ty2)
            } else if ty2 != ty3 {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  arms of conditional have different types:\n  {}, {}",
                        t.v, ty2, ty3
                    ),
                    level: 100,
                    kind: None,
                    line: t.line,
                    column: t.column,
                })
            } else {
                Err(ErrorWithPos {
                    message: format!(
                        "type check failed: {}\n  expected boolean type, but found {}: {}",
                        t.v, t1.v, ty1
                    ),
                    level: 100,
                    kind: None,
                    line: t1.line,
                    column: t1.column,
                })
            }
        }
    }
}

#[rstest]
#[case(r"true", Some(Type::Bool))]
#[case(r"false", Some(Type::Bool))]
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
