use rstest::rstest;

use crate::{
    span::{ErrorWithPos, Spanned},
    syntax::{context::Context, term::Term, r#type::Type},
};

// Helper function to compare types ignoring position information
fn types_equal_ignore_pos(ty1: &Type, ty2: &Type) -> bool {
    match (ty1, ty2) {
        (Type::Arr(t1a, t1b), Type::Arr(t2a, t2b)) => {
            types_equal_ignore_pos(&t1a.v, &t2a.v) && types_equal_ignore_pos(&t1b.v, &t2b.v)
        }
        (Type::TyVar(x1), Type::TyVar(x2)) => x1 == x2,
        _ => false,
    }
}

#[allow(dead_code)]
pub fn type_of(ctx: &Context, t: &Term) -> Result<Type, String> {
    match t {
        Term::Var(x, _info) => match ctx.get(*x) {
            Some(ty) => Ok(ty.clone()),
            None => Err(format!(
                "type check failed: {}\n: unbound variable {}",
                t, x
            )),
        },
        Term::TmpVar(s) => Err(format!(
            "type check failed: {}\n: unbound variable {}",
            t, s
        )),
        Term::Abs(ty, t2, info) => {
            let ctx = ctx.clone().push(ty.clone(), info.clone());
            let ty2 = type_of(&ctx, &t2.v)?;
            Ok(Type::Arr(
                Box::new(Spanned {
                    v: ty.clone(),
                    start: 0,
                    line: 1,
                    column: 1,
                }),
                Box::new(Spanned {
                    v: ty2.clone(),
                    start: 0,
                    line: 1,
                    column: 1,
                }),
            ))
        }
        Term::App(t1, t2) => {
            let ty1 = type_of(ctx, &t1.v)?;
            let ty2 = type_of(ctx, &t2.v)?;
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
                        Err(format!(
                            "type check failed: {}\ntype of argument to the term {} is incorrect:\n  expected: {}, found: {}: {}",
                            t, t1_str, ty11.v, t2.v, ty2
                        ))
                    }
                }
                _ => Err(format!(
                    "type check failed: {}\n  expected arrow type, but found {}: {}",
                    t, t1.v, ty1
                )),
            }
        }
    }
}

pub fn type_of_spanned(ctx: &Context, t: &Spanned<Term>) -> Result<Type, ErrorWithPos> {
    match &t.v {
        Term::Var(x, _info) => match ctx.get(*x) {
            Some(ty) => Ok(ty.clone()),
            None => Err(ErrorWithPos {
                message: format!("type check failed: {}\n: unbound variable {}", t.v, x),
                level: 100,
                kind: None,
                line: t.line,
                column: t.column,
            }),
        },
        Term::TmpVar(s) => Err(ErrorWithPos {
            message: format!("type check failed: {}\n: unbound variable {}", t.v, s),
            level: 100,
            kind: None,
            line: t.line,
            column: t.column,
        }),
        Term::Abs(ty, t2, info) => {
            let ctx = ctx.clone().push(ty.clone(), info.clone());
            let ty2 = type_of_spanned(&ctx, t2)?;
            Ok(Type::Arr(
                Box::new(Spanned {
                    v: ty.clone(),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                }),
                Box::new(Spanned {
                    v: ty2.clone(),
                    start: t2.start,
                    line: t2.line,
                    column: t2.column,
                }),
            ))
        }
        Term::App(t1, t2) => {
            let ty1 = type_of_spanned(ctx, t1)?;
            let ty2 = type_of_spanned(ctx, t2)?;
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
    }
}

// Helper function for creating spanned types in tests
#[cfg(test)]
fn spanned_type(ty: Type) -> Spanned<Type> {
    Spanned {
        v: ty,
        start: 0,
        line: 1,
        column: 1,
    }
}

#[allow(unused)]
// Helper function to extract just the type structure, ignoring position info
#[cfg(test)]
fn extract_type_structure(ty: &Type) -> Type {
    match ty {
        Type::Arr(t1, t2) => Type::Arr(
            Box::new(spanned_type(extract_type_structure(&t1.v))),
            Box::new(spanned_type(extract_type_structure(&t2.v))),
        ),
        Type::TyVar(x) => Type::TyVar(x.clone()),
    }
}

#[rstest]
#[case(
    r"\:Bool.0",
    Some(Type::Arr(Box::new(spanned_type(Type::TyVar("Bool".to_string()))), Box::new(spanned_type(Type::TyVar("Bool".to_string())))))
)]
#[case(
    r"\x:Bool.x",
    Some(Type::Arr(Box::new(spanned_type(Type::TyVar("Bool".to_string()))), Box::new(spanned_type(Type::TyVar("Bool".to_string())))))
)]
#[case(
    r"\:Bool.\:Bool.0",
    Some(Type::Arr(
        Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
            Box::new(spanned_type(Type::TyVar("Bool".to_string())))
        )))
    ))
)]
#[case(
    r"\x:Bool.\y:Bool.x",
    Some(Type::Arr(
        Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
            Box::new(spanned_type(Type::TyVar("Bool".to_string())))
        )))
    ))
)]
#[case(
    r"\x:Bool.\y:Bool.y",
    Some(Type::Arr(
        Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
            Box::new(spanned_type(Type::TyVar("Bool".to_string())))
        )))
    ))
)]
#[case(
    r"\:Bool->Bool.0",
    Some(Type::Arr(
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
            Box::new(spanned_type(Type::TyVar("Bool".to_string())))
        ))),
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
            Box::new(spanned_type(Type::TyVar("Bool".to_string())))
        ))),
    ))
)]
#[case(
    r"\f:Bool->Bool.f",
    Some(Type::Arr(
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
            Box::new(spanned_type(Type::TyVar("Bool".to_string())))
        ))),
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
            Box::new(spanned_type(Type::TyVar("Bool".to_string())))
        ))),
    ))
)]
#[case(r"(\:Bool->Bool.0) \:Bool.0", 
    Some(Type::Arr(
        Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
        Box::new(spanned_type(Type::TyVar("Bool".to_string())))
    ))
)]
#[case(r"(\f:Bool->Bool.f) \x:Bool.x", 
    Some(Type::Arr(
        Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
        Box::new(spanned_type(Type::TyVar("Bool".to_string())))
    ))
)]
#[case(r"(\:(Bool->Bool)->Bool.0) \:Bool.0", None)]
#[case(r"(\f:(Bool->Bool)->Bool.0) \x:Bool.x", None)]
fn test_type_of(#[case] input: &str, #[case] expected: Option<Type>) {
    use crate::parser;
    println!("input: {}", input);

    let ctx = Context::default();
    let t = parser::parse(input).unwrap();
    let ty = type_of(&ctx, &t);
    match expected {
        Some(ty2) => {
            if ty.is_err() {
                println!("expected: Some({:?})", ty2);
                println!("actual: {:?}", ty);
                panic!();
            }
            let result_ty = ty.unwrap();
            let extracted_result = extract_type_structure(&result_ty);
            println!("expected: {}", ty2);
            println!("actual: {}", extracted_result);
            assert_eq!(extracted_result, ty2);
        }
        None => {
            println!("expected: None");
            println!("actual: {:?}", ty);
            assert!(ty.is_err())
        }
    }
}
