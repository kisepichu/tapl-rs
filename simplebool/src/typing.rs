use crate::syntax::{context::Context, term::Term, ty::Type};

#[allow(dead_code)]
pub fn type_of(ctx: &Context, t: &Term) -> Result<Type, String> {
    match t {
        Term::Var(x) => match ctx.get(*x) {
            Some(ty) => Ok(ty.clone()),
            None => Err(format!("type_of: unbound variable {}", x)),
        },
        Term::Abs(t2, ty) => {
            let ctx = ctx.clone().push(ty.clone());
            let ty2 = type_of(&ctx, t2)?;
            Ok(Type::Arr(Box::new(ty.clone()), Box::new(ty2.clone())))
        }
        Term::App(t1, t2) => {
            let ty1 = type_of(ctx, t1)?;
            let ty2 = type_of(ctx, t2)?;
            match ty1 {
                Type::Arr(ty11, ty12) => {
                    if *ty11 == ty2 {
                        Ok(*ty12.clone())
                    } else {
                        Err(format!(
                            "type check failed: {}\n  argument to the term {} is incorrect:\n  expected: {}, found: {}",
                            t, t1, ty11, ty2
                        ))
                    }
                }
                _ => Err(format!(
                    "type check failed: {}\n  expected arrow type, but found {}: {}",
                    t, t1, ty1
                )),
            }
        }
        Term::True => Ok(Type::Bool),
        Term::False => Ok(Type::Bool),
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
    }
}
