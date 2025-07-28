use num::FromPrimitive;

use crate::{
    span::Spanned,
    syntax::{
        context::Context,
        term::{Info, Term},
    },
};

pub mod strategy;
pub use strategy::Strategy;

fn term_shift(t: &Term, d: isize) -> Result<Term, String> {
    fn walk(t: &Term, d: isize, c: usize) -> Result<Term, String> {
        match t {
            Term::Var(x, info) => {
                if *x >= c {
                    let s: usize = usize::from_isize(*x as isize + d).ok_or("minus after shift")?;
                    Ok(Term::Var(s, info.clone()))
                } else {
                    Ok(Term::Var(*x, info.clone()))
                }
            }
            Term::TmpVar(_) => Ok(t.clone()),
            Term::Abs(ty, t1, info) => Ok(Term::Abs(
                ty.clone(),
                Box::new(Spanned {
                    v: walk(&t1.v, d, c + 1)?,
                    start: t1.start,
                    line: t1.line,
                    column: t1.column,
                }),
                info.clone(),
            )),
            Term::App(t1, t2) => Ok(Term::App(
                Box::new(Spanned {
                    v: walk(&t1.v, d, c)?,
                    start: t1.start,
                    line: t1.line,
                    column: t1.column,
                }),
                Box::new(Spanned {
                    v: walk(&t2.v, d, c)?,
                    start: t2.start,
                    line: t2.line,
                    column: t2.column,
                }),
            )),
        }
    }
    walk(t, d, 0)
}

fn term_subst(j: isize, s: &Term, t: &Term) -> Result<Term, String> {
    fn walk(j: isize, s: &Term, c: isize, t: &Term) -> Result<Term, String> {
        match t {
            Term::Var(k, info) => {
                if Some(*k) == (j + c).try_into().ok() {
                    term_shift(s, c)
                } else {
                    Ok(Term::Var(*k, info.clone()))
                }
            }
            Term::TmpVar(_) => Ok(t.clone()),
            Term::Abs(ty, t1, info) => Ok(Term::Abs(
                ty.clone(),
                Box::new(Spanned {
                    v: walk(j, s, c + 1, &t1.v)?,
                    start: t1.start,
                    line: t1.line,
                    column: t1.column,
                }),
                info.clone(),
            )),
            Term::App(t1, t2) => Ok(Term::App(
                Box::new(Spanned {
                    v: walk(j, s, c, &t1.v)?,
                    start: t1.start,
                    line: t1.line,
                    column: t1.column,
                }),
                Box::new(Spanned {
                    v: walk(j, s, c, &t2.v)?,
                    start: t2.start,
                    line: t2.line,
                    column: t2.column,
                }),
            )),
        }
    }
    walk(j, s, 0, t)
}

fn term_subst_top(s: &Term, t: &Term) -> Result<Term, String> {
    term_shift(&term_subst(0, &term_shift(s, 1)?, t)?, -1)
}

fn zero_in_fv(t: &Term) -> bool {
    fn walk(t: &Term, c: usize) -> bool {
        match t {
            Term::Var(x, _) => *x == c,
            Term::TmpVar(_) => false,
            Term::Abs(_, t1, _) => walk(&t1.v, c + 1),
            Term::App(t1, t2) => walk(&t1.v, c) || walk(&t2.v, c),
        }
    }
    walk(t, 0)
}

fn resolve_conflicts(ctx: &Context, t: &Spanned<Term>) -> Result<Spanned<Term>, String> {
    fn walk(
        ctx: &Context,
        t: &Spanned<Term>,
        used_names: &mut std::collections::HashSet<String>,
    ) -> Result<Spanned<Term>, String> {
        match &t.v {
            Term::Var(x, _info) => {
                if let Some(ctx_info) = ctx.get_info(*x) {
                    Ok(Spanned {
                        v: Term::Var(*x, ctx_info.clone()),
                        start: t.start,
                        line: t.line,
                        column: t.column,
                    })
                } else {
                    Ok(t.clone())
                }
            }
            Term::TmpVar(_) => Ok(t.clone()),
            Term::Abs(ty, t1, info) => {
                let mut new_name = info.name.clone();
                while used_names.contains(&new_name) {
                    new_name.push('\'');
                }
                used_names.insert(new_name.clone());

                let new_info = Info {
                    name: new_name,
                    assumption_num: info.assumption_num,
                };

                let mut new_ctx = ctx.clone();
                new_ctx = new_ctx.push(ty.clone(), new_info.clone());

                let new_t1 = walk(&new_ctx, t1, used_names)?;

                Ok(Spanned {
                    v: Term::Abs(ty.clone(), Box::new(new_t1), new_info),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                })
            }
            Term::App(t1, t2) => {
                let new_t1 = walk(ctx, t1, used_names)?;
                let new_t2 = walk(ctx, t2, used_names)?;

                Ok(Spanned {
                    v: Term::App(Box::new(new_t1), Box::new(new_t2)),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                })
            }
        }
    }

    let mut used_names = std::collections::HashSet::new();
    walk(ctx, t, &mut used_names)
}

fn eval1(t: &Term, ctx: &Context, strategy: &Strategy) -> Result<Term, String> {
    match t {
        Term::App(t1, t2) => Ok(match (&t1.v, &t2.v) {
            // E-BETA
            (Term::Abs(_ty, t12, _), t2_) if t2_.isval() || strategy.beta_cbn => {
                // println!("E-BETA: {}", t);
                resolve_conflicts(
                    ctx,
                    &Spanned {
                        v: term_subst_top(t2_, &t12.v)?,
                        start: t2.start,
                        line: t2.line,
                        column: t2.column,
                    },
                )?
                .v
            }
            // E-APP2
            (v1, _) if v1.isval() => {
                // println!("E-APP2: {}", t);
                Term::App(
                    t1.clone(),
                    Box::new(Spanned {
                        v: eval1(&t2.v, ctx, strategy)?,
                        start: t2.start,
                        line: t2.line,
                        column: t2.column,
                    }),
                )
            }
            // E-APP1
            _ => {
                // println!("E-APP1: {}", t);
                Term::App(
                    Box::new(Spanned {
                        v: eval1(&t1.v, ctx, strategy)?,
                        start: t1.start,
                        line: t1.line,
                        column: t1.column,
                    }),
                    t2.clone(),
                )
            }
        }),
        Term::Abs(ty, t2, info) => match &t2.v {
            // E-ETA
            Term::App(t21, tvar0)
                if strategy.use_eta
                    && !zero_in_fv(&t21.v)
                    && matches!(tvar0.v, Term::Var(0, _)) =>
            {
                // println!("E-ETA: {}", t);
                Ok(term_shift(&t21.v, -1)?)
            }
            // (E-LAM)
            _ if strategy.use_lam => {
                // println!("E-LAM: {}", t);
                let mut new_ctx = ctx.clone();
                new_ctx = new_ctx.push(ty.clone(), info.clone());
                Ok(Term::Abs(
                    ty.clone(),
                    Box::new(Spanned {
                        v: eval1(&t2.v, &new_ctx, strategy)?,
                        start: t2.start,
                        line: t2.line,
                        column: t2.column,
                    }),
                    info.clone(),
                ))
            }
            _ => Err("eval1: no rule applies".to_string()),
        },
        _ => Err("eval1: no rule applies".to_string()),
    }
}

pub fn eval(t: &Term, strategy: &Strategy) -> Result<Term, String> {
    let mut t = t.clone();
    loop {
        t = match eval1(&t, &Context::default(), strategy) {
            Ok(t1) => t1,
            Err(e) => {
                if e == "eval1: no rule applies" {
                    break;
                } else {
                    return Err(e);
                }
            }
        }
    }
    Ok(t)
}

#[cfg(test)]
mod tests {
    use crate::syntax::term::Info;
    use crate::syntax::r#type::Type;

    use super::*;

    // Helper function to create Spanned<Term> for testing
    fn spanned(term: Term) -> Spanned<Term> {
        Spanned {
            v: term,
            start: 0,
            line: 1,
            column: 1,
        }
    }

    // Helper function to compare terms ignoring Info content
    fn terms_equal_ignore_info(t1: &Term, t2: &Term) -> bool {
        match (t1, t2) {
            (Term::Var(x1, _), Term::Var(x2, _)) => x1 == x2,
            (Term::TmpVar(s1), Term::TmpVar(s2)) => s1 == s2,
            (Term::Abs(ty1, t1, _), Term::Abs(ty2, t2, _)) => {
                ty1 == ty2 && terms_equal_ignore_info(&t1.v, &t2.v)
            }
            (Term::App(t11, t12), Term::App(t21, t22)) => {
                terms_equal_ignore_info(&t11.v, &t21.v) && terms_equal_ignore_info(&t12.v, &t22.v)
            }
            _ => false,
        }
    }

    #[test]
    fn test_eval1() {
        let st = Strategy::from("cbv").unwrap();

        // id = \x:Bool.x
        let id = Term::Abs(
            Type::TyVar("Bool".to_string()),
            Box::new(spanned(Term::Var(
                0,
                Info {
                    name: "x".to_string(),
                    assumption_num: 0,
                },
            ))),
            Info {
                name: "x".to_string(),
                assumption_num: 0,
            },
        );
        {
            // (\x.x) (\x.x) == \x.x
            let t = Term::App(Box::new(spanned(id.clone())), Box::new(spanned(id.clone())));
            let result = eval(&t, &st).unwrap();
            // Infoの内容は無視して、構造のみを比較
            assert!(terms_equal_ignore_info(&result, &id));
        }

        // tru = \t:Bool.\f:Bool.t
        let tru = Term::Abs(
            Type::TyVar("Bool".to_string()),
            Box::new(spanned(Term::Abs(
                Type::TyVar("Bool".to_string()),
                Box::new(spanned(Term::Var(
                    1,
                    Info {
                        name: "t".to_string(),
                        assumption_num: 1,
                    },
                ))),
                Info {
                    name: "f".to_string(),
                    assumption_num: 0,
                },
            ))),
            Info {
                name: "t".to_string(),
                assumption_num: 0,
            },
        );
        // fls = \t:Bool.\f:Bool.f
        let fls = Term::Abs(
            Type::TyVar("Bool".to_string()),
            Box::new(spanned(Term::Abs(
                Type::TyVar("Bool".to_string()),
                Box::new(spanned(Term::Var(
                    0,
                    Info {
                        name: "0".to_string(),
                        assumption_num: 0,
                    },
                ))),
                Info {
                    name: "x".to_string(),
                    assumption_num: 0,
                },
            ))),
            Info {
                name: "t".to_string(),
                assumption_num: 0,
            },
        );

        {
            // (\t:Bool.\f:Bool.f) id == id
            let t = Term::App(
                Box::new(spanned(fls.clone())),
                Box::new(spanned(id.clone())),
            );
            let result = eval(&t, &st).unwrap();
            assert!(terms_equal_ignore_info(&result, &id));
        }

        // and = \b: Bool.\c:Bool.b c fls
        let and = {
            Term::Abs(
                Type::TyVar("Bool".to_string()),
                Box::new(spanned(Term::Abs(
                    Type::TyVar("Bool".to_string()),
                    Box::new(spanned(Term::App(
                        Box::new(spanned(Term::App(
                            Box::new(spanned(Term::Var(
                                1,
                                Info {
                                    name: "1".to_string(),
                                    assumption_num: 1,
                                },
                            ))), // b
                            Box::new(spanned(Term::Var(
                                0,
                                Info {
                                    name: "0".to_string(),
                                    assumption_num: 0,
                                },
                            ))), // c
                        ))),
                        Box::new(spanned(fls.clone())),
                    ))),
                    Info {
                        name: "c".to_string(),
                        assumption_num: 0,
                    },
                ))),
                Info {
                    name: "b".to_string(),
                    assumption_num: 0,
                },
            )
        };

        {
            // and fls fls == fls
            let t = Term::App(
                Box::new(spanned(Term::App(
                    Box::new(spanned(and.clone())),
                    Box::new(spanned(fls.clone())),
                ))),
                Box::new(spanned(fls.clone())),
            );
            let result = eval(&t, &st).unwrap();
            assert!(terms_equal_ignore_info(&result, &fls));
        }

        {
            // and tru fls == fls
            let t = Term::App(
                Box::new(spanned(Term::App(
                    Box::new(spanned(and.clone())),
                    Box::new(spanned(tru.clone())),
                ))),
                Box::new(spanned(fls.clone())),
            );
            let result = eval(&t, &st).unwrap();
            assert!(terms_equal_ignore_info(&result, &fls));
        }

        {
            // and tru tru == tru
            let t = Term::App(
                Box::new(spanned(Term::App(
                    Box::new(spanned(and.clone())),
                    Box::new(spanned(tru.clone())),
                ))),
                Box::new(spanned(tru.clone())),
            );
            let result = eval(&t, &st).unwrap();
            assert!(terms_equal_ignore_info(&result, &tru));
        }
    }
}
