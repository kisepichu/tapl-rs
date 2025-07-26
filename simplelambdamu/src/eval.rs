use num::FromPrimitive;

use crate::{
    span::Spanned,
    syntax::{context::Context, term::Term, r#type::Type},
    typing::type_of_spanned,
};

fn term_shift(t: &Term, d: isize) -> Result<Term, String> {
    fn walk(t: &Term, d: isize, c: usize) -> Result<Term, String> {
        match t {
            Term::Var(x) => {
                if *x >= c {
                    let s: usize = usize::from_isize(*x as isize + d).ok_or("minus after shift")?;
                    Ok(Term::Var(s))
                } else {
                    Ok(Term::Var(*x))
                }
            }
            Term::TmpVar(_) => Ok(t.clone()),
            Term::Abs(ty, t1) => Ok(Term::Abs(
                ty.clone(),
                Box::new(Spanned {
                    v: walk(&t1.v, d, c + 1)?,
                    start: t1.start,
                    line: t1.line,
                    column: t1.column,
                }),
            )),
            Term::MAbs(ty, t1) => Ok(Term::MAbs(
                ty.clone(),
                Box::new(Spanned {
                    v: walk(&t1.v, d, c + 1)?,
                    start: t1.start,
                    line: t1.line,
                    column: t1.column,
                }),
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
            Term::Var(k) => {
                if Some(*k) == (j + c).try_into().ok() {
                    term_shift(s, c)
                } else {
                    Ok(Term::Var(*k))
                }
            }
            Term::TmpVar(_) => Ok(t.clone()),
            Term::Abs(ty, t1) => Ok(Term::Abs(
                ty.clone(),
                Box::new(Spanned {
                    v: walk(j, s, c + 1, &t1.v)?,
                    start: t1.start,
                    line: t1.line,
                    column: t1.column,
                }),
            )),
            Term::MAbs(ty, t1) => Ok(Term::MAbs(
                ty.clone(),
                Box::new(Spanned {
                    v: walk(j, s, c + 1, &t1.v)?,
                    start: t1.start,
                    line: t1.line,
                    column: t1.column,
                }),
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
            Term::Var(k) => *k == c,
            Term::TmpVar(_) => false,
            Term::Abs(_, t1) => walk(&t1.v, c + 1),
            Term::MAbs(_, t1) => walk(&t1.v, c + 1),
            Term::App(t1, t2) => walk(&t1.v, c) || walk(&t2.v, c),
        }
    }
    walk(t, 0)
}

fn is_neg_type(t: &Term) -> bool {
    let ty = type_of_spanned(
        &Context::default(),
        &Spanned {
            v: t.clone(),
            start: 1,
            line: 1,
            column: 1,
        },
    );
    if let Ok(Type::Arr(_, b)) = ty {
        b.v == Type::Bot
    } else {
        false
    }
}

fn eval1(t: &Term) -> Result<Term, String> {
    match t {
        Term::App(t1, t2) => Ok(match (&t1.v, &t2.v) {
            // E-BETA
            (Term::Abs(_ty, t12), v2) if v2.isval() => term_subst_top(v2, &t12.v)?,
            // E-MUBETA
            (n1, Term::MAbs(_ty, t22)) if is_neg_type(n1) => term_subst_top(n1, &t22.v)?,
            // E-STR
            (Term::MAbs(Type::Arr(tyab, tybot), t12), _) if tybot.v == Type::Bot => {
                if let Type::Arr(_tya, tyb) = &tyab.v {
                    fn asterisk(t: &Spanned<Term>, t2: &Spanned<Term>) -> Spanned<Term> {
                        fn walk(t: &Spanned<Term>, t2: &Spanned<Term>, c: usize) -> Spanned<Term> {
                            match &t.v {
                                Term::App(alpha, u) => match alpha.v {
                                    Term::Var(x) if x == c => Spanned {
                                        v: Term::App(
                                            Box::new(walk(alpha, t2, c)),
                                            Box::new(Spanned {
                                                v: Term::App(
                                                    Box::new(walk(u, t2, c)),
                                                    Box::new(t2.clone()),
                                                ),
                                                start: u.start,
                                                line: u.line,
                                                column: u.column,
                                            }),
                                        ),
                                        start: t.start,
                                        line: t.line,
                                        column: t.column,
                                    },
                                    _ => Spanned {
                                        v: Term::App(
                                            Box::new(walk(alpha, t2, c)),
                                            Box::new(walk(u, t2, c)),
                                        ),
                                        start: t.start,
                                        line: t.line,
                                        column: t.column,
                                    },
                                },

                                Term::Var(_) => t.clone(),
                                Term::TmpVar(_) => t.clone(),
                                Term::Abs(ty, t1) => Spanned {
                                    v: Term::Abs(ty.clone(), Box::new(walk(t1, t2, c + 1))),
                                    start: t.start,
                                    line: t.line,
                                    column: t.column,
                                },
                                Term::MAbs(ty, t1) => Spanned {
                                    v: Term::MAbs(ty.clone(), Box::new(walk(t1, t2, c + 1))),
                                    start: t.start,
                                    line: t.line,
                                    column: t.column,
                                },
                            }
                        }
                        walk(t, t2, 0)
                    }

                    let t12_aster = asterisk(t12, t2);
                    Ok(Term::MAbs(
                        Type::Arr(
                            tyb.clone(),
                            Box::new(Spanned {
                                v: Type::Bot,
                                start: t12.start,
                                line: t12.line,
                                column: t12.column,
                            }),
                        ),
                        Box::new(t12_aster),
                    ))
                } else {
                    Err("eval1: no rule applies".to_string())
                }
            }?,
            // E-STRV
            (v1, Term::MAbs(Type::Arr(_tya, tybot), t22)) if v1.isval() && tybot.v == Type::Bot => {
                {
                    fn star(t: &Spanned<Term>, v1: &Spanned<Term>) -> Spanned<Term> {
                        fn walk(t: &Spanned<Term>, v1: &Spanned<Term>, c: usize) -> Spanned<Term> {
                            match &t.v {
                                Term::App(alpha, u) => match alpha.v {
                                    Term::Var(x) if x == c => Spanned {
                                        v: Term::App(
                                            Box::new(walk(alpha, v1, c)),
                                            Box::new(Spanned {
                                                v: Term::App(
                                                    Box::new(v1.clone()),
                                                    Box::new(walk(u, v1, c)),
                                                ),
                                                start: u.start,
                                                line: u.line,
                                                column: u.column,
                                            }),
                                        ),
                                        start: t.start,
                                        line: t.line,
                                        column: t.column,
                                    },
                                    _ => Spanned {
                                        v: Term::App(
                                            Box::new(walk(alpha, v1, c)),
                                            Box::new(walk(u, v1, c)),
                                        ),
                                        start: t.start,
                                        line: t.line,
                                        column: t.column,
                                    },
                                },

                                Term::Var(_) => t.clone(),
                                Term::TmpVar(_) => t.clone(),
                                Term::Abs(ty, t1) => Spanned {
                                    v: Term::Abs(ty.clone(), Box::new(walk(t1, v1, c + 1))),
                                    start: t.start,
                                    line: t.line,
                                    column: t.column,
                                },
                                Term::MAbs(ty, t1) => Spanned {
                                    v: Term::MAbs(ty.clone(), Box::new(walk(t1, v1, c + 1))),
                                    start: t.start,
                                    line: t.line,
                                    column: t.column,
                                },
                            }
                        }
                        walk(t, v1, 0)
                    }

                    let t22_star = star(t22, t1);
                    let tyv1 = type_of_spanned(&Context::default(), t1);
                    if let Ok(Type::Arr(_tya, tyb)) = tyv1 {
                        Ok(Term::MAbs(
                            Type::Arr(
                                tyb.clone(),
                                Box::new(Spanned {
                                    v: Type::Bot,
                                    start: t22.start,
                                    line: t22.line,
                                    column: t22.column,
                                }),
                            ),
                            Box::new(t22_star),
                        ))
                    } else {
                        Err("eval1: no rule applies")
                    }
                }?
            }
            // E-APP2
            (v1, _) if v1.isval() => Term::App(
                t1.clone(),
                Box::new(Spanned {
                    v: eval1(&t2.v)?,
                    start: t2.start,
                    line: t2.line,
                    column: t2.column,
                }),
            ),
            // E-APP1
            _ => Term::App(
                Box::new(Spanned {
                    v: eval1(&t1.v)?,
                    start: t1.start,
                    line: t1.line,
                    column: t1.column,
                }),
                t2.clone(),
            ),
        }),
        Term::Abs(_ty, t2) => match &t2.v {
            // E-ETA
            Term::App(t21, tvar0) if zero_in_fv(&t21.v) && tvar0.v == Term::Var(0) => {
                Ok(t21.v.clone())
            }
            _ => Err("eval1: no rule applies".to_string()),
        },
        Term::MAbs(ty, t2) => match &t2.v {
            // E-MUETA
            Term::App(tvar0, t22) if zero_in_fv(&t22.v) && tvar0.v == Term::Var(0) => {
                Ok(t22.v.clone())
            }
            // E-MU
            _ if !t2.v.isval() => Ok(Term::MAbs(
                ty.clone(),
                Box::new(Spanned {
                    v: eval1(&t2.v)?,
                    start: t2.start,
                    line: t2.line,
                    column: t2.column,
                }),
            )),
            _ => Err("eval1: no rule applies".to_string()),
        },
        _ => Err("eval1: no rule applies".to_string()),
    }
}

pub fn eval(t: &Term) -> Result<Term, String> {
    let mut t = t.clone();
    loop {
        t = match eval1(&t) {
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

    #[test]
    fn test_eval1() {
        // id = \x:Bool.x
        let id = Term::Abs(
            Type::TyVar("Bool".to_string()),
            Box::new(spanned(Term::Var(0))),
        );
        {
            // (\x.x) (\x.x) == \x.x
            let t = Term::App(Box::new(spanned(id.clone())), Box::new(spanned(id.clone())));
            assert_eq!(eval(&t).unwrap(), id);
        }

        // tru = \t:Bool.\f:Bool.t
        let tru = Term::Abs(
            Type::TyVar("Bool".to_string()),
            Box::new(spanned(Term::Abs(
                Type::TyVar("Bool".to_string()),
                Box::new(spanned(Term::Var(1))),
            ))),
        );
        // fls = \t:Bool.\f:Bool.f
        let fls = Term::Abs(
            Type::TyVar("Bool".to_string()),
            Box::new(spanned(Term::Abs(
                Type::TyVar("Bool".to_string()),
                Box::new(spanned(Term::Var(0))),
            ))),
        );

        {
            // (\t:Bool.\f:Bool.f) id == id
            let t = Term::App(
                Box::new(spanned(fls.clone())),
                Box::new(spanned(id.clone())),
            );
            assert_eq!(eval(&t).unwrap(), id);
        }

        // and = \b: Bool.\c:Bool.b c fls
        let and = {
            Term::Abs(
                Type::TyVar("Bool".to_string()),
                Box::new(spanned(Term::Abs(
                    Type::TyVar("Bool".to_string()),
                    Box::new(spanned(Term::App(
                        Box::new(spanned(Term::App(
                            Box::new(spanned(Term::Var(1))), // b
                            Box::new(spanned(Term::Var(0))), // c
                        ))),
                        Box::new(spanned(fls.clone())),
                    ))),
                ))),
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
            assert_eq!(eval(&t).unwrap(), fls);
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
            assert_eq!(eval(&t).unwrap(), fls);
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
            assert_eq!(eval(&t).unwrap(), tru);
        }
    }
}
