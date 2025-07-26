use num::FromPrimitive;

use crate::{span::Spanned, syntax::term::Term};

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

fn eval1(t: &Term) -> Result<Term, String> {
    match t {
        Term::App(t1, t2) => Ok(match (&t1.v, &t2.v) {
            (Term::Abs(_ty, t12), v2) if v2.isval() => term_subst_top(v2, &t12.v)?,
            (v1, _) if v1.isval() => Term::App(
                t1.clone(),
                Box::new(Spanned {
                    v: eval1(&t2.v)?,
                    start: t2.start,
                    line: t2.line,
                    column: t2.column,
                }),
            ),
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
