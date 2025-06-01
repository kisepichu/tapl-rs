use num::FromPrimitive;

use crate::{
    span::{ErrorWithPos, Spanned, map_spanned, spanned, spanned_res},
    syntax::Term,
};

fn term_shift(t: &Spanned<Term>, d: isize) -> Result<Spanned<Term>, ErrorWithPos> {
    fn walk(sp_t: &Spanned<Term>, d: isize, c: usize) -> Result<Spanned<Term>, ErrorWithPos> {
        map_spanned(sp_t, |t| match t {
            Term::Var(x) => {
                if *x >= c {
                    let s: usize =
                        usize::from_isize(*x as isize + d).ok_or_else(|| ErrorWithPos {
                            message:
                                "internal error: term_shift: shift resulted in negative variable"
                                    .to_string(),
                            level: 100,
                            line: sp_t.line,
                            column: sp_t.column,
                            kind: None,
                        })?;
                    Ok(Term::Var(s))
                } else {
                    Ok(Term::Var(*x))
                }
            }
            Term::Abs(t1) => Ok(Term::Abs(Box::new(walk(t1, d, c + 1)?))),
            Term::App(t1, t2) => Ok(Term::App(
                Box::new(walk(t1, d, c)?),
                Box::new(walk(t2, d, c)?),
            )),
        })
    }
    walk(t, d, 0)
}

fn term_subst(
    j: isize,
    s: &Spanned<Term>,
    t: &Spanned<Term>,
) -> Result<Spanned<Term>, ErrorWithPos> {
    fn walk(
        j: isize,
        sp_s: &Spanned<Term>,
        c: isize,
        sp_t: &Spanned<Term>,
    ) -> Result<Spanned<Term>, ErrorWithPos> {
        match &sp_t.v {
            Term::Var(k) => {
                if Some(*k) == (j + c).try_into().ok() {
                    term_shift(sp_s, c)
                } else {
                    spanned_res(sp_t, Ok(Term::Var(*k)))
                }
            }
            Term::Abs(t1) => spanned_res(sp_t, Ok(Term::Abs(Box::new(walk(j, sp_s, c + 1, t1)?)))),
            Term::App(t1, t2) => spanned_res(
                sp_t,
                Ok(Term::App(
                    Box::new(walk(j, sp_s, c, t1)?),
                    Box::new(walk(j, sp_s, c, t2)?),
                )),
            ),
        }
    }
    walk(j, s, 0, t)
}

fn term_subst_top(s: &Spanned<Term>, t: &Spanned<Term>) -> Result<Spanned<Term>, ErrorWithPos> {
    term_shift(&term_subst(0, &term_shift(s, 1)?, t)?, -1)
}

fn eval1(t: &Spanned<Term>) -> Result<Spanned<Term>, ErrorWithPos> {
    match &t.v {
        Term::App(sp_t1, sp_t2) => Ok(match (&sp_t1.v, &sp_t2.v) {
            (Term::Abs(t12), v2) if v2.isval() => term_subst_top(sp_t2, t12)?,
            (v1, _t2) if v1.isval() => {
                spanned(t, Term::App(sp_t1.clone(), Box::new(eval1(sp_t2)?)))
            }
            _ => spanned(t, Term::App(Box::new(eval1(sp_t1)?), sp_t2.clone())),
        }),
        _ => spanned_res(t, Err("eval1: no rule applies".to_string())),
    }
}

pub fn eval(t: &Spanned<Term>) -> Result<Spanned<Term>, ErrorWithPos> {
    let mut t = t.clone();
    loop {
        t = match eval1(&t) {
            Ok(t1) => t1,
            Err(e) => {
                if e.message == "eval1: no rule applies" {
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
    use super::*;

    // dummy Spanned Term for testing
    fn b_sp(t: Term) -> Box<Spanned<Term>> {
        Box::new(Spanned {
            v: t,
            start: 0,
            line: 0,
            column: 0,
        })
    }
    fn b(sp_t: Spanned<Term>) -> Box<Spanned<Term>> {
        Box::new(sp_t)
    }
    fn s(t: Term) -> Spanned<Term> {
        Spanned {
            v: t,
            start: 0,
            line: 0,
            column: 0,
        }
    }

    #[test]
    fn test_eval1() {
        // id = \x.x
        let id = s(Term::Abs(b_sp(Term::Var(0))));
        {
            // (\x.x) (\x.x) == \x.x
            let t = s(Term::App(b(id.clone()), b(id.clone())));
            assert_eq!(eval(&t).unwrap(), id);
        }

        // tru = \t.\f.t
        let tru = s(Term::Abs(b_sp(Term::Abs(b_sp(Term::Var(1))))));
        // fls = \t.\f.f
        let fls = s(Term::Abs(b_sp(Term::Abs(b_sp(Term::Var(0))))));

        {
            // (\t.\f.f) id == id
            let t = b_sp(Term::App(b(fls.clone()), b(id.clone())));
            assert_eq!(eval(&t).unwrap(), id);
        }

        // and = \b.\c.b c fls
        let and = Term::Abs(b_sp(Term::Abs(b_sp(Term::App(
            b_sp(Term::App(
                b_sp(Term::Var(1)), // b
                b_sp(Term::Var(0)), // c
            )),
            b(fls.clone()),
        )))));

        {
            // and fls fls == fls
            let t = s(Term::App(
                b_sp(Term::App(b_sp(and.clone()), b(fls.clone()))),
                b(fls.clone()),
            ));
            assert_eq!(eval(&t).unwrap(), fls);
        }

        {
            // and tru fls == fls
            let t = s(Term::App(
                b_sp(Term::App(b_sp(and.clone()), b(tru.clone()))),
                b(fls.clone()),
            ));
            assert_eq!(eval(&t).unwrap(), fls);
        }

        {
            // and tru tru == tru
            let t = s(Term::App(
                b_sp(Term::App(b_sp(and.clone()), b(tru.clone()))),
                b(tru.clone()),
            ));
            assert_eq!(eval(&t).unwrap(), tru);
        }

        // zero = \s.\z.z
        let zero = fls.clone();
        // suc = \n.\s.\z.s (n s z)
        let suc = Term::Abs(b_sp(Term::Abs(b_sp(Term::Abs(b_sp(Term::App(
            b_sp(Term::Var(1)),
            b_sp(Term::App(
                b_sp(Term::App(b_sp(Term::Var(2)), b_sp(Term::Var(1)))),
                b_sp(Term::Var(0)),
            )),
        )))))));
        // one = \s.\z. s z
        let one = Term::Abs(b_sp(Term::Abs(b_sp(Term::App(
            b_sp(Term::Var(1)),
            b_sp(Term::Var(0)),
        )))));

        {
            // suc zero ~ one
            let t = s(Term::App(b_sp(suc.clone()), b(zero)));
            let eval_t = eval(&t).unwrap();

            {
                // eval_t \x.x \x.x == \x.x
                let t1 = s(Term::App(
                    b_sp(Term::App(b(eval_t.clone()), b(id.clone()))),
                    b(id.clone()),
                ));
                let t1o = s(Term::App(
                    b_sp(Term::App(b_sp(one.clone()), b(id.clone()))),
                    b(id.clone()),
                ));
                assert_eq!(eval(&t1).unwrap(), eval(&t1o).unwrap());
                assert_eq!(eval(&t1).unwrap(), id);
            }

            {
                // eval_t \x.\y.x \x.x == \y.\x.x
                let t2 = s(Term::App(
                    b_sp(Term::App(b(eval_t.clone()), b(tru.clone()))),
                    b(id.clone()),
                ));
                let t2o = s(Term::App(
                    b_sp(Term::App(b_sp(one.clone()), b(tru.clone()))),
                    b(id.clone()),
                ));
                assert_eq!(eval(&t2).unwrap(), eval(&t2o).unwrap());
                assert_eq!(eval(&t2).unwrap(), fls);
            }
        }
    }
}
