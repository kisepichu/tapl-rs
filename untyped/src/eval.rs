use crate::term::Term;

fn term_shift(t: &Term, d: isize) -> Result<Term, String> {
    fn walk(t: &Term, d: isize, c: usize) -> Result<Term, String> {
        match t {
            Term::Var(x) => {
                if *x >= c {
                    let s: usize = (*x as isize + d).try_into().unwrap();
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
            Term::Abs(t1) => Ok(Term::Abs(Box::new(walk(j, s, c + 1, t1)?))),
            Term::App(t1, t2) => Ok(Term::App(
                Box::new(walk(j, s, c, t1)?),
                Box::new(walk(j, s, c, t2)?),
            )),
        }
    }
    walk(j, s, 0, t)
}

fn term_subst_top(s: &Term, t: &Term) -> Result<Term, String> {
    term_shift(&term_subst(0, &term_shift(s, 1)?, t)?, -1)
}

fn isval(t: &Term) -> bool {
    matches!(t, Term::Abs(_))
}

fn eval1(t: &Term) -> Result<Term, String> {
    match t {
        Term::App(t1, t2) => Ok(match (&**t1, &**t2) {
            (Term::Abs(t12), v2) if isval(v2) => term_subst_top(v2, t12)?,
            (v1, t2) if isval(v1) => Term::App(Box::new(v1.clone()), Box::new(eval1(t2)?)),
            _ => Term::App(Box::new(eval1(t1)?), t2.clone()),
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
    use super::*;

    #[test]
    fn test_eval1() {
        // id = \x.x
        let id = Term::Abs(Box::new(Term::Var(0)));
        {
            // (\x.x) (\x.x) == \x.x
            let t = Term::App(Box::new(id.clone()), Box::new(id.clone()));
            assert_eq!(eval(&t).unwrap(), id);
        }

        // tru = \t.\f.t
        let tru = Term::Abs(Box::new(Term::Abs(Box::new(Term::Var(1)))));
        // fls = \t.\f.f
        let fls = Term::Abs(Box::new(Term::Abs(Box::new(Term::Var(0)))));

        {
            // fls id == id
            let t = Term::App(Box::new(fls.clone()), Box::new(id.clone()));
            assert_eq!(eval(&t).unwrap(), id);
        }

        // and = \b.\c.b c fls
        let and = {
            let fls_ctx2 = Term::Abs(Box::new(Term::Abs(Box::new(Term::Var(0)))));
            Term::Abs(Box::new(Term::Abs(Box::new(Term::App(
                Box::new(Term::App(
                    Box::new(Term::Var(1)), // b
                    Box::new(Term::Var(0)), // c
                )),
                Box::new(fls_ctx2.clone()),
            )))))
        };

        {
            // and fls fls == fls
            let t = Term::App(
                Box::new(Term::App(Box::new(and.clone()), Box::new(fls.clone()))),
                Box::new(fls.clone()),
            );
            assert_eq!(eval(&t).unwrap(), fls);
        }

        {
            // and tru fls == fls
            let t = Term::App(
                Box::new(Term::App(Box::new(and.clone()), Box::new(tru.clone()))),
                Box::new(fls.clone()),
            );
            assert_eq!(eval(&t).unwrap(), fls);
        }

        {
            // and tru tru == tru
            let t = Term::App(
                Box::new(Term::App(Box::new(and.clone()), Box::new(tru.clone()))),
                Box::new(tru.clone()),
            );
            assert_eq!(eval(&t).unwrap(), tru);
        }

        // zero = \s.\z.z
        let zero = fls.clone();
        // suc = \n.\s.\z.s (n s z)
        let suc = Term::Abs(Box::new(Term::Abs(Box::new(Term::Abs(Box::new(
            Term::App(
                Box::new(Term::Var(1)),
                Box::new(Term::App(
                    Box::new(Term::App(Box::new(Term::Var(2)), Box::new(Term::Var(1)))),
                    Box::new(Term::Var(0)),
                )),
            ),
        ))))));
        // one = \s.\z. s z
        let one = Term::Abs(Box::new(Term::Abs(Box::new(Term::App(
            Box::new(Term::Var(1)),
            Box::new(Term::Var(0)),
        )))));

        {
            // suc zero ~ one
            let t = Term::App(Box::new(suc.clone()), Box::new(zero.clone()));
            let eval_t = eval(&t).unwrap();

            {
                // eval_t \x.x \x.x == \x.x
                let t1 = Term::App(
                    Box::new(Term::App(Box::new(eval_t.clone()), Box::new(id.clone()))),
                    Box::new(id.clone()),
                );
                let t1o = Term::App(
                    Box::new(Term::App(Box::new(one.clone()), Box::new(id.clone()))),
                    Box::new(id.clone()),
                );
                assert_eq!(eval(&t1).unwrap(), eval(&t1o).unwrap());
                assert_eq!(eval(&t1).unwrap(), id);
            }

            {
                // eval_t \x.\y.x \x.x == \y.\x.x
                let t2 = Term::App(
                    Box::new(Term::App(Box::new(eval_t.clone()), Box::new(tru.clone()))),
                    Box::new(id.clone()),
                );
                let t2o = Term::App(
                    Box::new(Term::App(Box::new(one.clone()), Box::new(tru.clone()))),
                    Box::new(id.clone()),
                );
                assert_eq!(eval(&t2).unwrap(), eval(&t2o).unwrap());
                assert_eq!(eval(&t2).unwrap(), fls);
            }
        }
    }
}
