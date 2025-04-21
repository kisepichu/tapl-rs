#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    TmVar(usize, isize),
    TmAbs(Box<Term>),
    TmApp(Box<Term>, Box<Term>),
}

pub fn print_tm(term: &Term) -> String {
    match term {
        Term::TmVar(x, _check) => format!("{}", x),
        Term::TmAbs(t) => format!("(\\{})", print_tm(t)),
        Term::TmApp(t1, t2) => format!("({} {})", print_tm(t1), print_tm(t2)),
    }
}

pub fn term_shift(t: &Term, d: isize) -> Term {
    fn walk(t: &Term, d: isize, c: usize) -> Term {
        match t {
            Term::TmVar(x, check) => {
                if *x >= c {
                    let s: usize = (*x as isize + d).try_into().unwrap();
                    Term::TmVar(s, check + d)
                } else {
                    Term::TmVar(*x, check + d)
                }
            }
            Term::TmAbs(t1) => Term::TmAbs(Box::new(walk(t1, d, c + 1))),
            Term::TmApp(t1, t2) => Term::TmApp(Box::new(walk(t1, d, c)), Box::new(walk(t2, d, c))),
        }
    }
    walk(t, d, 0)
}

fn term_subst(j: isize, s: &Term, t: &Term) -> Term {
    fn walk(j: isize, s: &Term, c: isize, t: &Term) -> Term {
        match t {
            Term::TmVar(k, check) => {
                if Some(*k) == (j + c).try_into().ok() {
                    term_shift(s, c)
                } else {
                    Term::TmVar(*k, *check)
                }
            }
            Term::TmAbs(t1) => Term::TmAbs(Box::new(walk(j, s, c + 1, t1))),
            Term::TmApp(t1, t2) => {
                Term::TmApp(Box::new(walk(j, s, c, t1)), Box::new(walk(j, s, c, t2)))
            }
        }
    }
    walk(j, s, 0, t)
}

fn term_subst_top(s: &Term, t: &Term) -> Term {
    term_shift(&term_subst(0, &term_shift(s, 1), t), -1)
}

fn isval(t: &Term) -> bool {
    matches!(t, Term::TmAbs(_))
}

fn eval1(t: &Term) -> Result<Term, String> {
    match t {
        Term::TmApp(t1, t2) => Ok(match (&**t1, &**t2) {
            (Term::TmAbs(t12), v2) if isval(v2) => term_subst_top(v2, t12),
            (v1, t2) if isval(v1) => Term::TmApp(Box::new(v1.clone()), Box::new(eval1(t2)?)),
            _ => Term::TmApp(Box::new(eval1(t1)?), Box::new(*t2.clone())),
        }),
        _ => Err("eval1: no rule applies".to_string()),
    }
}

pub fn eval(t: &Term) -> Term {
    let mut t = t.clone();
    while let Ok(t1) = eval1(&t) {
        t = t1;
    }
    t
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval1() {
        // id = \x.x
        let id = Term::TmAbs(Box::new(Term::TmVar(0, 1)));
        {
            // (\x.x) (\x.x) == \x.x
            let t = Term::TmApp(Box::new(id.clone()), Box::new(id.clone()));
            assert_eq!(eval(&t), id);
        }

        // tru = \t.\f.t
        let tru = Term::TmAbs(Box::new(Term::TmAbs(Box::new(Term::TmVar(1, 2)))));
        // fls = \t.\f.f
        let fls = Term::TmAbs(Box::new(Term::TmAbs(Box::new(Term::TmVar(0, 2)))));

        {
            // fls id == id
            let t = Term::TmApp(Box::new(fls.clone()), Box::new(id.clone()));
            assert_eq!(eval(&t), id);
        }

        // and = \b.\c.b c fls
        let and = {
            let fls_ctx2 = Term::TmAbs(Box::new(Term::TmAbs(Box::new(Term::TmVar(0, 4)))));
            Term::TmAbs(Box::new(Term::TmAbs(Box::new(Term::TmApp(
                Box::new(Term::TmApp(
                    Box::new(Term::TmVar(1, 2)), // b
                    Box::new(Term::TmVar(0, 2)), // c
                )),
                Box::new(fls_ctx2.clone()),
            )))))
        };

        {
            // and fls fls == fls
            let t = Term::TmApp(
                Box::new(Term::TmApp(Box::new(and.clone()), Box::new(fls.clone()))),
                Box::new(fls.clone()),
            );
            assert_eq!(eval(&t), fls);
        }

        {
            // and tru fls == fls
            let t = Term::TmApp(
                Box::new(Term::TmApp(Box::new(and.clone()), Box::new(tru.clone()))),
                Box::new(fls.clone()),
            );
            assert_eq!(eval(&t), fls);
        }

        {
            // and tru tru == tru
            let t = Term::TmApp(
                Box::new(Term::TmApp(Box::new(and.clone()), Box::new(tru.clone()))),
                Box::new(tru.clone()),
            );
            assert_eq!(eval(&t), tru);
        }

        // zero = \s.\z.z
        let zero = fls.clone();
        // suc = \n.\s.\z.s (n s z)
        let suc = Term::TmAbs(Box::new(Term::TmAbs(Box::new(Term::TmAbs(Box::new(
            Term::TmApp(
                Box::new(Term::TmVar(1, 3)),
                Box::new(Term::TmApp(
                    Box::new(Term::TmApp(
                        Box::new(Term::TmVar(2, 3)),
                        Box::new(Term::TmVar(1, 3)),
                    )),
                    Box::new(Term::TmVar(0, 3)),
                )),
            ),
        ))))));
        // one = \s.\z. s z
        let one = Term::TmAbs(Box::new(Term::TmAbs(Box::new(Term::TmApp(
            Box::new(Term::TmVar(1, 2)),
            Box::new(Term::TmVar(0, 2)),
        )))));

        {
            // suc zero ~ one
            let t = Term::TmApp(Box::new(suc.clone()), Box::new(zero.clone()));
            let eval_t = eval(&t);

            {
                // eval_t \x.x \x.x == \x.x
                let t1 = Term::TmApp(
                    Box::new(Term::TmApp(Box::new(eval_t.clone()), Box::new(id.clone()))),
                    Box::new(id.clone()),
                );
                let t1o = Term::TmApp(
                    Box::new(Term::TmApp(Box::new(one.clone()), Box::new(id.clone()))),
                    Box::new(id.clone()),
                );
                assert_eq!(eval(&t1), eval(&t1o));
                assert_eq!(eval(&t1), id);
            }

            {
                // eval_t \x.\y.x \x.x == \y.\x.x
                let t2 = Term::TmApp(
                    Box::new(Term::TmApp(Box::new(eval_t.clone()), Box::new(tru.clone()))),
                    Box::new(id.clone()),
                );
                let t2o = Term::TmApp(
                    Box::new(Term::TmApp(Box::new(one.clone()), Box::new(tru.clone()))),
                    Box::new(id.clone()),
                );
                assert_eq!(eval(&t2), eval(&t2o));
                assert_eq!(eval(&t2), fls);
            }
        }

        // plus = \mnsz.m s (n s z)
        let _plus = Term::TmAbs(Box::new(Term::TmAbs(Box::new(Term::TmAbs(Box::new(
            Term::TmAbs(Box::new(Term::TmApp(
                Box::new(Term::TmApp(
                    Box::new(Term::TmVar(3, 4)), // m
                    Box::new(Term::TmVar(1, 4)), // s
                )),
                Box::new(Term::TmApp(
                    Box::new(Term::TmApp(
                        Box::new(Term::TmVar(2, 4)), // n
                        Box::new(Term::TmVar(1, 4)), // s
                    )),
                    Box::new(Term::TmVar(0, 4)), // z
                )),
            ))),
        ))))));
    }
}
