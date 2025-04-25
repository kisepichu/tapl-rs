use crate::syntax::{term::Term, ty};

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
            Term::Abs(t1, ty) => Ok(Term::Abs(Box::new(walk(t1, d, c + 1)?), ty.clone())),
            Term::App(t1, t2) => Ok(Term::App(
                Box::new(walk(t1, d, c)?),
                Box::new(walk(t2, d, c)?),
            )),
            Term::True => Ok(Term::True),
            Term::False => Ok(Term::False),
            Term::If(t1, t2, t3) => Ok(Term::If(
                Box::new(walk(t1, d, c)?),
                Box::new(walk(t2, d, c)?),
                Box::new(walk(t3, d, c)?),
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
            Term::Abs(t1, ty) => Ok(Term::Abs(Box::new(walk(j, s, c + 1, t1)?), ty.clone())),
            Term::App(t1, t2) => Ok(Term::App(
                Box::new(walk(j, s, c, t1)?),
                Box::new(walk(j, s, c, t2)?),
            )),
            Term::True => Ok(Term::True),
            Term::False => Ok(Term::False),
            Term::If(t1, t2, t3) => Ok(Term::If(
                Box::new(walk(j, s, c, t1)?),
                Box::new(walk(j, s, c, t2)?),
                Box::new(walk(j, s, c, t3)?),
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
        Term::App(t1, t2) => Ok(match (&**t1, &**t2) {
            (Term::Abs(t12, _ty), v2) if v2.isval() => term_subst_top(v2, t12)?,
            (v1, t2) if v1.isval() => Term::App(Box::new(v1.clone()), Box::new(eval1(t2)?)),
            _ => Term::App(Box::new(eval1(t1)?), t2.clone()),
        }),
        Term::If(t1, t2, t3) => Ok(match (&**t1, &**t2, &**t3) {
            (Term::True, t2, _) => t2.clone(),
            (Term::False, _, t3) => t3.clone(),
            _ => Term::If(Box::new(eval1(t1)?), t2.clone(), t3.clone()),
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
    use crate::syntax::ty::Type;

    use super::*;

    #[test]
    fn test_eval1() {
        // id = \x:Bool.x
        let id = Term::Abs(Box::new(Term::Var(0)), Type::Bool);
        {
            // (\x.x) (\x.x) == \x.x
            let t = Term::App(Box::new(id.clone()), Box::new(id.clone()));
            assert_eq!(eval(&t).unwrap(), id);
        }

        // tru = \t:Bool.\f:Bool.t
        let tru = Term::Abs(
            Box::new(Term::Abs(Box::new(Term::Var(1)), Type::Bool)),
            Type::Bool,
        );
        // fls = \t:Bool.\f:Bool.f
        let fls = Term::Abs(
            Box::new(Term::Abs(Box::new(Term::Var(0)), Type::Bool)),
            Type::Bool,
        );

        {
            // (\t:Bool.\f:Bool.f) id == id
            let t = Term::App(Box::new(fls.clone()), Box::new(id.clone()));
            assert_eq!(eval(&t).unwrap(), id);
        }

        // and = \b: Bool.\c:Bool.b c fls
        let and = {
            Term::Abs(
                Box::new(Term::Abs(
                    Box::new(Term::App(
                        Box::new(Term::App(
                            Box::new(Term::Var(1)), // b
                            Box::new(Term::Var(0)), // c
                        )),
                        Box::new(fls.clone()),
                    )),
                    Type::Bool,
                )),
                Type::Bool,
            )
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

        // realbool = \b:Bool.b true false
        let realbool = Term::Abs(
            Box::new(Term::App(
                Box::new(Term::App(Box::new(Term::Var(0)), Box::new(Term::True))),
                Box::new(Term::False),
            )),
            Type::Bool,
        );
        // churchbool = \b.if b then tru else fls
        let churchbool = Term::Abs(
            Box::new(Term::If(
                Box::new(Term::Var(0)),
                Box::new(tru.clone()),
                Box::new(fls.clone()),
            )),
            Type::Bool,
        );

        {
            // realbool tru == true
            let t = Term::App(Box::new(realbool.clone()), Box::new(tru.clone()));
            assert_eq!(eval(&t).unwrap(), Term::True);
        }
        {
            // realbool fls == false
            let t = Term::App(Box::new(realbool.clone()), Box::new(fls.clone()));
            assert_eq!(eval(&t).unwrap(), Term::False);
        }
        {
            // churchbool true == tru
            let t = Term::App(Box::new(churchbool.clone()), Box::new(Term::True));
            assert_eq!(eval(&t).unwrap(), tru);
        }
        {
            // churchbool false == fls
            let t = Term::App(Box::new(churchbool.clone()), Box::new(Term::False));
            assert_eq!(eval(&t).unwrap(), fls);
        }
    }
}
