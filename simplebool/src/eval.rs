use num::FromPrimitive;

use crate::syntax::term::Term;

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
            Term::Abs(ty, t1) => Ok(Term::Abs(ty.clone(), Box::new(walk(t1, d, c + 1)?))),
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
            Term::Abs(ty, t1) => Ok(Term::Abs(ty.clone(), Box::new(walk(j, s, c + 1, t1)?))),
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
            (Term::Abs(_ty, t12), v2) if v2.isval() => term_subst_top(v2, t12)?,
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
    use crate::syntax::r#type::Type;

    use super::*;

    #[test]
    fn test_eval1() {
        // id = \x:Bool.x
        let id = Term::Abs(Type::Bool, Box::new(Term::Var(0)));
        {
            // (\x.x) (\x.x) == \x.x
            let t = Term::App(Box::new(id.clone()), Box::new(id.clone()));
            assert_eq!(eval(&t).unwrap(), id);
        }

        // tru = \t:Bool.\f:Bool.t
        let tru = Term::Abs(
            Type::Bool,
            Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(1)))),
        );
        // fls = \t:Bool.\f:Bool.f
        let fls = Term::Abs(
            Type::Bool,
            Box::new(Term::Abs(Type::Bool, Box::new(Term::Var(0)))),
        );

        {
            // (\t:Bool.\f:Bool.f) id == id
            let t = Term::App(Box::new(fls.clone()), Box::new(id.clone()));
            assert_eq!(eval(&t).unwrap(), id);
        }

        // and = \b: Bool.\c:Bool.b c fls
        let and = {
            Term::Abs(
                Type::Bool,
                Box::new(Term::Abs(
                    Type::Bool,
                    Box::new(Term::App(
                        Box::new(Term::App(
                            Box::new(Term::Var(1)), // b
                            Box::new(Term::Var(0)), // c
                        )),
                        Box::new(fls.clone()),
                    )),
                )),
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

        // realbool = \b:Bool->Bool->Bool.b true false
        let realbool = Term::Abs(
            Type::Bool,
            Box::new(Term::App(
                Box::new(Term::App(Box::new(Term::Var(0)), Box::new(Term::True))),
                Box::new(Term::False),
            )),
        );
        // churchbool = \b:Bool.if b then tru else fls
        let churchbool = Term::Abs(
            Type::Bool,
            Box::new(Term::If(
                Box::new(Term::Var(0)),
                Box::new(tru.clone()),
                Box::new(fls.clone()),
            )),
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
