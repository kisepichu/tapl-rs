use num::FromPrimitive;

use crate::syntax::term::{Arm, Field, Term};

impl Term {
    pub fn shift(&self, d: isize) -> Result<Term, String> {
        fn walk(t: &Term, d: isize, c: usize) -> Result<Term, String> {
            match t {
                Term::Var(x) => {
                    if *x >= c {
                        let s: usize =
                            usize::from_isize(*x as isize + d).ok_or("minus after shift")?;
                        Ok(Term::Var(s))
                    } else {
                        Ok(Term::Var(*x))
                    }
                }
                Term::TmpVar(_) => Ok(t.clone()),
                Term::Abs(ty, t1) => Ok(Term::Abs(ty.clone(), Box::new(walk(t1, d, c + 1)?))),
                Term::App(t1, t2) => Ok(Term::App(
                    Box::new(walk(t1, d, c)?),
                    Box::new(walk(t2, d, c)?),
                )),
                Term::Unit => Ok(Term::Unit),
                Term::True => Ok(Term::True),
                Term::False => Ok(Term::False),
                Term::Record(fields) => {
                    let fields = fields
                        .iter()
                        .map(|field| {
                            Ok::<Field, String>(Field {
                                label: field.label.clone(),
                                term: walk(&field.term, d, c)?,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(Term::Record(fields))
                }
                Term::Projection(t, label) => {
                    Ok(Term::Projection(Box::new(walk(t, d, c)?), label.clone()))
                }
                Term::If(t1, t2, t3) => Ok(Term::If(
                    Box::new(walk(t1, d, c)?),
                    Box::new(walk(t2, d, c)?),
                    Box::new(walk(t3, d, c)?),
                )),
                Term::Let(t1, t2) => Ok(Term::Let(
                    Box::new(walk(t1, d, c)?),
                    Box::new(walk(t2, d, c + 1)?),
                )),
                Term::Plet(_pat, t1, _t2) => {
                    let _t1 = walk(t1, d, c)?;
                    todo!();
                    // let t2 = walk(t2, d, c + 0)?;
                    // Ok(Term::Plet(pat.clone(), Box::new(t1), Box::new(t2)))
                }
                Term::Tagging(_) => Ok(t.clone()),
                Term::Case(t, bs) => {
                    let t = walk(t, d, c)?;
                    let bs = bs
                        .iter()
                        .map(|b| {
                            Ok::<_, String>(Arm {
                                ptag: b.ptag.clone(),
                                term: walk(&b.term, d, c + b.ptag.len())?,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Term::Case(Box::new(t), bs))
                }
            }
        }
        walk(self, d, 0)
    }
}

fn term_subst(j: usize, s: &Term, t: &Term) -> Result<Term, String> {
    fn walk(j: usize, s: &Term, c: isize, t: &Term) -> Result<Term, String> {
        match t {
            Term::Var(k) => {
                if *k as isize == j as isize + c {
                    s.shift(c)
                } else {
                    Ok(Term::Var(*k))
                }
            }
            Term::TmpVar(_) => Ok(t.clone()),
            Term::Abs(ty, t1) => Ok(Term::Abs(ty.clone(), Box::new(walk(j, s, c + 1, t1)?))),
            Term::App(t1, t2) => Ok(Term::App(
                Box::new(walk(j, s, c, t1)?),
                Box::new(walk(j, s, c, t2)?),
            )),
            Term::Unit => Ok(Term::Unit),
            Term::True => Ok(Term::True),
            Term::False => Ok(Term::False),
            Term::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        Ok::<Field, String>(Field {
                            label: field.label.clone(),
                            term: walk(j, s, c, &field.term)?,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Term::Record(fields))
            }
            Term::Projection(t, label) => {
                Ok(Term::Projection(Box::new(walk(j, s, c, t)?), label.clone()))
            }
            Term::If(t1, t2, t3) => Ok(Term::If(
                Box::new(walk(j, s, c, t1)?),
                Box::new(walk(j, s, c, t2)?),
                Box::new(walk(j, s, c, t3)?),
            )),
            Term::Let(t1, t2) => Ok(Term::Let(
                Box::new(walk(j, s, c, t1)?),
                Box::new(walk(j, s, c + 1, t2)?),
            )),
            Term::Plet(_pat, t1, _t2) => {
                let _t1 = walk(j, s, c, t1)?;
                todo!();
                // let t2 = walk(j, s, c + 0, &t2)?;
                // Ok(Term::Plet(pat.clone(), Box::new(t1), Box::new(t2)))
            }
            Term::Tagging(_) => Ok(t.clone()),
            Term::Case(t, bs) => {
                let t = walk(j, s, c, t)?;
                let bs = bs
                    .iter()
                    .map(|b| {
                        Ok::<_, String>(Arm {
                            ptag: b.ptag.clone(),
                            term: walk(j, s, c + b.ptag.len() as isize, &b.term)?,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Term::Case(Box::new(t), bs))
            }
        }
    }
    walk(j, s, 0, t)
}

fn term_subst_top(s: &Term, t: &Term) -> Result<Term, String> {
    term_subst(0, &s.shift(1)?, t)?.shift(-1)
}

fn eval1(t: &Term) -> Result<Term, String> {
    match t {
        Term::App(t1, t2) => Ok(match (&**t1, &**t2) {
            (Term::Abs(_ty, t12), v2) if v2.isval() => term_subst_top(v2, t12)?,
            (v1, t2) if v1.isval() => Term::App(Box::new(v1.clone()), Box::new(eval1(t2)?)),
            _ => Term::App(Box::new(eval1(t1)?), t2.clone()),
        }),
        Term::Record(fields) if !t.isval() => {
            let fields = fields
                .iter()
                .map(|field| -> Result<_, _> {
                    Ok::<Field, String>(Field {
                        label: field.label.clone(),
                        term: if field.term.isval() {
                            Ok(field.term.clone())
                        } else {
                            eval1(&field.term)
                        }?,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Term::Record(fields))
        }
        Term::Projection(t, label) => {
            if t.isval() {
                if let Term::Record(fields) = &**t {
                    if let Some(field) = fields.iter().find(|f| f.label == *label) {
                        Ok(field.term.clone())
                    } else {
                        Err(format!("internal error: undefined label: {}", label))
                    }
                } else {
                    Err(format!("internal error: expected Record: {}", t))
                }
            } else {
                Ok(Term::Projection(Box::new(eval1(t)?), label.clone()))
            }
        }
        Term::If(t1, t2, t3) => Ok(match (&**t1, &**t2, &**t3) {
            (Term::True, t2, _) => t2.clone(),
            (Term::False, _, t3) => t3.clone(),
            _ => Term::If(Box::new(eval1(t1)?), t2.clone(), t3.clone()),
        }),
        Term::Let(t1, t2) => match (&**t1, &**t2) {
            (v1, t2) if v1.isval() => term_subst(0, v1, t2)?.shift(-1),
            _ => Ok(Term::Let(Box::new(eval1(t1)?), t2.clone())),
        },
        Term::Case(v, bs) if v.isval() => {
            fn tagapp_to_vec(t: &Term) -> Result<Vec<Term>, String> {
                match t.clone() {
                    Term::App(l, r) => {
                        let mut v = tagapp_to_vec(&l)?;
                        v.push(*r);
                        Ok(v)
                    }
                    Term::Tagging(_) => Ok(vec![t.clone()]),
                    _ => Err(format!(
                        "internal error: expected tagging or app, but got {}",
                        t
                    )),
                }
            }
            let v = tagapp_to_vec(v)?;
            let labelv0 = {
                if let Term::Tagging(tag) = &v[0] {
                    tag.label.clone()
                } else {
                    return Err(format!(
                        "internal error: expected tagging, but got {}",
                        v[0]
                    ));
                }
            };
            let bj = bs
                .iter()
                .find(|b| b.ptag.label == labelv0)
                .ok_or(format!("internal error: no branch found for {}", labelv0))?;
            let mut t = bj.term.clone();
            for vv in v.iter().skip(1).rev() {
                t = term_subst_top(vv, &t)?;
            }
            Ok(t)
        }
        Term::Case(t, bs) => eval1(t).map(|t1| Term::Case(Box::new(t1), bs.clone())),
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
    if !t.isval() {
        println!("soundness not hold. no rule applies but not value");
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

        {
            // unit ->* unit
            let t = Term::Unit;
            assert_eq!(eval(&t).unwrap(), Term::Unit);
        }

        {
            // (\:Bool.unit) false; unit; \:Unit.true ->* \:Unit.true
            let t = Term::App(
                Box::new(Term::Abs(
                    Type::Unit,
                    Box::new(Term::App(
                        Box::new(Term::Abs(
                            Type::Unit,
                            Box::new(Term::Abs(Type::Unit, Box::new(Term::True))),
                        )),
                        Box::new(Term::Unit),
                    )),
                )),
                Box::new(Term::App(
                    Box::new(Term::Abs(Type::Bool, Box::new(Term::Unit))),
                    Box::new(Term::False),
                )),
            );
            assert_eq!(
                eval(&t).unwrap(),
                Term::Abs(Type::Unit, Box::new(Term::True))
            );
        }
    }
}
