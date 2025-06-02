use crate::syntax::{
    pattern::Pattern,
    term::{Arm, Field, Term},
};

fn term_subst(j: usize, s: &Term, t: &Term) -> Result<Term, String> {
    use crate::span::dummy_spanned;

    fn walk(j: usize, s: &Term, c: isize, t: &Term) -> Result<Term, String> {
        match t {
            Term::Var(k) => {
                if *k as isize == j as isize + c {
                    s.shift(c).map_err(|e| e.to_string())
                } else {
                    Ok(Term::Var(*k))
                }
            }
            Term::TmpVar(_) => Ok(t.clone()),
            Term::Abs(ty, t1) => Ok(Term::Abs(
                ty.clone(),
                Box::new(dummy_spanned(walk(j, s, c + 1, &t1.v)?)),
            )),
            Term::App(t1, t2) => Ok(Term::App(
                Box::new(dummy_spanned(walk(j, s, c, &t1.v)?)),
                Box::new(dummy_spanned(walk(j, s, c, &t2.v)?)),
            )),
            Term::Unit => Ok(Term::Unit),
            Term::True => Ok(Term::True),
            Term::False => Ok(Term::False),
            Term::Zero => Ok(Term::Zero),
            Term::Succ(t1) => Ok(Term::Succ(Box::new(dummy_spanned(walk(j, s, c, &t1.v)?)))),
            Term::Pred(t1) => Ok(Term::Pred(Box::new(dummy_spanned(walk(j, s, c, &t1.v)?)))),
            Term::IsZero(t1) => Ok(Term::IsZero(Box::new(dummy_spanned(walk(j, s, c, &t1.v)?)))),
            Term::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        Ok::<Field, String>(Field {
                            label: field.label.clone(),
                            term: dummy_spanned(walk(j, s, c, &field.term.v)?),
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Term::Record(fields))
            }
            Term::Projection(t, label) => Ok(Term::Projection(
                Box::new(dummy_spanned(walk(j, s, c, &t.v)?)),
                label.clone(),
            )),
            Term::If(t1, t2, t3) => Ok(Term::If(
                Box::new(dummy_spanned(walk(j, s, c, &t1.v)?)),
                Box::new(dummy_spanned(walk(j, s, c, &t2.v)?)),
                Box::new(dummy_spanned(walk(j, s, c, &t3.v)?)),
            )),
            Term::Let(t1, t2) => Ok(Term::Let(
                Box::new(dummy_spanned(walk(j, s, c, &t1.v)?)),
                Box::new(dummy_spanned(walk(j, s, c + 1, &t2.v)?)),
            )),
            Term::Plet(p, t1, t2) => {
                let t1 = walk(j, s, c, &t1.v)?;
                let t2 = walk(j, s, c + p.v.len() as isize, &t2.v)?;
                Ok(Term::Plet(
                    p.clone(),
                    Box::new(dummy_spanned(t1)),
                    Box::new(dummy_spanned(t2)),
                ))
            }
            Term::Tagging(_) => Ok(t.clone()),
            Term::Case(t, bs) => {
                let t = walk(j, s, c, &t.v)?;
                let bs = bs
                    .iter()
                    .map(|b| {
                        Ok::<_, String>(Arm {
                            ptag: b.ptag.clone(),
                            term: dummy_spanned(walk(j, s, c + b.ptag.len() as isize, &b.term.v)?),
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Term::Case(Box::new(dummy_spanned(t)), bs))
            }
            Term::Fix(t1) => Ok(Term::Fix(Box::new(dummy_spanned(walk(j, s, c, &t1.v)?)))),
        }
    }
    walk(j, s, 0, t)
}

fn term_subst_top(s: &Term, t: &Term) -> Result<Term, String> {
    term_subst(0, &s.shift(1).map_err(|e| e.to_string())?, t)?
        .shift(-1)
        .map_err(|e| e.to_string())
}

fn walk_pattern(p: &Pattern, v1: &Term, t2: &Term) -> Result<Term, String> {
    match p {
        Pattern::Var(_, _) => term_subst_top(v1, t2),
        Pattern::Record(pfs) => {
            let mut t2 = t2.clone();
            if let Term::Record(fs) = v1 {
                for (_pf, f) in pfs.iter().zip(fs.iter()) {
                    t2 = term_subst_top(&f.term.v, &t2)?;
                }
                for (pf, f) in pfs.iter().zip(fs.iter()) {
                    t2 = walk_pattern(&pf.pat, &f.term.v, &t2)?;
                }
                Ok(t2)
            } else {
                Err(format!("internal error: expected Record, but got {}", v1))
            }
        }
        Pattern::TmpTagging(ptag) => {
            let mut v1 = v1.clone();
            let mut t2 = t2.clone();
            for _arg in ptag.nargs.iter().rev() {
                if let Term::App(l, r) = v1 {
                    v1 = l.v;
                    t2 = term_subst_top(&r.v, &t2)?;
                } else {
                    return Err(format!(
                        "internal error: expected tagging or app, but got {}",
                        v1
                    ));
                }
            }
            if let Term::Tagging(tag) = v1 {
                if tag.label != ptag.label {
                    return Err(format!(
                        "internal error: expected tag {}, but got {}",
                        ptag.label, tag.label
                    ));
                }
            } else {
                return Err(format!("internal error: expected tagging, but got {}", v1));
            }
            Ok(t2)
        }
    }
}

fn eval1(t: &Term) -> Result<Term, String> {
    use crate::span::dummy_spanned;

    match t {
        Term::App(t1, t2) => Ok(match (&t1.v, &t2.v) {
            (Term::Abs(_ty, t12), v2) if v2.isval() => term_subst_top(v2, &t12.v)?,
            (v1, t2) if v1.isval() => Term::App(
                Box::new(dummy_spanned(v1.clone())),
                Box::new(dummy_spanned(eval1(t2)?)),
            ),
            _ => Term::App(
                Box::new(dummy_spanned(eval1(&t1.v)?)),
                Box::new(dummy_spanned(t2.v.clone())),
            ),
        }),
        Term::Succ(t1) if !t1.v.isval() => Ok(Term::Succ(Box::new(dummy_spanned(eval1(&t1.v)?)))),
        Term::Pred(t1) => match &t1.v {
            Term::Zero => Ok(Term::Zero),
            Term::Succ(v1) if v1.v.isval() => Ok(v1.v.clone()),
            _ if t1.v.isval() => Err(format!("internal error: expected Nat, but got {}", t1.v)),
            _ => Ok(Term::Pred(Box::new(dummy_spanned(eval1(&t1.v)?)))),
        },
        Term::IsZero(t1) => match &t1.v {
            Term::Zero => Ok(Term::True),
            Term::Succ(v1) if v1.v.isval() => Ok(Term::False),
            _ if t1.v.isval() => Err(format!("internal error: expected Nat, but got {}", t1.v)),
            _ => Ok(Term::IsZero(Box::new(dummy_spanned(eval1(&t1.v)?)))),
        },
        Term::Record(fields) if !t.isval() => {
            let fields = fields
                .iter()
                .map(|field| -> Result<_, _> {
                    Ok::<Field, String>(Field {
                        label: field.label.clone(),
                        term: if field.term.v.isval() {
                            field.term.clone()
                        } else {
                            dummy_spanned(eval1(&field.term.v)?)
                        },
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Term::Record(fields))
        }
        Term::Projection(t, label) => {
            if t.v.isval() {
                if let Term::Record(fields) = &t.v {
                    if let Some(field) = fields.iter().find(|f| f.label == *label) {
                        Ok(field.term.v.clone())
                    } else {
                        Err(format!("internal error: undefined label: {}", label))
                    }
                } else {
                    Err(format!("internal error: expected Record: {}", t.v))
                }
            } else {
                Ok(Term::Projection(
                    Box::new(dummy_spanned(eval1(&t.v)?)),
                    label.clone(),
                ))
            }
        }
        Term::If(t1, t2, t3) => Ok(match (&t1.v, &t2.v, &t3.v) {
            (Term::True, t2, _) => t2.clone(),
            (Term::False, _, t3) => t3.clone(),
            _ => Term::If(
                Box::new(dummy_spanned(eval1(&t1.v)?)),
                Box::new(dummy_spanned(t2.v.clone())),
                Box::new(dummy_spanned(t3.v.clone())),
            ),
        }),
        Term::Let(t1, t2) => match (&t1.v, &t2.v) {
            (v1, t2) if v1.isval() => term_subst(0, v1, t2)?.shift(-1).map_err(|e| e.to_string()),
            _ => Ok(Term::Let(
                Box::new(dummy_spanned(eval1(&t1.v)?)),
                Box::new(dummy_spanned(t2.v.clone())),
            )),
        },
        Term::Plet(p, t1, t2) => match (&t1.v, &t2.v) {
            (v1, t2) if v1.isval() => walk_pattern(&p.v, v1, t2),
            _ => Ok(Term::Plet(
                p.clone(),
                Box::new(dummy_spanned(eval1(&t1.v)?)),
                Box::new(dummy_spanned(t2.v.clone())),
            )),
        },
        Term::Case(v, bs) if v.v.isval() => {
            fn tagapp_to_vec(t: &Term) -> Result<Vec<Term>, String> {
                match t.clone() {
                    Term::App(l, r) => {
                        let mut v = tagapp_to_vec(&l.v)?;
                        v.push(r.v);
                        Ok(v)
                    }
                    Term::Tagging(_) => Ok(vec![t.clone()]),
                    _ => Err(format!(
                        "internal error: expected tagging or app, but got {}",
                        t
                    )),
                }
            }
            let v = tagapp_to_vec(&v.v)?;
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
            let mut t = bj.term.v.clone();
            for vv in v.iter().skip(1).rev() {
                t = term_subst_top(vv, &t)?;
            }
            Ok(t)
        }
        Term::Case(t, bs) => {
            eval1(&t.v).map(|t1| Term::Case(Box::new(dummy_spanned(t1)), bs.clone()))
        }
        Term::Fix(t) => match &t.v {
            Term::Abs(_ty1, t2) => {
                term_subst_top(&Term::Fix(Box::new(dummy_spanned(t.v.clone()))), &t2.v)
            }
            _ if t.v.isval() => Err(format!("internal error: expected Abs, but got {}", t.v)),
            _ => Ok(Term::Fix(Box::new(dummy_spanned(eval1(&t.v)?)))),
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
    if !t.isval() {
        println!("soundness not hold. no rule applies but not a value");
    }
    Ok(t)
}

#[cfg(test)]
mod tests {
    use crate::syntax::r#type::Type;

    use super::*;

    #[test]
    fn test_eval1() {
        use crate::span::dummy_spanned;

        // id = \x:Bool.x
        let id = Term::Abs(Type::Bool, Box::new(dummy_spanned(Term::Var(0))));
        {
            // (\x.x) (\x.x) == \x.x
            let t = Term::App(
                Box::new(dummy_spanned(id.clone())),
                Box::new(dummy_spanned(id.clone())),
            );
            assert_eq!(eval(&t).unwrap(), id);
        }

        // tru = \t:Bool.\f:Bool.t
        let tru = Term::Abs(
            Type::Bool,
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(1))),
            ))),
        );
        // fls = \t:Bool.\f:Bool.f
        let fls = Term::Abs(
            Type::Bool,
            Box::new(dummy_spanned(Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Var(0))),
            ))),
        );

        {
            // (\t:Bool.\f:Bool.f) id == id
            let t = Term::App(
                Box::new(dummy_spanned(fls.clone())),
                Box::new(dummy_spanned(id.clone())),
            );
            assert_eq!(eval(&t).unwrap(), id);
        }

        // and = \b: Bool.\c:Bool.b c fls
        let and = {
            Term::Abs(
                Type::Bool,
                Box::new(dummy_spanned(Term::Abs(
                    Type::Bool,
                    Box::new(dummy_spanned(Term::App(
                        Box::new(dummy_spanned(Term::App(
                            Box::new(dummy_spanned(Term::Var(1))), // b
                            Box::new(dummy_spanned(Term::Var(0))), // c
                        ))),
                        Box::new(dummy_spanned(fls.clone())),
                    ))),
                ))),
            )
        };

        {
            // and fls fls == fls
            let t = Term::App(
                Box::new(dummy_spanned(Term::App(
                    Box::new(dummy_spanned(and.clone())),
                    Box::new(dummy_spanned(fls.clone())),
                ))),
                Box::new(dummy_spanned(fls.clone())),
            );
            assert_eq!(eval(&t).unwrap(), fls);
        }

        {
            // and tru fls == fls
            let t = Term::App(
                Box::new(dummy_spanned(Term::App(
                    Box::new(dummy_spanned(and.clone())),
                    Box::new(dummy_spanned(tru.clone())),
                ))),
                Box::new(dummy_spanned(fls.clone())),
            );
            assert_eq!(eval(&t).unwrap(), fls);
        }

        {
            // and tru tru == tru
            let t = Term::App(
                Box::new(dummy_spanned(Term::App(
                    Box::new(dummy_spanned(and.clone())),
                    Box::new(dummy_spanned(tru.clone())),
                ))),
                Box::new(dummy_spanned(tru.clone())),
            );
            assert_eq!(eval(&t).unwrap(), tru);
        }

        // realbool = \b:Bool->Bool->Bool.b true false
        let realbool = Term::Abs(
            Type::Bool,
            Box::new(dummy_spanned(Term::App(
                Box::new(dummy_spanned(Term::App(
                    Box::new(dummy_spanned(Term::Var(0))),
                    Box::new(dummy_spanned(Term::True)),
                ))),
                Box::new(dummy_spanned(Term::False)),
            ))),
        );
        // churchbool = \b:Bool.if b then tru else fls
        let churchbool = Term::Abs(
            Type::Bool,
            Box::new(dummy_spanned(Term::If(
                Box::new(dummy_spanned(Term::Var(0))),
                Box::new(dummy_spanned(tru.clone())),
                Box::new(dummy_spanned(fls.clone())),
            ))),
        );

        {
            // realbool tru == true
            let t = Term::App(
                Box::new(dummy_spanned(realbool.clone())),
                Box::new(dummy_spanned(tru.clone())),
            );
            assert_eq!(eval(&t).unwrap(), Term::True);
        }
        {
            // realbool fls == false
            let t = Term::App(
                Box::new(dummy_spanned(realbool.clone())),
                Box::new(dummy_spanned(fls.clone())),
            );
            assert_eq!(eval(&t).unwrap(), Term::False);
        }
        {
            // churchbool true == tru
            let t = Term::App(
                Box::new(dummy_spanned(churchbool.clone())),
                Box::new(dummy_spanned(Term::True)),
            );
            assert_eq!(eval(&t).unwrap(), tru);
        }
        {
            // churchbool false == fls
            let t = Term::App(
                Box::new(dummy_spanned(churchbool.clone())),
                Box::new(dummy_spanned(Term::False)),
            );
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
                Box::new(dummy_spanned(Term::Abs(
                    Type::Unit,
                    Box::new(dummy_spanned(Term::App(
                        Box::new(dummy_spanned(Term::Abs(
                            Type::Unit,
                            Box::new(dummy_spanned(Term::Abs(
                                Type::Unit,
                                Box::new(dummy_spanned(Term::True)),
                            ))),
                        ))),
                        Box::new(dummy_spanned(Term::Unit)),
                    ))),
                ))),
                Box::new(dummy_spanned(Term::App(
                    Box::new(dummy_spanned(Term::Abs(
                        Type::Bool,
                        Box::new(dummy_spanned(Term::Unit)),
                    ))),
                    Box::new(dummy_spanned(Term::False)),
                ))),
            );
            assert_eq!(
                eval(&t).unwrap(),
                Term::Abs(Type::Unit, Box::new(dummy_spanned(Term::True)))
            );
        }
    }
}
