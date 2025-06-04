use std::fmt;

use super::{
    pattern::{PTmpTag, PatField, Pattern},
    r#type::Type,
};
use crate::{span::Spanned, syntax::r#type::TyField};

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub label: String,
    pub term: Spanned<Term>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arm {
    pub ptag: PTmpTag,
    pub term: Spanned<Term>,
    pub ptag_pos: (u32, usize), // (line, column) for the pattern position
}

impl fmt::Display for Arm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "| {} => {}", self.ptag, self.term)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tag {
    pub ty: Type,
    pub label: String,
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:::{}", self.ty, self.label)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(usize),
    TmpVar(String),
    Abs(Type, Box<Spanned<Term>>),
    App(Box<Spanned<Term>>, Box<Spanned<Term>>),
    Unit,
    True,
    False,
    Zero,
    Succ(Box<Spanned<Term>>),
    Pred(Box<Spanned<Term>>),
    IsZero(Box<Spanned<Term>>),
    Record(Vec<Field>),
    Tagging(Tag),
    If(Box<Spanned<Term>>, Box<Spanned<Term>>, Box<Spanned<Term>>),
    Let(Box<Spanned<Term>>, Box<Spanned<Term>>),
    Plet(Spanned<Pattern>, Box<Spanned<Term>>, Box<Spanned<Term>>),
    Projection(Box<Spanned<Term>>, String),
    Case(Box<Spanned<Term>>, Vec<Arm>),
    Fix(Box<Spanned<Term>>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print(term: &Term, has_arg_after: bool, is_app_right: bool) -> String {
            match term {
                Term::Var(x) => format!("{}", x),
                Term::TmpVar(x) => x.to_string(),
                Term::Abs(ty, t) => {
                    if has_arg_after {
                        format!("(\\:{}.{})", ty, print(&t.v, false, false))
                    } else {
                        format!("\\:{}.{}", ty, print(&t.v, false, false))
                    }
                }
                Term::App(t1, t2) => {
                    if is_app_right {
                        format!(
                            "({} {})",
                            print(&t1.v, true, false),
                            print(&t2.v, has_arg_after, true)
                        )
                    } else {
                        format!(
                            "{} {}",
                            print(&t1.v, true, false),
                            print(&t2.v, has_arg_after, true)
                        )
                    }
                }
                Term::Unit => "unit".to_string(),
                Term::True => "true".to_string(),
                Term::False => "false".to_string(),
                Term::Zero => "zero".to_string(),
                Term::Succ(t) => {
                    if matches!(t.v, Term::Zero) {
                        format!("succ {}", print(&t.v, false, false))
                    } else {
                        format!("succ ({})", print(&t.v, false, false))
                    }
                }
                Term::Pred(t) => {
                    if matches!(t.v, Term::Zero) {
                        format!("pred {}", print(&t.v, false, false))
                    } else {
                        format!("pred ({})", print(&t.v, false, false))
                    }
                }
                Term::IsZero(t) => {
                    if has_arg_after {
                        format!("iszero ({})", print(&t.v, false, false))
                    } else {
                        format!("iszero {}", print(&t.v, false, false))
                    }
                }
                Term::Record(fields) => {
                    let fields_str: Vec<String> = fields
                        .iter()
                        .map(|field| {
                            format!("{}={}", field.label, print(&field.term.v, false, false))
                        })
                        .collect();
                    format!("{{{}}}", fields_str.join(", "))
                }
                Term::Projection(t, label) => {
                    format!("{}.{}", print(&t.v, false, false), label)
                }
                Term::If(t1, t2, t3) => format!(
                    "if {} then {} else {}",
                    print(&t1.v, false, false),
                    print(&t2.v, false, false),
                    if has_arg_after {
                        format!("({})", print(&t3.v, false, false))
                    } else {
                        print(&t3.v, false, false)
                    }
                ),
                Term::Let(t1, t2) => {
                    if has_arg_after {
                        format!(
                            "let 0 = {} in ({})",
                            print(&t1.v, false, false),
                            print(&t2.v, false, false)
                        )
                    } else {
                        format!(
                            "let 0 = {} in {}",
                            print(&t1.v, false, false),
                            print(&t2.v, false, false)
                        )
                    }
                }
                Term::Plet(p, t1, t2) => {
                    if has_arg_after {
                        format!(
                            "let {} = {} in ({})",
                            p.v,
                            print(&t1.v, false, false),
                            print(&t2.v, false, false)
                        )
                    } else {
                        format!(
                            "let {} = {} in {}",
                            p.v,
                            print(&t1.v, false, false),
                            print(&t2.v, false, false)
                        )
                    }
                }
                Term::Tagging(tag) => tag.to_string(),
                Term::Case(t, bs) => {
                    format!("case {} of {}", print(&t.v, false, false), {
                        bs.iter()
                            .map(|b| b.to_string())
                            .fold("".to_string(), |acc, x| acc + &x)
                    })
                }
                Term::Fix(t) => {
                    if has_arg_after {
                        format!("fix ({})", print(&t.v, false, false))
                    } else {
                        format!("fix {}", print(&t.v, false, false))
                    }
                }
            }
        }
        write!(f, "{}", print(self, false, false))
    }
}

impl Term {
    pub fn isval(&self) -> bool {
        match self {
            Term::Unit | Term::True | Term::False | Term::Zero | Term::Abs(_, _) => true,
            Term::Succ(v1) if v1.v.isval() => true,
            Term::Record(fields) => fields.iter().all(|field| field.term.v.isval()),
            Term::Tagging(_tag) => true,
            Term::App(t1, t2) => {
                fn is_vtag(t: &Term) -> bool {
                    match t {
                        Term::Tagging(_) => true,
                        Term::App(t1, t2) => is_vtag(&t1.v) && t2.v.isval(),
                        _ => false,
                    }
                }
                is_vtag(&t1.v) && t2.v.isval()
            }
            _ => false,
        }
    }

    pub fn shift(&self, d: isize) -> Result<Term, &'static str> {
        use crate::span::dummy_spanned;

        fn shift_help(t: &Term, d: isize, c: usize) -> Result<Term, &'static str> {
            match t {
                Term::Var(x) => {
                    if *x >= c {
                        if d >= 0 {
                            Ok(Term::Var(x + d as usize))
                        } else {
                            let d_abs = (-d) as usize;
                            if *x >= d_abs {
                                Ok(Term::Var(x - d_abs))
                            } else {
                                Err("shift would make negative index")
                            }
                        }
                    } else {
                        Ok(t.clone())
                    }
                }
                Term::TmpVar(_) => Ok(t.clone()),
                Term::Abs(ty, t1) => {
                    let shifted = shift_help(&t1.v, d, c + 1)?;
                    Ok(Term::Abs(ty.clone(), Box::new(dummy_spanned(shifted))))
                }
                Term::App(t1, t2) => {
                    let t1_shifted = shift_help(&t1.v, d, c)?;
                    let t2_shifted = shift_help(&t2.v, d, c)?;
                    Ok(Term::App(
                        Box::new(dummy_spanned(t1_shifted)),
                        Box::new(dummy_spanned(t2_shifted)),
                    ))
                }
                Term::Unit => Ok(Term::Unit),
                Term::True => Ok(Term::True),
                Term::False => Ok(Term::False),
                Term::Zero => Ok(Term::Zero),
                Term::Succ(t1) => {
                    let shifted = shift_help(&t1.v, d, c)?;
                    Ok(Term::Succ(Box::new(dummy_spanned(shifted))))
                }
                Term::Pred(t1) => {
                    let shifted = shift_help(&t1.v, d, c)?;
                    Ok(Term::Pred(Box::new(dummy_spanned(shifted))))
                }
                Term::IsZero(t1) => {
                    let shifted = shift_help(&t1.v, d, c)?;
                    Ok(Term::IsZero(Box::new(dummy_spanned(shifted))))
                }
                Term::Record(fields) => {
                    let shifted_fields: Result<Vec<_>, _> = fields
                        .iter()
                        .map(|field| {
                            let shifted_term = shift_help(&field.term.v, d, c)?;
                            Ok(Field {
                                label: field.label.clone(),
                                term: dummy_spanned(shifted_term),
                            })
                        })
                        .collect();
                    Ok(Term::Record(shifted_fields?))
                }
                Term::Projection(t1, label) => {
                    let shifted = shift_help(&t1.v, d, c)?;
                    Ok(Term::Projection(
                        Box::new(dummy_spanned(shifted)),
                        label.clone(),
                    ))
                }
                Term::If(t1, t2, t3) => {
                    let t1_shifted = shift_help(&t1.v, d, c)?;
                    let t2_shifted = shift_help(&t2.v, d, c)?;
                    let t3_shifted = shift_help(&t3.v, d, c)?;
                    Ok(Term::If(
                        Box::new(dummy_spanned(t1_shifted)),
                        Box::new(dummy_spanned(t2_shifted)),
                        Box::new(dummy_spanned(t3_shifted)),
                    ))
                }
                Term::Let(t1, t2) => {
                    let t1_shifted = shift_help(&t1.v, d, c)?;
                    let t2_shifted = shift_help(&t2.v, d, c + 1)?;
                    Ok(Term::Let(
                        Box::new(dummy_spanned(t1_shifted)),
                        Box::new(dummy_spanned(t2_shifted)),
                    ))
                }
                Term::Plet(p, t1, t2) => {
                    let len = p.v.len();
                    let t1_shifted = shift_help(&t1.v, d, c)?;
                    let t2_shifted = shift_help(&t2.v, d, c + len)?;
                    Ok(Term::Plet(
                        p.clone(),
                        Box::new(dummy_spanned(t1_shifted)),
                        Box::new(dummy_spanned(t2_shifted)),
                    ))
                }
                Term::Tagging(tag) => Ok(Term::Tagging(tag.clone())),
                Term::Case(t1, arms) => {
                    let t1_shifted = shift_help(&t1.v, d, c)?;
                    let arms_shifted: Result<Vec<_>, _> = arms
                        .iter()
                        .map(|arm| {
                            let len = arm.ptag.len();
                            let term_shifted = shift_help(&arm.term.v, d, c + len)?;
                            Ok(Arm {
                                ptag: arm.ptag.clone(),
                                term: dummy_spanned(term_shifted),
                                ptag_pos: arm.ptag_pos,
                            })
                        })
                        .collect();
                    Ok(Term::Case(
                        Box::new(dummy_spanned(t1_shifted)),
                        arms_shifted?,
                    ))
                }
                Term::Fix(t1) => {
                    let shifted = shift_help(&t1.v, d, c)?;
                    Ok(Term::Fix(Box::new(dummy_spanned(shifted))))
                }
            }
        }

        shift_help(self, d, 0)
    }

    pub fn subst_name(&self, zero_name: &str) -> Term {
        use crate::span::dummy_spanned;

        fn walk(t: &Term, z: &str, c: usize) -> Term {
            match t {
                Term::Var(x) => Term::Var(*x),
                Term::TmpVar(s) => {
                    if z == *s {
                        Term::Var(c)
                    } else {
                        t.clone()
                    }
                }
                Term::Abs(ty, t1) => {
                    Term::Abs(ty.clone(), Box::new(dummy_spanned(walk(&t1.v, z, c + 1))))
                }
                Term::App(t1, t2) => Term::App(
                    Box::new(dummy_spanned(walk(&t1.v, z, c))),
                    Box::new(dummy_spanned(walk(&t2.v, z, c))),
                ),
                Term::Unit => Term::Unit,
                Term::True => Term::True,
                Term::False => Term::False,
                Term::Zero => Term::Zero,
                Term::Succ(t1) => Term::Succ(Box::new(dummy_spanned(walk(&t1.v, z, c)))),
                Term::Pred(t1) => Term::Pred(Box::new(dummy_spanned(walk(&t1.v, z, c)))),
                Term::IsZero(t1) => Term::IsZero(Box::new(dummy_spanned(walk(&t1.v, z, c)))),
                Term::Record(fields) => {
                    let fields = fields
                        .iter()
                        .map(|field| Field {
                            label: field.label.clone(),
                            term: dummy_spanned(walk(&field.term.v, z, c)),
                        })
                        .collect();
                    Term::Record(fields)
                }
                Term::Projection(t, label) => {
                    Term::Projection(Box::new(dummy_spanned(walk(&t.v, z, c))), label.clone())
                }
                Term::If(t1, t2, t3) => Term::If(
                    Box::new(dummy_spanned(walk(&t1.v, z, c))),
                    Box::new(dummy_spanned(walk(&t2.v, z, c))),
                    Box::new(dummy_spanned(walk(&t3.v, z, c))),
                ),
                Term::Let(t1, t2) => {
                    let t1 = walk(&t1.v, z, c);
                    let t2 = walk(&t2.v, z, c + 1);
                    Term::Let(Box::new(dummy_spanned(t1)), Box::new(dummy_spanned(t2)))
                }
                Term::Plet(p, t1, t2) => {
                    let n = p.v.len();
                    let t1 = walk(&t1.v, z, c + n);
                    let t2 = walk(&t2.v, z, c + n);
                    Term::Plet(
                        p.clone(),
                        Box::new(dummy_spanned(t1)),
                        Box::new(dummy_spanned(t2)),
                    )
                }
                Term::Tagging(_) => t.clone(),
                Term::Case(t, bs) => Term::Case(
                    Box::new(dummy_spanned(walk(&t.v, z, c))),
                    bs.iter()
                        .map(|b| Arm {
                            ptag: b.ptag.clone(),
                            term: dummy_spanned(walk(&b.term.v, z, c + b.ptag.len())),
                            ptag_pos: b.ptag_pos,
                        })
                        .collect::<Vec<_>>(),
                ),
                Term::Fix(t1) => Term::Fix(Box::new(dummy_spanned(walk(&t1.v, z, c)))),
            }
        }
        walk(self, zero_name, 0)
    }

    /// 位置情報を保持しながら変数名を数値に変換する
    pub fn subst_name_spanned(
        &self,
        zero_name: &str,
        spanned_term: &Spanned<Term>,
    ) -> Spanned<Term> {
        use crate::span::Spanned;

        fn walk_spanned(t: &Spanned<Term>, z: &str, c: usize) -> Spanned<Term> {
            match &t.v {
                Term::Var(x) => Spanned {
                    v: Term::Var(*x),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::TmpVar(s) => {
                    if z == *s {
                        Spanned {
                            v: Term::Var(c),
                            start: t.start,
                            line: t.line,
                            column: t.column,
                        }
                    } else {
                        t.clone()
                    }
                }
                Term::Abs(ty, t1) => Spanned {
                    v: Term::Abs(ty.clone(), Box::new(walk_spanned(t1, z, c + 1))),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::App(t1, t2) => Spanned {
                    v: Term::App(
                        Box::new(walk_spanned(t1, z, c)),
                        Box::new(walk_spanned(t2, z, c)),
                    ),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::Unit => t.clone(),
                Term::True => t.clone(),
                Term::False => t.clone(),
                Term::Zero => t.clone(),
                Term::Succ(t1) => Spanned {
                    v: Term::Succ(Box::new(walk_spanned(t1, z, c))),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::Pred(t1) => Spanned {
                    v: Term::Pred(Box::new(walk_spanned(t1, z, c))),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::IsZero(t1) => Spanned {
                    v: Term::IsZero(Box::new(walk_spanned(t1, z, c))),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::Record(fields) => {
                    let fields = fields
                        .iter()
                        .map(|field| Field {
                            label: field.label.clone(),
                            term: walk_spanned(&field.term, z, c),
                        })
                        .collect();
                    Spanned {
                        v: Term::Record(fields),
                        start: t.start,
                        line: t.line,
                        column: t.column,
                    }
                }
                Term::Projection(t_inner, label) => Spanned {
                    v: Term::Projection(Box::new(walk_spanned(t_inner, z, c)), label.clone()),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::If(t1, t2, t3) => Spanned {
                    v: Term::If(
                        Box::new(walk_spanned(t1, z, c)),
                        Box::new(walk_spanned(t2, z, c)),
                        Box::new(walk_spanned(t3, z, c)),
                    ),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::Let(t1, t2) => {
                    let t1_new = walk_spanned(t1, z, c);
                    let t2_new = walk_spanned(t2, z, c + 1);
                    Spanned {
                        v: Term::Let(Box::new(t1_new), Box::new(t2_new)),
                        start: t.start,
                        line: t.line,
                        column: t.column,
                    }
                }
                Term::Plet(p, t1, t2) => {
                    let n = p.v.len();
                    let t1_new = walk_spanned(t1, z, c + n);
                    let t2_new = walk_spanned(t2, z, c + n);
                    Spanned {
                        v: Term::Plet(p.clone(), Box::new(t1_new), Box::new(t2_new)),
                        start: t.start,
                        line: t.line,
                        column: t.column,
                    }
                }
                Term::Tagging(_) => t.clone(),
                Term::Case(t_case, bs) => {
                    let t_case_new = walk_spanned(t_case, z, c);
                    let bs_new = bs
                        .iter()
                        .map(|b| Arm {
                            ptag: b.ptag.clone(),
                            term: walk_spanned(&b.term, z, c + b.ptag.len()),
                            ptag_pos: b.ptag_pos,
                        })
                        .collect::<Vec<_>>();
                    Spanned {
                        v: Term::Case(Box::new(t_case_new), bs_new),
                        start: t.start,
                        line: t.line,
                        column: t.column,
                    }
                }
                Term::Fix(t1) => Spanned {
                    v: Term::Fix(Box::new(walk_spanned(t1, z, c))),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
            }
        }
        walk_spanned(spanned_term, zero_name, 0)
    }

    /// 位置情報を保持しながら型名を置換する
    pub fn subst_type_name_spanned(
        &self,
        type_name: &str,
        ty2: &Type,
        spanned_term: &Spanned<Term>,
    ) -> Spanned<Term> {
        use crate::span::Spanned;

        fn walk_type_spanned(t: &Spanned<Term>, type_name: &str, ty2: &Type) -> Spanned<Term> {
            match &t.v {
                Term::Var(_) => t.clone(),
                Term::TmpVar(_) => t.clone(),
                Term::Abs(ty, t1) => Spanned {
                    v: Term::Abs(
                        ty.subst_name(type_name, ty2),
                        Box::new(walk_type_spanned(t1, type_name, ty2)),
                    ),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::Unit => t.clone(),
                Term::True => t.clone(),
                Term::False => t.clone(),
                Term::Zero => t.clone(),
                Term::Succ(t1) => Spanned {
                    v: Term::Succ(Box::new(walk_type_spanned(t1, type_name, ty2))),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::Pred(t1) => Spanned {
                    v: Term::Pred(Box::new(walk_type_spanned(t1, type_name, ty2))),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::IsZero(t1) => Spanned {
                    v: Term::IsZero(Box::new(walk_type_spanned(t1, type_name, ty2))),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::App(t1, t2) => Spanned {
                    v: Term::App(
                        Box::new(walk_type_spanned(t1, type_name, ty2)),
                        Box::new(walk_type_spanned(t2, type_name, ty2)),
                    ),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::Record(fields) => {
                    let fields = fields
                        .iter()
                        .map(|field| Field {
                            label: field.label.clone(),
                            term: walk_type_spanned(&field.term, type_name, ty2),
                        })
                        .collect();
                    Spanned {
                        v: Term::Record(fields),
                        start: t.start,
                        line: t.line,
                        column: t.column,
                    }
                }
                Term::Projection(t_inner, label) => Spanned {
                    v: Term::Projection(
                        Box::new(walk_type_spanned(t_inner, type_name, ty2)),
                        label.clone(),
                    ),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::If(t1, t2, t3) => Spanned {
                    v: Term::If(
                        Box::new(walk_type_spanned(t1, type_name, ty2)),
                        Box::new(walk_type_spanned(t2, type_name, ty2)),
                        Box::new(walk_type_spanned(t3, type_name, ty2)),
                    ),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::Let(t1, t2) => {
                    let t1_new = walk_type_spanned(t1, type_name, ty2);
                    let t2_new = walk_type_spanned(t2, type_name, ty2);
                    Spanned {
                        v: Term::Let(Box::new(t1_new), Box::new(t2_new)),
                        start: t.start,
                        line: t.line,
                        column: t.column,
                    }
                }
                Term::Plet(p, t1, t2) => {
                    let p_new = p.v.subst_type_name(type_name, ty2);
                    let t1_new = walk_type_spanned(t1, type_name, ty2);
                    let t2_new = walk_type_spanned(t2, type_name, ty2);
                    Spanned {
                        v: Term::Plet(
                            Spanned {
                                v: p_new,
                                start: p.start,
                                line: p.line,
                                column: p.column,
                            },
                            Box::new(t1_new),
                            Box::new(t2_new),
                        ),
                        start: t.start,
                        line: t.line,
                        column: t.column,
                    }
                }
                Term::Tagging(tag) => Spanned {
                    v: Term::Tagging(Tag {
                        ty: tag.ty.subst_name(type_name, ty2),
                        label: tag.label.clone(),
                    }),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::Case(t_case, bs) => {
                    let t_case_new = walk_type_spanned(t_case, type_name, ty2);
                    let bs_new = bs
                        .iter()
                        .map(|b| Arm {
                            ptag: b.ptag.subst_type_name(type_name, ty2),
                            term: walk_type_spanned(&b.term, type_name, ty2),
                            ptag_pos: b.ptag_pos,
                        })
                        .collect::<Vec<_>>();
                    Spanned {
                        v: Term::Case(Box::new(t_case_new), bs_new),
                        start: t.start,
                        line: t.line,
                        column: t.column,
                    }
                }
                Term::Fix(t1) => Spanned {
                    v: Term::Fix(Box::new(walk_type_spanned(t1, type_name, ty2))),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
            }
        }
        walk_type_spanned(spanned_term, type_name, ty2)
    }

    pub fn subst_ptag(
        &self,
        ptag: &PTmpTag,
        offset: usize,
    ) -> Result<(Term, PTmpTag, usize), &str> {
        let t_renamed = ptag.nargs.iter().fold(self.clone(), |acc, arg| {
            acc.shift(1)
                .expect("plus shift does not fail")
                .subst_name(arg)
        });
        let args_renamed = (0..ptag.nargs.len())
            .rev()
            .map(|i| (i + offset).to_string())
            .collect();
        let ptag_renamed = PTmpTag {
            ty: ptag.ty.clone(),
            label: ptag.label.clone(),
            nargs: args_renamed,
        };
        Ok((t_renamed, ptag_renamed, ptag.nargs.len()))
    }

    pub fn subst_label(&self, from: &str, to: &str) -> Term {
        use crate::span::dummy_spanned;

        match &self {
            Term::Record(fs) => Term::Record(
                fs.iter()
                    .map(|f| Field {
                        label: if f.label == from {
                            to.to_string()
                        } else {
                            f.label.clone()
                        },
                        term: dummy_spanned(f.term.v.subst_label(from, to)),
                    })
                    .collect(),
            ),
            _ => self.clone(),
        }
    }

    pub fn subst_pat(
        &self,
        t1: &Term,
        p: &Pattern,
        offset: usize,
    ) -> Result<(Term, Term, Pattern, usize), &str> {
        match p {
            Pattern::Var(name, ty) => {
                let t = self.subst_name(name.as_str());
                let p = Pattern::Var(offset.to_string(), ty.clone());
                Ok((t, t1.clone(), p, 1))
            }
            Pattern::Record(pfs) => {
                let mut n = pfs.iter().fold(0, |acc, pf| {
                    acc + pf.pat.len()
                        + if matches!(pf.pat, Pattern::Var(_, _)) {
                            0
                        } else {
                            1
                        }
                });
                // inside patterns
                let (t_, t1_, pfs_r, original_names) = pfs.iter().enumerate().try_rfold(
                    (self.clone(), t1.clone(), vec![], vec![]),
                    |(acc_t, acc_t1, mut acc_pfs, mut acc_original_names), (i, pf)| match pf
                        .pat
                        .clone()
                    {
                        Pattern::Var(label, _ty) => {
                            acc_original_names.push(label.clone());
                            acc_pfs.push(PatField {
                                label,
                                pat: Pattern::Var((offset + i).to_string(), _ty),
                            });
                            Ok((acc_t, acc_t1, acc_pfs, acc_original_names))
                        }
                        _ => {
                            let len = pf.pat.len();
                            n -= len;
                            println!("inside pattern found: {}", pf.pat);
                            println!("acc_t1: {}", acc_t1);
                            let (acc_t_, acc_t1_, pi, ni) = acc_t
                                .subst_pat(&acc_t1, &pf.pat, offset + n)
                                .map_err(|_| "internal error: subst_pat")?;
                            if ni != len {
                                return Err("internal error: len check failed");
                            }
                            acc_original_names.push(pf.label.clone());
                            acc_pfs.push(PatField {
                                label: pf.label.clone(),
                                pat: pi,
                            });
                            Ok((acc_t_, acc_t1_, acc_pfs, acc_original_names))
                        }
                    },
                )?;
                // 0..n patterns
                let (t_, t1_, pfs_r) = pfs_r.iter().zip(original_names).rev().enumerate().rfold(
                    (t_, t1_, vec![]),
                    |(acc_t, acc_t1, mut acc_pfs), (i, (pf, original_name))| match pf.pat.clone() {
                        Pattern::Var(name, ty) => {
                            acc_pfs.push(PatField {
                                label: i.to_string(),
                                pat: pf.pat.clone(),
                            });
                            println!("Var name: {}", original_name);
                            (
                                acc_t
                                    .shift(1)
                                    .expect("plus shift does not fail")
                                    .subst_name(&original_name),
                                acc_t1.subst_label(&pf.label, i.to_string().as_str()),
                                acc_pfs,
                            )
                        }
                        _ => {
                            fn pat_type_parsing(pat: &Pattern) -> Type {
                                match pat {
                                    Pattern::Var(_, ty) => ty.clone(),
                                    Pattern::Record(pfs) => Type::TyRecord(
                                        pfs.iter()
                                            .map(|pf| TyField {
                                                label: pf.label.clone(),
                                                ty: Spanned {
                                                    v: pat_type_parsing(&pf.pat),
                                                    start: 0, // todo
                                                    line: 0,
                                                    column: 0,
                                                },
                                            })
                                            .collect::<Vec<_>>(),
                                    ),
                                    Pattern::TmpTagging(ptag) => ptag.ty.clone(),
                                }
                            }

                            let _ty = pat_type_parsing(&pf.pat);
                            acc_pfs.push(PatField {
                                label: i.to_string(),
                                pat: pf.pat.clone(),
                            });
                            println!("_ name: {}", original_name);
                            (
                                acc_t
                                    .shift(1)
                                    .expect("plus shift does not fail")
                                    .subst_name(&original_name),
                                acc_t1.subst_label(&pf.label, i.to_string().as_str()),
                                acc_pfs,
                            )
                        }
                    },
                );
                let pfs_ = pfs_r.iter().rev().cloned().collect();

                Ok((t_, t1_, Pattern::Record(pfs_), n))
            }
            Pattern::TmpTagging(pttag) => {
                let (t, ptag, n) = self.subst_ptag(pttag, offset)?;
                Ok((t, t1.clone(), Pattern::TmpTagging(ptag), n))
            }
        }
    }
}
