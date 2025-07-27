use std::fmt;

use super::r#type::Type;
use crate::span::Spanned;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Info {
    pub name: String,
    pub assumption_num: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(usize, Info),
    TmpVar(String),
    Abs(Type, Box<Spanned<Term>>, Info),
    MAbs(Type, Box<Spanned<Term>>, Info),
    App(Box<Spanned<Term>>, Box<Spanned<Term>>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(term: &Term, has_arg_after: bool, is_app_right: bool) -> String {
            match term {
                Term::Var(_x, info) => info.name.to_string(),
                Term::TmpVar(x) => x.to_string(),
                Term::Abs(ty, t, info) => {
                    if has_arg_after {
                        format!("(\\{}:{}.{})", info.name, ty, p(&t.v, false, false))
                    } else {
                        format!("\\{}:{}.{}", info.name, ty, p(&t.v, false, false))
                    }
                }
                Term::MAbs(ty, t, info) => {
                    if has_arg_after {
                        format!("(/{}:{}.{})", info.name, ty, p(&t.v, false, false))
                    } else {
                        format!("/{}:{}.{}", info.name, ty, p(&t.v, false, false))
                    }
                }
                Term::App(t1, t2) => {
                    if is_app_right {
                        format!(
                            "({} {})",
                            p(&t1.v, true, false),
                            p(&t2.v, has_arg_after, true)
                        )
                    } else {
                        format!(
                            "{} {}",
                            p(&t1.v, true, false),
                            p(&t2.v, has_arg_after, true)
                        )
                    }
                }
            }
        }
        write!(f, "{}", p(self, false, false))
    }
}

impl Term {
    pub fn isval(&self) -> bool {
        matches!(self, Term::Abs(_, _, _)) || matches!(self, Term::Var(_, _))
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
                Term::Var(x, info) => Spanned {
                    v: Term::Var(*x, info.clone()),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::TmpVar(s) => {
                    if z == *s {
                        Spanned {
                            v: Term::Var(
                                c,
                                Info {
                                    name: s.clone(),
                                    assumption_num: c,
                                },
                            ),
                            start: t.start,
                            line: t.line,
                            column: t.column,
                        }
                    } else {
                        t.clone()
                    }
                }
                Term::Abs(ty, t1, info) => Spanned {
                    v: Term::Abs(
                        ty.clone(),
                        Box::new(walk_spanned(t1, z, c + 1)),
                        info.clone(),
                    ),
                    start: t.start,
                    line: t.line,
                    column: t.column,
                },
                Term::MAbs(ty, t1, info) => Spanned {
                    v: Term::MAbs(
                        ty.clone(),
                        Box::new(walk_spanned(t1, z, c + 1)),
                        info.clone(),
                    ),
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
            }
        }
        walk_spanned(spanned_term, zero_name, 0)
    }
}
