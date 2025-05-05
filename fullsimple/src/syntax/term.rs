use std::fmt;

use super::r#type::Type;

#[derive(Debug, Clone)]
pub struct Field {
    pub label: String,
    pub term: Term,
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub pat: Field, // tmp
    pub term: Term,
}

#[derive(Debug, Clone)]
pub enum Term {
    Var(usize),
    TmpVar(String),
    Abs(Type, Box<Term>),
    App(Box<Term>, Box<Term>),
    Unit,
    True,
    False,
    Record(Vec<Field>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
    Projection(Box<Term>, String),
    Tagging(Box<Field>),
    Case(Box<Term>, Vec<Branch>),
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Term::Var(x), Term::Var(y)) => x == y,
            (Term::TmpVar(x), Term::TmpVar(y)) => x == y,
            (Term::Abs(ty1l, t2l), Term::Abs(ty1r, t2r)) => ty1l == ty1r && t2l == t2r,
            (Term::App(t1l, t2l), Term::App(t1r, t2r)) => t1l == t1r && t2l == t2r,
            (Term::Unit, Term::Unit) => true,
            (Term::True, Term::True) => true,
            (Term::False, Term::False) => true,
            (Term::Record(fsl), Term::Record(fsr)) => {
                if fsl.len() == fsr.len() {
                    let mut fsl = fsl.clone();
                    let mut fsr = fsr.clone();
                    fsl.sort_by(|l, r| l.label.cmp(&r.label));
                    fsr.sort_by(|l, r| l.label.cmp(&r.label));
                    fsl.iter()
                        .zip(fsr.iter())
                        .all(|(fl, fr)| fl.label == fr.label && fl.term == fr.term)
                } else {
                    false
                }
            }
            (Term::If(t1l, t2l, tl3), Term::If(t1r, t2r, tr3)) => {
                t1l == t1r && t2l == t2r && tl3 == tr3
            }
            (Term::Let(t1l, t2l), Term::Let(t1r, t2r)) => t1l == t1r && t2l == t2r,
            (Term::Projection(t1l, ll1), Term::Projection(t2r, lr2)) => ll1 == lr2 && t1l == t2r,
            (Term::Tagging(fl), Term::Tagging(fr)) => fl.label == fr.label && fl.term == fr.term,
            (Term::Case(tl, bsl), Term::Case(tr, bsr)) => {
                if tl == tr && bsl.len() == bsr.len() {
                    let mut bsl = bsl.clone();
                    let mut bsr = bsr.clone();
                    bsl.sort_by(|l, r| l.pat.label.cmp(&r.pat.label));
                    bsr.sort_by(|l, r| l.pat.label.cmp(&r.pat.label));
                    bsl.iter().zip(bsr.iter()).all(|(bl, br)| {
                        bl.pat.label == br.pat.label
                            && bl.pat.term == br.pat.term
                            && bl.term == br.term
                    })
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(term: &Term, has_arg_after: bool, is_app_right: bool) -> String {
            match term {
                Term::Var(x) => format!("{}", x),
                Term::TmpVar(x) => x.to_string(),
                Term::Abs(ty, t) => {
                    if has_arg_after {
                        format!("(\\:{}.{})", ty, p(t, false, false))
                    } else {
                        format!("\\:{}.{}", ty, p(t, false, false))
                    }
                }
                Term::App(t1, t2) => {
                    if is_app_right {
                        format!("({} {})", p(t1, true, false), p(t2, has_arg_after, true))
                    } else {
                        format!("{} {}", p(t1, true, false), p(t2, has_arg_after, true))
                    }
                }
                Term::Unit => "unit".to_string(),
                Term::True => "true".to_string(),
                Term::False => "false".to_string(),
                Term::Record(fields) => {
                    let fields_str: Vec<String> = fields
                        .iter()
                        .map(|field| format!("{}={}", field.label, p(&field.term, false, false)))
                        .collect();
                    format!("{{{}}}", fields_str.join(", "))
                }
                Term::Projection(t, label) => {
                    format!("{}.{}", p(t, false, false), label)
                }
                Term::If(t1, t2, t3) => format!(
                    "if {} then {} else {}",
                    p(t1, false, false),
                    p(t2, false, false),
                    if has_arg_after {
                        format!("({})", p(t3, false, false))
                    } else {
                        p(t3, false, false)
                    }
                ),
                Term::Let(t1, t2) => {
                    if has_arg_after {
                        format!(
                            "let 0 = {} in ({})",
                            p(t1, false, false),
                            p(t2, false, false)
                        )
                    } else {
                        format!("let 0 = {} in {}", p(t1, false, false), p(t2, false, false))
                    }
                }
                Term::Tagging(f) => {
                    format!("<{}={}>", f.label, p(&f.term, false, false))
                }
                Term::Case(t, bs) => {
                    format!("case {} of {}", t, {
                        bs.iter()
                            .map(|b| {
                                format!(
                                    "| {} => {} ",
                                    p(&b.term, false, false),
                                    p(&b.pat.term, false, false),
                                )
                            })
                            .fold("".to_string(), |acc, x| acc + &x)
                    })
                }
            }
        }
        write!(f, "{}", p(self, false, false))
    }
}

impl Term {
    pub fn isval(&self) -> bool {
        match self {
            Term::Unit | Term::True | Term::False | Term::Abs(_, _) => true,
            Term::Record(fields) => fields.iter().all(|field| field.term.isval()),
            _ => false,
        }
    }
}
