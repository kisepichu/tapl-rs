#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    TmVar(usize),
    TmAbs(Box<Term>),
    TmApp(Box<Term>, Box<Term>),
}

pub fn print_tm(term: &Term) -> String {
    match term {
        Term::TmVar(x) => format!("{}", x),
        Term::TmAbs(t) => format!("(\\{})", print_tm(t)),
        Term::TmApp(t1, t2) => format!("({} {})", print_tm(t1), print_tm(t2)),
    }
}
