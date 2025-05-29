use std::fmt;

use super::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct PatField {
    pub label: String,
    pub pat: Pattern,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PTmpTag {
    pub ty: Type,
    pub label: String,
    pub nargs: Vec<String>,
}

impl fmt::Display for PTmpTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ps_str = self
            .nargs
            .iter()
            .map(|arg| format!(" {}", arg))
            .collect::<Vec<_>>()
            .join("");
        write!(f, "{}:::{}{}", self.ty, self.label, ps_str)
    }
}

impl PTmpTag {
    pub fn len(&self) -> usize {
        self.nargs.len()
    }
    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Pattern {
    pub fn len(&self) -> usize {
        match self {
            Pattern::Var(_, _) => 1,
            Pattern::Record(pfs) => pfs.iter().fold(0, |acc, pf| acc + pf.pat.len()),
            Pattern::TmpTagging(ptag) => ptag.len(),
        }
    }
    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Var(String, Type),
    Record(Vec<PatField>),
    TmpTagging(PTmpTag),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(pat: &Pattern) -> String {
            match pat {
                Pattern::Var(x, ty) => format!("{}:{}", x, ty),
                Pattern::Record(fields) => {
                    let fields_str: Vec<String> = fields
                        .iter()
                        .map(|field| format!("{}={}", field.label, p(&field.pat)))
                        .collect();
                    format!("{{{}}}", fields_str.join(", "))
                }
                Pattern::TmpTagging(ptag) => ptag.to_string(),
            }
        }
        write!(f, "{}", p(self))
    }
}
