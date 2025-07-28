use std::fmt;

use super::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct PatField {
    pub label: String,
    pub pat: Pattern,
}

impl fmt::Display for PatField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}={}", self.label, self.pat)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PTmpTag {
    pub ty: Type,
    pub label: String,
    pub nargs: Vec<String>,
}

impl PTmpTag {
    pub fn len(&self) -> usize {
        self.nargs.len()
    }

    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn subst_type_name(&self, type_name: &str, ty2: &Type) -> PTmpTag {
        PTmpTag {
            ty: self.ty.subst_name(type_name, ty2),
            label: self.label.clone(),
            nargs: self.nargs.clone(),
        }
    }
}

impl fmt::Display for PTmpTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:::{}{}",
            self.ty,
            self.label,
            if self.nargs.is_empty() {
                "".to_string()
            } else {
                format!(" {}", self.nargs.join(" "))
            }
        )
    }
}

impl Pattern {
    pub fn len(&self) -> usize {
        match self {
            Pattern::Var(_, _) => 1,
            Pattern::Record(pfs) => pfs.iter().map(|pf| pf.pat.len()).sum(),
            Pattern::TmpTagging(ptag) => ptag.len(),
        }
    }

    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn subst_type_name(&self, type_name: &str, ty2: &Type) -> Pattern {
        match self {
            Pattern::Var(label, ty) => Pattern::Var(label.clone(), ty.subst_name(type_name, ty2)),
            Pattern::Record(pfs) => Pattern::Record(
                pfs.iter()
                    .map(|pf| PatField {
                        label: pf.label.clone(),
                        pat: pf.pat.subst_type_name(type_name, ty2),
                    })
                    .collect(),
            ),
            Pattern::TmpTagging(ptag) => Pattern::TmpTagging(ptag.subst_type_name(type_name, ty2)),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Var(name, ty) => write!(f, "{}:{}", name, ty),
            Pattern::Record(fields) => {
                let fields_str: Vec<String> = fields.iter().map(|f| f.to_string()).collect();
                write!(f, "{{{}}}", fields_str.join(", "))
            }
            Pattern::TmpTagging(ptag) => write!(f, "{}", ptag),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Var(String, Type),
    Record(Vec<PatField>),
    TmpTagging(PTmpTag),
}
