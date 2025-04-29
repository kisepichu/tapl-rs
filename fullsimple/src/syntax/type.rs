use std::fmt;

use rstest::rstest;

#[derive(Debug, Clone)]
pub struct TyField {
    pub label: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Arr(Box<Type>, Box<Type>),
    Unit,
    Bool,
    TyRecord(Vec<TyField>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Arr(t1a, t1b), Type::Arr(t2a, t2b)) => t1a == t2a && t1b == t2b,
            (Type::Unit, Type::Unit) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::TyRecord(fields1), Type::TyRecord(fields2)) => {
                if fields1.len() == fields2.len() {
                    let mut fields1 = fields1.clone();
                    let mut fields2 = fields2.clone();
                    fields1.sort_by(|l, r| l.label.cmp(&r.label));
                    fields2.sort_by(|l, r| l.label.cmp(&r.label));
                    fields1
                        .iter()
                        .zip(fields2.iter())
                        .all(|(e1, e2)| e1.label == e2.label && e1.ty == e2.ty)
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

impl Eq for Type {}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(ty: &Type, is_left_arr: bool) -> String {
            match ty {
                Type::Arr(t1, t2) => {
                    if is_left_arr {
                        format!("({}->{})", p(t1, true), p(t2, false))
                    } else {
                        format!("{}->{}", p(t1, true), p(t2, false))
                    }
                }
                Type::Unit => "Unit".to_string(),
                Type::Bool => "Bool".to_string(),
                Type::TyRecord(fields) => {
                    let fields_str: Vec<String> = fields
                        .iter()
                        .map(|field| format!("{}:{}", field.label, p(&field.ty, false)))
                        .collect();
                    format!("{{{}}}", fields_str.join(", "))
                }
            }
        }
        write!(f, "{}", p(self, false))
    }
}

#[rstest]
#[case(Type::Unit, r"Unit")]
#[case(Type::Bool, r"Bool")]
#[case(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)), r"Bool->Bool")]
#[case(
    Type::Arr(
        Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
        Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
    ),
    r"(Bool->Bool)->Bool->Bool"
)]
#[case(
    Type::Arr(
        Box::new(Type::Bool),
        Box::new(Type::Arr(
            Box::new(Type::Arr(
                Box::new(Type::Bool),
                Box::new(Type::Arr(Box::new(Type::Unit), Box::new(Type::Bool))),
            )),
            Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
        ))
    ),
    r"Bool->(Bool->Unit->Bool)->Bool->Bool"
)]
fn test_display(#[case] ty: Type, #[case] expected: &str) {
    assert_eq!(ty.to_string(), expected);
}
