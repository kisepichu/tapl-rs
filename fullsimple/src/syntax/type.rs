use std::fmt;

use rstest::rstest;

#[derive(Debug, Clone, PartialEq)]
pub struct TyField {
    pub label: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Arr(Box<Type>, Box<Type>),
    Unit,
    Bool,
    TyRecord(Vec<TyField>),
}

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
