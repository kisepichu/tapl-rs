use std::fmt;

use rstest::rstest;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Arr(Box<Type>, Box<Type>),
    Bool,
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
                Type::Bool => "Bool".to_string(),
            }
        }
        write!(f, "{}", p(self, false))
    }
}

#[rstest]
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
                Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool))),
            )),
            Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
        ))
    ),
    r"Bool->(Bool->Bool->Bool)->Bool->Bool"
)]
fn test_display(#[case] ty: Type, #[case] expected: &str) {
    assert_eq!(ty.to_string(), expected);
}
