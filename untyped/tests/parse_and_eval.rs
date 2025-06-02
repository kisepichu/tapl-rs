extern crate untyped;

use rstest::rstest;
use untyped::{eval, span::Spanned};

const EQUAL: &str = r"(
        \\(\\1 0 \\0)
        ((\0(\\0)\\1) ((\\0 (\(\0\\1) (0 (\(\\\0 2 1) ((\0\\0) 0) ((\\\\3 1(2 1 0)) (\\1 0) ((\0\\0) 0))) ((\\\0 2 1) (\\0) \\0))) 1) 0 1))
        ((\0(\\0)\\1) ((\\0 (\(\0\\1) (0 (\(\\\0 2 1) ((\0\\0) 0) ((\\\\3 1(2 1 0)) (\\1 0) ((\0\\0) 0))) ((\\\0 2 1) (\\0) \\0))) 1) 1 0))
    )";

#[rstest]
#[rustfmt::skip]
#[case(r"\0", r"\0")]
#[case(r"(\\1)\0", r"\\0")]
// tru = \\1
// fls = \\0
// and = \\1 0 fls = \\1 0 \\0
#[case(r"(\\1 0 \\0) (\\0) \\0", r"\\0")] // and fls fls == fls
#[case(r"(\\1 0 \\0) (\\1) \\0", r"\\0")] // and tru fls == fls
#[case(r"(\\1 0 \\0) (\\0) \\1", r"\\0")] // and fls tru == fls
#[case(r"(\\1 0 \\0) (\\1) \\1", r"\\1")] // and tru tru == tru
// or = \\1 tru 0 = 1 (\\1) 0
#[case(r"(\\1 (\\1) 0) (\\0) (\\0)", r"\\0")] // or fls fls == fls
#[case(r"(\\1 (\\1) 0) (\\1) (\\0)", r"\\1")] // or tru fls == tru
#[case(r"(\\1 (\\1) 0) (\\0) (\\1)", r"\\1")] // or fls tru == tru
#[case(r"(\\1 (\\1) 0) (\\1) (\\1)", r"\\1")] // or tru tru == tru
// c_0 = \\0
// c_1 = \\1 0
// scc = \\\1(2 1 0)
#[case(r"(\\\1(2 1 0)) \\0", r"\\1 ((\\0) 1 0)")] // scc c_0 == c_1' == \\1 ((\\0) 1 0) -> \\1 0
// pair = \\\0 2 1
// fst = \0 tru = \0\\1
#[case(r"(\0\\1) ((\\\0 2 1) (\\0) \\1 0)", r"\\0")] // fst (pair c_0 c_1) == c_0
// snd = \0 fls = \0\\0
#[case(r"(\0\\0) ((\\\0 2 1) (\\0) \\1 0)", r"\\1 0")] // snd (pair c_0 c_1) == c_1
// iszro = \0(\fls)tru = \0(\\0)\\1
#[case(r"(\0(\\\0)\\1)\\0", r"\\1")] // iszro c_0 == tru
#[case(r"(\0(\\\0)\\1)\\1 0", r"\\0")] // iszro c_1 == fls
#[case(r"(\0(\\\0)\\1)\\1(1 0)", r"\\0")] // iszro c_2 == fls
// plus = \\\\3 1(2 1 0)
#[case(r"(\\\\3 1(2 1 0)) (\\0) \\0", r"\\(\\0) 1 ((\\0) 1 0)")] // plus c_0 c_0 == c_0' == \\(\\0) 1 ((\\0) 1 0) -> \\(\\0) 1 0 -> \\0
#[case(r"(\\\\3 1(2 1 0)) (\\1 0) \\1 0", r"\\(\\1 0) 1 ((\\1 0) 1 0)")] // plus c_1 c_1 == c_2' == \\(\\1 0) 1 ((\\1 0) 1 0) -> \\(\\1 0) 1 (1 0) -> \\1(1 0)
fn test_parse_and_eval(#[case] input: &str, #[case] expected: &str) {
    let t = Spanned {
        v: untyped::parser::parse_and_render_err(input).unwrap(),
        start: 0,
        line: 0,
        column: 0
    };
    let t = eval::eval(&t).unwrap();
    let s = t.to_string();
    if s != expected {
        println!("got term: {:?}",t);
        panic!("expected: {}, \ngot:      {}", expected, s);
    }
}

#[rstest]
#[rustfmt::skip]
// zz = pair c_0 c_0 = (\\\0 2 1)(\\0) \\0
// ss = \pair (snd 0) (plus c_1 (snd 0)) = \(\\\0 2 1) ((\0\\0) 0) ((\\\\3 1(2 1 0)) (\\1 0) ((\0\\0) 0))
// prd = \fst (0 ss zz) = \(\0\\1) (0 (\(\\\0 2 1) ((\0\\0) 0) ((\\\\3 1(2 1 0)) (\\1 0) ((\0\\0) 0))) ((\\\0 2 1)(\\0) \\0))
// sub = \\0 prd 1 = \\0 (\(\0\\1) (0 (\(\\\0 2 1) ((\0\\0) 0) ((\\\\3 1(2 1 0)) (\\1 0) ((\0\\0) 0))) ((\\\0 2 1) (\\0) \\0))) 1
// equal = \\and (iszro (sub 0 1)) (iszro (sub 1 0))
// = \\(\\1 0 \\0)
//   ((\0(\\0)\\1) ((\\0 (\(\0\\1) (0 (\(\\\0 2 1) ((\0\\0) 0) ((\\\\3 1(2 1 0)) (\\1 0) ((\0\\0) 0))) ((\\\0 2 1) (\\0) \\0))) 1) 0 1))
//   ((\0(\\0)\\1) ((\\0 (\(\0\\1) (0 (\(\\\0 2 1) ((\0\\0) 0) ((\\\\3 1(2 1 0)) (\\1 0) ((\0\\0) 0))) ((\\\0 2 1) (\\0) \\0))) 1) 1 0))
#[case(r"((\\\\3 1(2 1 0)) (\\0) \\0) \\0", r"\\1")] // equal (plus c_0 c_0) c_0 == tru
#[case(r"((\\\\3 1(2 1 0)) (\\0) \\1 0) \\1 0", r"\\1")] // equal (plus c_0 c_1) c_1 == tru
#[case(r"((\\\\3 1(2 1 0)) (\\1(1 0)) (\\1(1(1 0))) ) \\1(1(1(1(1 0))))", r"\\1")] // equal (plus c_2 c_3) c_5 == tru
// times = \\\2 (1 0)
#[case(r"((\\\2 (1 0)) (\\1 0) \\0) \\0", r"\\1")] // equal (times c_1 c_0) c_0 == tru
#[case(r"((\\\2 (1 0)) (\\0) \\1(1 0)) \\0", r"\\1")] // equal (times c_0 c_2) c_0 == tru
#[case(r"((\\\2 (1 0)) (\\1(1(1 0))) \\1(1 0)) \\1(1(1(1(1(1 0)))))", r"\\1")] // equal (times c_2 c_3) c_6 == tru
// pow1 = \\0 (times 1) c_1 = \\0 ((\\\2 (1 0)) 1) (\\1 0)
#[case(r"((\\0 ((\\\2 (1 0)) 1) (\\1 0)) (\\1(1 0)) \\1(1(1 0))) \\1(1(1(1(1(1(1(1 0)))))))", r"\\1")] // equal (pow1 c_2 c_3) c_8 == tru
// pow2 = \\0 1
#[case(r"((\\0 1) (\\1(1 0)) \\1(1(1 0))) \\1(1(1(1(1(1(1(1 0)))))))", r"\\1")] // equal (pow2 c_2 c_3) c_8 == tru
#[case(r"((\\0 1) (\\1(1 0)) \\0) \\1 0", r"\\1")] // equal (pow2 c_2 c_0) c_1 == tru
fn test_parse_and_eval_equal(#[case] input: &str, #[case] expected: &str) {
    let input = EQUAL.to_string() + input;
    let t = Spanned {
        v: untyped::parser::parse_and_render_err(input.as_str()).unwrap(),
        start: 0,
        line: 0,
        column: 0
    };
    let t = eval::eval(&t).unwrap();
    let s = t.to_string();
    if s != expected {
        panic!("expected: {}, \ngot:      {}", expected, s);
    }
}
