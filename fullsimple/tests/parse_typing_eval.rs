use rstest::rstest;

extern crate fullsimple;
#[rstest]
#[case(
    r"
let f=\:Bool.0 in
let x=true in
f x
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
let x = true in
let f = \:Bool.0 in
f x
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
let f=\:Bool.\:Bool.0 in
let x=true in
let y=false in
f x y
    ",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(
    r"
let f = 
  let t = true
    in
  \:Bool->Bool. 0 t
in
f \:Bool.
  if 0 then
    false
  else
    true
    ",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(
    r"
(\f:Bool->Bool.\:Bool.f 0)
(\b:Bool.if b then false else true)
true
    ",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(r"{ a0 = unit , false } . 1", Some(r"Bool"), Some(r"false"))]
#[case(r"{ a0 = unit , false } . 0", None, None)]
#[case(r"{a0=unit, false}.1.0", None, None)]
#[case(r"\:Bool.0 {a0=unit, false}", None, None)]
#[case(r"(\:Bool->Bool.0) {a0=unit, false}", None, None)]
#[case(r"(\:Bool->Bool.0) {a0=unit, false}", None, None)]
#[case(r"(\:Bool.0) {a0=unit, 1=false}", None, None)]
#[case(r"(\:Bool.0) {a0=unit, false}.1", Some(r"Bool"), Some(r"false"))]
#[case(r"(\:{a0:Unit, 1:Bool}. 1.0) {a0=unit, 1=false}.1", None, None)]
#[case(
    r"(\:{Unit, Bool} . 0 . 1 ) {unit, false}",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(
    r" ( \ r : { b : Bool , a : Unit } . r . b ) { a = unit ,  b = false } ",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(
    r"
let f = \:{cool:Bool, cute:Bool, f:Bool->Bool, Bool}.
  if 0.cool then
    if 0.cute then
      0.f false
    else
      false
  else
    false
in
let x = {f=\b:Bool.true, cool=true, cute=false, true} in
let y = {f=\b:Bool.true, cool=true, cute=true, false} in
{f x, f y}
    ",
    Some(r"{0:Bool, 1:Bool}"),
    Some(r"{0=false, 1=true}")
)]
// #[case(
//     r"",
//     Some(r""),
//     Some(r"")
// )]
#[case(r"", None, None)]
fn test_parse_typing_eval(
    #[case] input: &str,
    #[case] expected_type: Option<&str>,
    #[case] expected_eval: Option<&str>,
) {
    use fullsimple::eval::eval;
    use fullsimple::parser::parse;
    use fullsimple::syntax::context::Context;
    use fullsimple::typing::type_of;

    println!("input= {}", input);

    let ctx = Context::default();
    let t = parse(input).ok();

    if let Some(t) = t {
        let ty = type_of(&ctx, &t).map(|t| t.to_string()).ok();
        println!("ty= {:?}", ty);

        if ty.is_some() {
            let eval_result = eval(&t).map(|t| t.to_string()).ok();
            println!("eval= {:?}", eval_result);
            assert_eq!(ty, expected_type.map(|s| s.to_string()));
            assert_eq!(eval_result, expected_eval.map(|s| s.to_string()));
        } else {
            assert!(expected_type.is_none());
            assert!(expected_eval.is_none());
        }
    } else {
        assert!(expected_type.is_none());
        assert!(expected_eval.is_none());
    }
}
