use rstest::rstest;

extern crate fullsimple;
#[rstest]
#[case(
    r"
    let f=\:Bool.0 in
    let x=true in
    f x",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
    let x = true in
    let f = \:Bool.0 in
    f x",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
    let f=\:Bool.\:Bool.0 in
    let x=true in
    let y=false in
    f x y",
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
fn test_parse_typing_eval(
    #[case] input: &str,
    #[case] expected_type: Option<&str>,
    #[case] expected_eval: Option<&str>,
) {
    use fullsimple::eval::eval;
    use fullsimple::parser::parse;
    use fullsimple::syntax::context::Context;
    use fullsimple::typing::type_of;

    let ctx = Context::default();
    let t = parse(input).unwrap();
    let ty = type_of(&ctx, &t).map(|t| t.to_string()).ok();
    let eval_result = eval(&t).map(|t| t.to_string()).ok();

    assert_eq!(ty, expected_type.map(|s| s.to_string()));
    assert_eq!(eval_result, expected_eval.map(|s| s.to_string()));
}
