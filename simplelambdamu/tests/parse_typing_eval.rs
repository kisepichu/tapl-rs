use rstest::rstest;

use simplelambdamu::eval::Strategy;

#[rstest]
#[case(
    Strategy::Cbv,
    "(\\f:!(N->N)->N->N. /alpha:!(N->N). alpha (f \\x:N->N. alpha x)) \\k:(N->N)->Bot. /alpha:!(N->N). alpha \\n:N.n",
    Some(
        "(\\:((N->N)->Bot)->N->N./:(N->N)->Bot.0 (1 \\:N->N.1 0)) \\:(N->N)->Bot./:(N->N)->Bot.0 \\:N.0"
    ),
    Some("N->N"),
    Some("/:(N->N)->Bot.0 ((\\:(N->N)->Bot./:(N->N)->Bot.0 \\:N.0) \\:N->N.1 0)")
)]
#[case(
    Strategy::Cbv,
    "\\s:N->N. \\z:N. /alpha:!N. alpha (s (s /beta:!N. alpha z))",
    Some("\\:N->N.\\:N./:N->Bot.0 (2 (2 /:N->Bot.1 2))"),
    Some("(N->N)->N->N"),
    Some("\\:N->N.\\:N./:N->Bot.0 (2 (2 /:N->Bot.1 2))")
)]
#[case(
    Strategy::NormalOrder,
    "\\s:N->N. \\z:N. /alpha:!N. alpha (s (s /beta:!N. alpha z))",
    Some("\\:N->N.\\:N./:N->Bot.0 (2 (2 /:N->Bot.1 2))"),
    Some("(N->N)->N->N"),
    Some("\\:N->N.\\:N.0")
)]
#[case(
    Strategy::Cbv,
    "\\g: !!A. (\\f:!A->A. /alpha:!A. alpha (f \\x:A. alpha x)) \\k:A->Bot. /alpha:!A. g k",
    Some("\\:(A->Bot)->Bot.(\\:(A->Bot)->A./:A->Bot.0 (1 \\:A.1 0)) \\:A->Bot./:A->Bot.2 1"),
    Some("((A->Bot)->Bot)->A"),
    Some("\\:(A->Bot)->Bot.(\\:(A->Bot)->A./:A->Bot.0 (1 \\:A.1 0)) \\:A->Bot./:A->Bot.2 1")
)]
#[case(
    Strategy::NormalOrder,
    "\\g: !!A. (\\f:!A->A. /alpha:!A. alpha (f \\x:A. alpha x)) \\k:A->Bot. /alpha:!A. g k",
    Some("\\:(A->Bot)->Bot.(\\:(A->Bot)->A./:A->Bot.0 (1 \\:A.1 0)) \\:A->Bot./:A->Bot.2 1"),
    Some("((A->Bot)->Bot)->A"),
    Some("\\:(A->Bot)->Bot./:A->Bot.1 0")
)]
fn test_parse_typing_eval(
    #[case] strategy: Strategy,
    #[case] input: &str,
    #[case] expected_parsed: Option<&str>,
    #[case] expected_type: Option<&str>,
    #[case] expected_eval: Option<&str>,
) {
    use simplelambdamu::eval::eval;
    use simplelambdamu::parser::parse_spanned;
    use simplelambdamu::syntax::context::Context;
    use simplelambdamu::typing::type_of;

    println!("input= {}", input);

    let ctx = Context::default();
    let actual = parse_spanned(input).ok();

    if let Some(t) = actual {
        let parsed_str = t.v.to_string();
        println!("parsed= {}", parsed_str);
        assert_eq!(Some(parsed_str.as_str()), expected_parsed);

        let ty = type_of(&ctx, &t).map(|t| t.to_string()).ok();
        println!("ty= {:?}", ty);

        if ty.is_some() {
            let eval_result = eval(&t.v, &strategy).map(|t| t.to_string()).ok();
            println!("eval= {:?}", eval_result);
            assert_eq!(ty, expected_type.map(|s| s.to_string()));
            assert_eq!(eval_result, expected_eval.map(|s| s.to_string()));
        } else {
            assert!(expected_type.is_none());
            assert!(expected_eval.is_none());
        }
    } else {
        assert!(expected_parsed.is_none());
        assert!(expected_type.is_none());
        assert!(expected_eval.is_none());
    }
}
