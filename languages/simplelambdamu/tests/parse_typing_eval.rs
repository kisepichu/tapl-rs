use rstest::rstest;

use simplelambdamu::eval::Strategy;

#[rstest]
// (\f:!(N->N)->N->N. /alpha:!(N->N). alpha (f \x:N->N. alpha x)) \k:(N->N)->Bot. /alpha:!(N->N). alpha \n:N.n
#[case(
    Strategy::from("cbv").unwrap(),
    "(\\f:!(N->N)->N->N. /alpha:!(N->N). alpha (f \\x:N->N. alpha x)) \\k:(N->N)->Bot. /alpha:!(N->N). alpha \\n:N.n",
    Some(
        "(\\f:((N->N)->Bot)->N->N./alpha:(N->N)->Bot.alpha (f \\x:N->N.alpha x)) \\k:(N->N)->Bot./alpha:(N->N)->Bot.alpha \\n:N.n"
    ),
    Some("N->N"),
    Some(
        "/alpha:(N->N)->Bot.alpha ((\\k:(N->N)->Bot./alpha':(N->N)->Bot.alpha' \\n:N.n) \\x:N->N.alpha x)"
    )
)]
// \s:N->N. \z:N. /alpha:!N. alpha (s (s /beta:!N. alpha z))
#[case(
    Strategy::from("cbv").unwrap(),
    "\\s:N->N. \\z:N. /alpha:!N. alpha (s (s /beta:!N. alpha z))",
    Some("\\s:N->N.\\z:N./alpha:N->Bot.alpha (s (s /beta:N->Bot.alpha z))"),
    Some("(N->N)->N->N"),
    Some("\\s:N->N.\\z:N./alpha:N->Bot.alpha (s (s /beta:N->Bot.alpha z))")
)]
#[case(
    Strategy::from("normalorderwitheta").unwrap(),
    "\\s:N->N. \\z:N. /alpha:!N. alpha (s (s /beta:!N. alpha z))",
    Some("\\s:N->N.\\z:N./alpha:N->Bot.alpha (s (s /beta:N->Bot.alpha z))"),
    Some("(N->N)->N->N"),
    Some("\\s:N->N.\\z:N.z")
)]
// \g: !!A. (\f:!A->A. /alpha:!A. alpha (f \x:A. alpha x)) \k:A->Bot. /alpha:!A. g k
#[case(
    Strategy::from("cbv").unwrap(),
    "\\g: !!A. (\\f:!A->A. /alpha:!A. alpha (f \\x:A. alpha x)) \\k:A->Bot. /alpha:!A. g k",
    Some(
        "\\g:(A->Bot)->Bot.(\\f:(A->Bot)->A./alpha:A->Bot.alpha (f \\x:A.alpha x)) \\k:A->Bot./alpha:A->Bot.g k"
    ),
    Some("((A->Bot)->Bot)->A"),
    Some(
        "\\g:(A->Bot)->Bot.(\\f:(A->Bot)->A./alpha:A->Bot.alpha (f \\x:A.alpha x)) \\k:A->Bot./alpha:A->Bot.g k"
    )
)]
#[case(
    Strategy::from("normalorderwitheta").unwrap(),
    "\\g: !!A. (\\f:!A->A. /alpha:!A. alpha (f \\x:A. alpha x)) \\k:A->Bot. /alpha:!A. g k",
    Some(
        "\\g:(A->Bot)->Bot.(\\f:(A->Bot)->A./alpha:A->Bot.alpha (f \\x:A.alpha x)) \\k:A->Bot./alpha:A->Bot.g k"
    ),
    Some("((A->Bot)->Bot)->A"),
    Some("\\g:(A->Bot)->Bot./alpha:A->Bot.g alpha")
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
