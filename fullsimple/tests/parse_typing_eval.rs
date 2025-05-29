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
    None,
    None
)]
#[case(
    r" ( \ r : { a : Unit , b : Bool } . r . b ) { a = unit ,  b = false } ",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(
    r"
let f = \:{f:Bool->Bool, cool:Bool, cute:Bool, Bool}.
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
#[case(
    r"
type Ty = <Bool->Bool->Bool->Self, Unit->Self> in
case Ty:::0 false true true of
  | Ty:::0 b c d => b
  | Ty:::1 u => true
    ",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(
    r"
type Ty = <Bool->Bool->Bool->Self, Unit->Self> in
case Ty:::0 false true false of
  | Ty:::1 u => false
  | Ty:::0 b c d => c
    ",
    None,
    None
)]
#[case(
    r"
type Ty = <Unit->Self, Bool->Bool->Bool->Self> in
case Ty:::1 false true false of
  | Ty:::0 u => false
  | Ty:::1 b c d => c
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
let t = true in
let f = false in
type Ty = <Bool->Self, Bool->Bool->Bool->Self> in
let middle = \k:Bool.\x:Bool.\y:Bool.\z:Bool.
  let c =
    if k then
      (\:Unit.Ty:::0) unit x
    else
      Ty:::1 x y z
  in
  case c of
    | Ty:::0 a => f
    | Ty:::1 b c d => c
in
middle f f t f
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
type V = <{x:Bool, y:Bool}->Bool->Self> in
let {V:::0 p z, b:Bool} = {V:::0 {x=true, y=false} false, true} in
p.x
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
type V = <a:{x:Bool, y:Bool}->Bool->Self> in
let {V:::a p z, b:Bool, r:{Bool, Bool}, {sx:Bool, sy:Bool}} = {V:::a {x=true, y=false} false, true, r={false, false}, {sx=false, sy=true}} in
p.x
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
type N = Unit in
type Shape = Circle N N N + Rectangle N N N N in
let iscircle = \s:Shape.
  case s of
    | Shape:::Circle r x y => x; y; r; true
    | Shape:::Rectangle x1 y1 x2 y2 => false
in
iscircle (Shape:::Circle unit unit unit)
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
type N = Bool in
type Pair = N * N in
let swap = \p:Pair. {p.1, p.0} in
swap {true, false}
    ",
    Some(r"{0:Bool, 1:Bool}"),
    Some(r"{0=false, 1=true}")
)]
#[case(
    r"
type P = Pair Bool Bool in
let swap = \p:P.
  case p of
    | P:::Pair x y => P:::Pair y x
in
swap (P:::Pair true false)
    ",
    Some(r"<Pair:Bool->Bool->Self>"),
    Some(r"<Pair:Bool->Bool->Self>:::Pair false true")
)]
#[case(
    r"
type B = T + F in
let not = \b:B.
  case b of
    | B:::T => B:::F
    | B:::F => B:::T
in
not B:::T
    ",
    Some(r"<T:Self, F:Self>"),
    Some(r"<T:Self, F:Self>:::F")
)]
#[case(
    r"
type N = Zero + Succ Self in
let iszero = \n:N.
  case n of
    | N:::Zero => true
    | N:::Succ p => false
in
iszero N:::Zero
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
type N = Zero + Succ Self in
let iszero = \n:N.
  case n of
    | N:::Zero => true
    | N:::Succ p => false
in
let two = N:::Succ (N:::Succ N:::Zero) in
iszero two
    ",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(
    r"
T Bool:::T true
    ",
    None,
    None
)]
#[case(
    r"
(T Bool):::T true
    ",
    Some(r"<T:Bool->Self>"),
    Some(r"<T:Bool->Self>:::T true")
)]
#[case(
    r"
type OptionBool = Some Bool + None in
let issome = \o:OptionBool.
  case o of
    | OptionBool:::Some b => true
    | OptionBool:::None => false
in
issome (OptionBool:::Some true)
    ",
    Some(r"Bool"),
    Some(r"true")
)]
// #[case(
//     r"
//
//     ",
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
    let actual = parse(input).ok();

    if let Some(t) = actual {
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
