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
// 11
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
// 21
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
type S = A + B Nat in
case S:::B (succ zero) of
  | S:::A => zero
  | S:::B n => n
    ",
    Some(r"Nat"),
    Some(r"succ zero")
)]
#[case(
    r"
type S = A + B Nat in
case S:::A of
  | S:::A => 0
  | S:::B n => n
    ",
    None,
    None
)]
#[case(
    r"
type S = A + B Nat in
let f = \s:S.
  case s of
    | S:::A => 0
    | S:::B n => n
in
f S:::A
    ",
    None,
    None
)]
#[case(
    r"
type S = <A:Self> in
let f = \s:S.
  case s of
    | S:::A => 0
in
f S:::A
    ",
    Some(r"<A:Self>"),
    Some(r"<A:Self>:::A")
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
// 31
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
#[case(
    r"
type N = Zero + Succ Self in
let iszero_ = \n:N.
  case n of
    | N:::Zero => true
    | N:::Succ p => false
in
iszero_ N:::Zero
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
type N = Zero + Succ Self in
let iszero_ = \n:N.
  case n of
    | N:::Zero => true
    | N:::Succ p => false
in
let two = N:::Succ (N:::Succ N:::Zero) in
iszero_ two
    ",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(
    r"
iszero zero
    ",
    Some(r"Bool"),
    Some(r"true")
)]
#[case(
    r"
iszero (succ zero)
    ",
    Some(r"Bool"),
    Some(r"false")
)]
#[case(
    r"
iszero true
    ",
    None,
    None
)]
// 41
#[case(
    r"
succ true
    ",
    None,
    None
)]
#[case(
    r"
pred(succ(pred(succ(succ (succ zero)))))
    ",
    Some(r"Nat"),
    Some(r"succ (succ zero)")
)]
#[case(
    r"
let iseven = fix \iseven:Nat->Bool.
  \n:Nat.
    if iszero n then
      true
    else if iszero (pred n) then
      false
    else
      iseven (pred (pred n))
in
let two = succ (succ zero) in
let three = succ two in
{iseven two, iseven three}
    ",
    Some(r"{0:Bool, 1:Bool}"),
    Some(r"{0=true, 1=false}")
)]
#[case(
    r"
type SumNat = Zero + Succ Self in
let realnat = fix \realnat:SumNat->Nat.\n:SumNat.
  case n of
    | SumNat:::Zero => zero
    | SumNat:::Succ p => succ (realnat p)
in
let two = SumNat:::Succ (SumNat:::Succ SumNat:::Zero) in
realnat two
    ",
    Some(r"Nat"),
    Some(r"succ (succ zero)")
)]
#[case(
    r"
type B = Real Bool + Church Self->Self->Self in
let tru = B:::Church \t:B.\f:B.t in
let fls = B:::Church \t:B.\f:B.f in
let realbool = \b:B.
  case b of
    | B:::Real rb => rb
    | B:::Church cb =>
      case cb (B:::Real true) (B:::Real false) of
        | B:::Real rb => rb
        | B:::Church _ => false
in
let churchbool = \b:B.
  case b of
    | B:::Real rb => if rb then \t:B.\f:B.t else \t:B.\f:B.f
    | B:::Church cb => cb
in
let churchand = \b:B.\c:B.
  let cb = churchbool b in
  let cc = B:::Church (churchbool c) in
  cb cc fls
in

type N = Real Nat + Church (Self->Self)->Self->Self in
let zro = N:::Church \s:N->N.\z:N.z in
let one = N:::Church \s:N->N.\z:N.s z  in
let two = N:::Church \s:N->N.\z:N.s (s z) in
let suc = \n:N.
  case n of
    | N:::Real rn => N:::Real (succ rn)
    | N:::Church cn => N:::Church \s:N->N.\z:N. s (cn s z)
in
let realnat = \n:N.
  case n of
    | N:::Real rn => rn
    | N:::Church cn =>
      case cn suc (N:::Real zero) of
        | N:::Real rn => rn
        | N:::Church _ => zero
in

{
  realbool tru,
  realbool fls,
  realbool (churchand tru tru),
  realbool (churchand tru fls),
  realbool (churchand fls fls),
  realbool (B:::Church (churchbool (B:::Real true))),
  realbool (B:::Church (churchbool (B:::Real false))),
  realnat zro,
  realnat one,
  realnat (suc two),
}
    ",
    Some(r"{0:Bool, 1:Bool, 2:Bool, 3:Bool, 4:Bool, 5:Bool, 6:Bool, 7:Nat, 8:Nat, 9:Nat}"),
    Some(r"{0=true, 1=false, 2=true, 3=false, 4=false, 5=true, 6=false, 7=zero, 8=succ zero, 9=succ (succ (succ zero))}")
)]
#[case(
    r"
type SumNat = Zero + Succ Self in
type N = SumNat in
let sumnatplus = fix \plus:N->N->N.
  \n:N.\m:N.
    case n of
      | N:::Zero => m
      | N:::Succ p => N:::Succ (plus p m)
in
let two = N:::Succ (N:::Succ N:::Zero) in
let three = N:::Succ (N:::Succ (N:::Succ N:::Zero)) in
let ans = sumnatplus two three in

let realnat = fix \realnat:N->Nat.\n:N.
  case n of
    | N:::Zero => zero
    | N:::Succ p => succ (realnat p)
in
realnat ans
    ",
    Some(r"Nat"),
    Some(r"succ (succ (succ (succ (succ zero))))")
)]
#[case(
    r"
let realnateq = fix \eq:Nat->Nat->Bool.
  \n:Nat.\m:Nat.
    if iszero n then
      iszero m
    else if iszero m then
      false
    else
      eq (pred n) (pred m)
in
let realnatplus = fix \plus:Nat->Nat->Nat.
  \n:Nat.\m:Nat.
    if iszero n then
      m
    else
      succ (plus (pred n) m)
in

let two = succ (succ zero) in
let three = succ two in
let five = succ (succ three) in
{
  realnateq zero zero,
  realnateq two three,
  realnateq five (realnatplus two three)
}
    ",
    Some(r"{0:Bool, 1:Bool, 2:Bool}"),
    Some(r"{0=true, 1=false, 2=true}")
)]
#[case(
    r"
type OptionNat = Some Nat + None in
letrec iseven: OptionNat->Bool = \o:OptionNat.
  case o of
    | OptionNat:::Some n =>
      if iszero n then
        true
      else if iszero (pred n) then
        false
      else
        iseven (OptionNat:::Some (pred (pred n)))
    | OptionNat:::None => false
in
{
  iseven (OptionNat:::Some zero),
  iseven (OptionNat:::Some (succ zero)),
  iseven (OptionNat:::Some (succ (succ zero))),
  iseven OptionNat:::None
}
    ",
    Some(r"{0:Bool, 1:Bool, 2:Bool, 3:Bool}"),
    Some(r"{0=true, 1=false, 2=true, 3=false}")
)]
#[case(
    r"
letrec iseven: Nat->Bool = 
  \n:Nat.
    if iszero n then
      true
    else if iszero (pred n) then
      false
    else
      iseven (pred (pred n))
in
let two = succ (succ zero) in

{
  iseven two,
  iseven (succ two)
}
    ",
    Some(r"{0:Bool, 1:Bool}"),
    Some(r"{0=true, 1=false}")
)]
#[case(
    r"
letrec equal: Nat->Nat->Bool =
  \n:Nat.\m:Nat.
    if iszero n then
      iszero m
    else if iszero m then
      false
    else
      equal (pred n) (pred m)
in
letrec plus: Nat->Nat->Nat =
  \n:Nat.\m:Nat.
    if iszero n then
      m
    else
      succ (plus (pred n) m)
in
letrec times: Nat->Nat->Nat =
  \n:Nat.\m:Nat.
    if iszero n then
      zero
    else
      plus m (times (pred n) m)
in
letrec factorial: Nat->Nat =
  \n:Nat.
    if iszero n then 
      succ zero
    else
      times n (factorial (pred n))
in
let two = succ (succ zero) in
let three = succ two in
let six = succ (succ (succ three)) in

{
  equal six succ (plus two three),
  equal six (times two three),
  equal two (factorial two),
  equal six (factorial three),
}
    ",
    Some(r"{0:Bool, 1:Bool, 2:Bool, 3:Bool}"),
    Some(r"{0=true, 1=true, 2=true, 3=true}")
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
    use fullsimple::parser::parse_spanned;
    use fullsimple::syntax::context::Context;
    use fullsimple::typing::type_of;

    println!("input= {}", input);

    let ctx = Context::default();
    let actual = parse_spanned(input).ok();

    if let Some(t) = actual {
        let ty = type_of(&ctx, &t).map(|t| t.to_string()).ok();
        println!("ty= {:?}", ty);

        if ty.is_some() {
            let eval_result = eval(&t.v).map(|t| t.to_string()).ok();
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
