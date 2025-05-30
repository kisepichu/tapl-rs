# tapl-rs

A Rust implementation of exercises from _Types and Programming Languages_ (Japanese edition).

TaPL として知られる型システム入門という本の演習を Rust で実装したものです。

## Untyped lambda calculus

[`untyped/notes.md`](https://github.com/kisepichu/tapl-rs/tree/main/untyped/notes.md)

```
$ cargo run --bin untyped
> (\\1 0)\0
input= (\\1 0) \0
   ->* \(\0) 0

>
```

## Simply typed lambda calculus + Bool

[`simplebool/notes.md`](https://github.com/kisepichu/tapl-rs/tree/main/simplebool/notes.md)

```
$ cargo run --bin simplebool
> (\:Bool.if 0 then \:Bool.\:Bool.1 else \:Bool.\:Bool.0) true
input= (\:Bool.if 0 then (\:Bool.\:Bool.1) else (\:Bool.\:Bool.0)) true: Bool->Bool->Bool
   ->* \:Bool.\:Bool.1

>
```

## Extensions of simply typed lambda calculus

[`fullsimple/notes.md`](https://github.com/kisepichu/tapl-rs/tree/main/fullsimple/notes.md)

```
$ cargo run --bin fullsimple
> type OptionNat = Some Nat + None in
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

input= let 0 = fix \:<Some:Nat->Self, None:Self>->Bool.\:<Some:Nat->Self, None:Self>.case 0 of | <Some:Nat->Self, None:Self>:::Some 0 => if iszero 0 then true else if iszero pred (0) then false else 2 (<Some:Nat->Self, None:Self>:::Some pred (pred (0)))| <Some:Nat->Self, None:Self>:::None => false in {0=0 (<Some:Nat->Self, None:Self>:::Some zero), 1=0 (<Some:Nat->Self, None:Self>:::Some succ zero), 2=0 (<Some:Nat->Self, None:Self>:::Some succ (succ zero)), 3=0 <Some:Nat->Self, None:Self>:::None}
     : {0:Bool, 1:Bool, 2:Bool, 3:Bool}
   ->* {0=true, 1=false, 2=true, 3=false}

>
```
