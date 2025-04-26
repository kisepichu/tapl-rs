# tapl-rs

A Rust implementation of exercises from _Types and Programming Languages_ (Japanese edition).

## Untyped lambda calculus($\lambda$)

[`untyped/notes.md`](https://github.com/kisepichu/tapl-rs/tree/main/untyped/notes.md)

```
$ cargo run --bin untyped
> (\\1 0)\0
input= (\\1 0) \0
   ->* \(\0) 0

>
```

## Simply typed lambda calculus($\lambda_{\rightarrow}$) + Bool

[`simplebool/notes.md`](https://github.com/kisepichu/tapl-rs/tree/main/simplebool/notes.md)

```
$ cargo run --bin simplebool
> (\:Bool.if 0 then \:Bool.\:Bool.1 else \:Bool.\:Bool.0) true
input= (\:Bool.if 0 then (\:Bool.\:Bool.1) else (\:Bool.\:Bool.0)) true: Bool->Bool->Bool
   ->* \:Bool.\:Bool.1

>
```
