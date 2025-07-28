# Untyped lambda calculus

```
cargo run --bin untyped
```

Figure 5-3(p.54), 6.3(p.61)

型無し λ 計算は、 λ 抽象と適用からなる強力な言語で、型付けされていない項を評価する。

## Syntax

`fn parse` in [`untyped/src/parser.rs`](https://github.com/kisepichu/tapl-rs/blob/main/untyped/src/parser.rs), `enum Term` in [`untyped/src/syntax.rs`](https://github.com/kisepichu/tapl-rs/blob/main/untyped/src/syntax.rs)

### Concrete syntax

```bnf
<term> ::= <app>
<app>  ::= <atom> <app> | <atom>
<atom> ::= <encl> | <abs> | <var>
<encl> ::= "(" <term> ")"
<abs> ::= "\" <term>
<var> ::= number
```

### Abstract syntax

```math
\begin{align*}
t ::=&   &\quad (\text{terms}) \\
  \quad \mid\ &x &\quad (\text{variable}) \\
  \quad \mid\ &\lambda.t &\quad (\text{abstraction}) \\
  \quad \mid\ &t_1\ t_2 &\quad (\text{application}) \\
  \\
v ::=&   &\quad (\text{values}) \\
  \quad \mid\ &\lambda.t &\quad (\text{abstraction value}) \\
\end{align*}
```

### parsing

- `<app>` は、 `<atom>` の列が左結合で application に変換される。
- `<abs>` は、 abstraction に変換される。
- `<var>` は、 variable に変換される。

## evaluation

`fn eval1` in [`untyped/src/eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/untyped/src/eval.rs)

$$
\begin{align*}
\frac{}{(\lambda.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} [0 \mapsto\ \uparrow^{1} v_2]t_{12}} \quad &\text{(E-APPABS)} \\
\\
\frac{t_2 \rightarrow t_2'}{v_1\ t_2 \rightarrow v_1\ t_2'} \quad &\text{(E-APP2)} \\
\\
\frac{t_1 \rightarrow t_1'}{t_1\ t_2 \rightarrow t_1'\ t_2} \quad &\text{(E-APP1)} \\
\end{align*}
$$

## examples

```
$ cargo run --bin untyped
> (\\1 0)\0
input= (\\1 0) \0
   ->* \(\0) 0

>
```

de Bruijn index は何個外側の λ 抽象の変数かを表し、これは `(λx.λy.x y)λx.x -> λy.(λx.x)y` に対応。
値呼び(call by value)の評価規則で、最後の y は値でないため、これ以上簡約(E-APPABS 適用)できなくなっている。 5.1(p.42)
