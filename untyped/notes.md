## Untyped lambda calculus ($\lambda$)

p.54, p.61

### Syntax

`fn parse` in [`untyped/src/parser.rs`](https://github.com/kisepichu/tapl-rs/blob/main/untyped/src/parser.rs), `enum Term` in [`untyped/src/syntax.rs`](https://github.com/kisepichu/tapl-rs/blob/main/untyped/src/syntax.rs)

#### Concrete syntax

```bnf
<term> ::= <app>
<app>  ::= <atom> <app> | <atom>
<atom> ::= <encl> | <abs> | <var>
<encl> ::= "(" <term> ")"
<abs> ::= "\" <term>
<var> ::= number
```

#### Abstract syntax

$$
\begin{align*}
t ::=&   \tag{terms} \\
  \mid\ &x \tag{variable} \\
  \mid\ &\lambda.t \tag{abstraction} \\
  \mid\ &t_1\ t_2 \tag{application} \\
  \\
v ::=&   \tag{values} \\
  \mid\ &\lambda.t \tag{abstraction value} \\
\end{align*}
$$

- `<app>` は、 `<atom>` の列が左結合で application に変換される。
- `<abs>` は、 abstraction に変換される。 
- `<var>` は、 variable に変換される。

### evaluation

`fn eval1` in [`untyped/src/eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/untyped/src/eval.rs) 

$$
\begin{align*}
\frac{}{(\lambda.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} [`0 \mapsto\ \uparrow^{1} v_2`]t_{12}} \quad &\text{(E-APPABS)} \\
\\
\frac{t_2 \rightarrow t_2'}{v_1\ t_2 \rightarrow v_1\ t_2'} \quad &\text{(E-APP2)} \\
\\
\frac{t_1 \rightarrow t_1'}{t_1\ t_2 \rightarrow t_1'\ t_2} \quad &\text{(E-APP1)} \\
\end{align*}
$$

### examples

```
    (\\1 0)\0
->  \(\0)0
```

de Bruijn index は何個外側のλ抽象の変数かを表し、これは `(λx.λy.x y)λx.x -> λy.(λx.x)y` に対応。
値呼び(call by value)の評価規則で、最後の y は値でないため、これ以上簡約(E-APPABS 適用)できなくなっている。 p.42

