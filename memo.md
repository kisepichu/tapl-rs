## untyped lambda calculus($\lambda$)

https://github.com/kisepichu/tapl-rs/tree/main/untyped

### syntax

wip in term.rs and parser.rs

```bnf
<term> ::= <app>
<app>  ::= <app> <atom> | <atom>
<atom> ::= <var> | <abs> | "(" <term> ")"
<abs> ::= "\" <term>
<var> ::= number
```

$$
\begin{align*}
t ::=& x \tag{term} \\
  \mid\ &\lambda.t \\
  \mid\ &t_1\ t_2 \\

v ::=& \lambda.t \tag{value} 
\end{align*}
$$

### evaluation

`fn eval1` in [untyped/src/eval.rs](https://github.com/kisepichu/tapl-rs/blob/main/untyped/src/eval.rs) 

$$
\begin{align*}
\frac{}{(\lambda.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} [0 \mapsto\ \uparrow^{1} v_2]t_{12}} \quad &\text{(E-APPABS)} \\
\\
\frac{t_2 \rightarrow t_2'}{v_1\ t_2 \rightarrow v_1\ t_2'} \quad &\text{(E-APP2)} \\
\\
\frac{t_1 \rightarrow t_1'}{t_1\ t_2 \rightarrow t_1'\ t_2} \quad &\text{(E-APP1)} \\
\end{align*}
$$

### examples

```
    (\\1 0)\0
->* \(\0)0
```

de Bruijn index は何個外側のλ抽象の変数かを表す。<br>
`(λx.λy.x y)λx.x ->* λy.(λx.x)y` に対応。
y は値でないため、簡約規則 E-APPABS を適用できない。簡約(E-APPABS)の引数を値に限る戦略は値呼び(call by value)。 p.42

