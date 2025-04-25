## Simply typed lambda calculus($\lambda_{\rightarrow}$) + Bool

p.78, p.70, p.25

WIP

### Syntax

`fn parse` in [`simplebool/src/parser.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/parser.rs), `enum Term` in [`simplebool/src/syntax/term.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/syntax/term.rs)

#### Concrete syntax

```bnf
<term> ::= <app>
<app>  ::= <atom> <app> | <atom>
<atom> ::= <encl> | <abs> | <var> | <true> | <false> | <if>
<encl> ::= "(" <term> ")"
<abs> ::= "\:" <ty> "." <term>
<if> ::= "if" <term> "then" <term> "else" <term>
<ty> ::= "Bool" | <ty> "->" <ty> | <tyencl>
<tyencl> ::= "(" <ty> ")"
<var> ::= number
<true> ::= "true"
<false> ::= "false"
```

#### Abstract syntax

$$
\begin{align*}
t ::=&   \tag{terms} \\
  \mid\ &x \tag{variable} \\
  \mid\ &\lambda\mathord{:}T.t_2  \tag{abstraction} \\
  \mid\ &t_1\ t_2 \tag{application} \\
  \mid\ &\mathrm{true} \tag{constant true} \\
  \mid\ &\mathrm{false} \tag{constant false} \\
  \mid\ &\mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 \tag{if} \\
  \\
v ::=&   \tag{values} \\
  \mid\ &\lambda\mathord{:}T.t_2 \tag{abstraction value} \\
  \mid\ &\mathrm{true} \tag{true} \\
  \mid\ &\mathrm{false} \tag{false} \\
  \\
T ::=   \tag{types} \\
  \mid\ &T_1 \rightarrow T_2 \tag{function} \\
  \mid\ &\mathrm{Bool} \tag{boolean} \\
  \\
\Gamma ::=   \tag{contexts} \\
  \mid\ &\varnothing \tag{empty} \\
  \mid\ &\Gamma, x\mathord{:}T \tag{term variable binding} \\
\end{align*}
$$

- `<var>`, `<abs>`, `<true>`, `<false>`, `<if>` が、それぞれ対応する term に変換される。
- `<app>` は、 `<atom>` の列が左結合で application に変換される。

### evaluation

`fn eval1` in [`simplebool/src/eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/eval.rs)

$$
\begin{align*}
\frac{}{(\lambda:T.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} [`0 \mapsto\ \uparrow^{1} v_2`]t_{12}} \quad &\text{(E-APPABS)} \\
\\
\frac{t_2 \rightarrow t_2'}{v_1\ t_2 \rightarrow v_1\ t_2'} \quad &\text{(E-APP2)} \\
\\
\frac{t_1 \rightarrow t_1'}{t_1\ t_2 \rightarrow t_1'\ t_2} \quad &\text{(E-APP1)} \\
\\
\frac{}{\mathrm{if} \ \mathrm{true} \ \mathrm{then} \ t_2 \ \mathrm{else} \ t_3 \rightarrow t_2} \quad &\text{(E-IFTRUE)} \\
\\
\frac{}{\mathrm{if} \ \mathrm{false} \ \mathrm{then} \ t_2 \ \mathrm{else} \ t_3 \rightarrow t_3} \quad &\text{(E-IFFALSE)} \\
\\
\frac{t_1 \rightarrow t_1'}{\mathrm{if} \ t_1 \ \mathrm{then} \ t_2 \ \mathrm{else} \ t_3 \rightarrow \mathrm{if} \ t_1' \ \mathrm{then} \ t_2 \ \mathrm{else} \ t_3} \quad &\text{(E-IF)} \\
\end{align*}
$$

### examples
