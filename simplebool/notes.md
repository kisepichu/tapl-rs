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
<ty> ::= <tyatom> <tyarr> | <tyatom>
<tyatom> ::= <tyencl> | <tybool>
<tyarr> ::= "->" <ty>
<tyencl> ::= "(" <ty> ")"
<tybool> ::= "Bool"
<var> ::= number
<true> ::= "true"
<false> ::= "false"
```

#### Abstract syntax

```math
\begin{align*}
t ::=&   &\quad (\text{terms}) \\
  \mid\ &x &\quad (\text{variable}) \\
  \mid\ &\lambda\mathord{:}T.t_2  &\quad (\text{abstraction}) \\
  \mid\ &t_1\ t_2 &\quad (\text{application}) \\
  \mid\ &\mathrm{true} &\quad (\text{constant true}) \\
  \mid\ &\mathrm{false} &\quad (\text{constant false}) \\
  \mid\ &\mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 &\quad (\text{if}) \\
  \\
v ::=&   &\quad (\text{values}) \\
  \mid\ &\lambda\mathord{:}T.t_2 &\quad (\text{abstraction value}) \\
  \mid\ &\mathrm{true} &\quad (\text{true}) \\
  \mid\ &\mathrm{false} &\quad (\text{false}) \\
  \\
T ::=   &\quad (\text{types}) \\
  \mid\ &T_1 \rightarrow T_2 &\quad (\text{function}) \\
  \mid\ &\mathrm{Bool} &\quad (\text{boolean}) \\
  \\
\Gamma ::=   &\quad (\text{contexts}) \\
  \mid\ &\varnothing &\quad (\text{empty}) \\
  \mid\ &\Gamma, x\mathord{:}T &\quad (\text{term variable binding}) \\
\end{align*}
```

- `<var>`, `<abs>`, `<true>`, `<false>`, `<if>` が、それぞれ対応する term に変換される。
- `<app>` は、 `<atom>` の列が左結合で application に変換される。

### evaluation

`fn eval1` in [`simplebool/src/eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/eval.rs)

```math
\begin{align*}
\frac{}{(\lambda\mathord{:}T.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} [`0 \mapsto\ \uparrow^{1} v_2`]t_{12}} \quad &\text{(E-APPABS)} \\
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
```

### typing

```math
\begin{align*}
\frac{x\mathord{:}T \in \Gamma}{\Gamma \vdash x \mathord{:} T} \quad &\text{(T-VAR)} \\
\\
\frac{\Gamma, x\mathord{:}T_1 \vdash t_2 \mathord{:} T_2}{\Gamma \vdash \lambda\mathord{:}T_1.t_2 : T_1 \rightarrow T_2} \quad &\text{(T-ABS)} \\
\\
\frac{{\Gamma \vdash t_1 \mathord{:} T_{11} \rightarrow T_{12}} \quad {\Gamma \vdash t_2 \mathord{:} T_{21}}}{\Gamma \vdash t_1\ t_2 \mathord{:} T_{12} \rightarrow T_{21}} \quad &\text{(T-APP)} \\
\\
\frac{}{\Gamma \vdash \mathrm{true} : \mathrm{Bool}} \quad &\text{(T-TRUE)} \\
\\
\frac{}{\Gamma \vdash \mathrm{false} : \mathrm{Bool}} \quad &\text{(T-FALSE)} \\
\\
\frac{\Gamma \vdash t_1 \mathord{:} \mathrm{Bool} \quad \Gamma \vdash t_2 \mathord{:} T \quad \Gamma \vdash t_3 \mathord{:} T}{\Gamma \vdash \mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 : T} \quad &\text{(T-IF)} \\
\end{align*}
```

### examples
