# Extensions of Simply Typed Lambda Calculus

WIP

```
$ cargo run --bin fullsimple
```

11.1(p.89)

## Syntax

`fn parse` in [`fullsimple/src/parser.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/parser.rs), `enum Term` in [`fullsimple/src/syntax/term.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/syntax/term.rs)

### Concrete syntax

```bnf
<term> ::= <seq>
<seq> ::= <app> ";" <seq> | <app>
<app>  ::= <atom> <app> | <atom>
<atom> ::= <encl> | <abs> | <var> | <unit> | <true> | <false> | <if>
<encl> ::= "(" <term> ")"
<abs> ::= "\:" <ty> "." <term>
<if> ::= "if" <term> "then" <term> "else" <term>
<var> ::= number
<unit> ::= "unit"
<true> ::= "true"
<false> ::= "false"

<ty> ::= <tyarr>
<tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
<tyarrsub> ::= "->" <ty>
<tyatom> ::= <tyencl> | <tyunit> | <tybool>
<tyencl> ::= "(" <ty> ")"
<tyunit> ::= "Unit"
<tybool> ::= "Bool"
```

### Abstract syntax

$$
\begin{align*}
t ::=&   &\quad (\text{terms}) \\
  \quad \mid\ &x &\quad (\text{variable}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2  &\quad (\text{abstraction}) \\
  \quad \mid\ &t_1\ t_2 &\quad (\text{application}) \\
  \quad \mid\ &\mathrm{unit} &\quad (\text{unit}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{constant true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{constant false}) \\
  \quad \mid\ &\mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 &\quad (\text{if}) \\
  \\

v ::=&   &\quad (\text{values}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2 &\quad (\text{abstraction value}) \\
  \quad \mid\ &\mathrm{unit} &\quad (\text{unit}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{false}) \\
  \\

T ::=&   &\quad (\text{types}) \\
  \quad \mid\ &T_1 \rightarrow T_2 &\quad (\text{arrow}) \\
  \quad \mid\ &\mathrm{Unit} &\quad (\text{unit}) \\
  \quad \mid\ &\mathrm{Bool} &\quad (\text{boolean}) \\
  \\

\Gamma ::=&   &\quad (\text{contexts}) \\
  \quad \mid\ &\varnothing &\quad (\text{empty}) \\
  \quad \mid\ &\Gamma, x\mathord{:}T &\quad (\text{term variable binding}) \\
\end{align*}
$$

### parsing

- `<var>`, `<abs>`, `<true>`, `<false>`, `<unit>`, `<if>` が、それぞれ対応する term に変換される。
- `<app>` は、 `<atom>` の列が左結合で application に変換される。
- `<tybool>` は boolean に変換され、 `<tyarr>` は、 `<tyatom>` と "->" の列が右結合で arrow に変換される。

糖衣構文(syntactic sugar, 派生形式, derived forms)

- `<seq>` は、 `<app>` と ";" の列が右結合で脱糖衣される。

$$
\begin{align*}
  \quad & \quad &\quad \text{(derived forms)} \\
t_1; t_2 \stackrel{\mathrm{def}}{=} &\ (\lambda\mathord{:}\mathrm{Unit}.\uparrow^1 t_2) t_1 &\ (\text{sequence})
\end{align*}
$$

補足: 本文では sequence は以下のようになっているが、シフトすることで名無し項で同じことをする実装にした。未証明

$$
\begin{align*}
t_1; t_2 \stackrel{\mathrm{def}}{=} &\ (\lambda x\mathord{:}\mathrm{Unit}.t_2) t_1  \\
  & \quad \text{where} \quad x \notin \mathrm{FV}(t_2) &\ (\text{sequence})
\end{align*}
$$

## evaluation

`fn eval1` in [`fullsimple/src/eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/eval.rs)

$$
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
$$

## typing

`fn type_of` in [`fullsimple/src/typing.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/typing.rs)

$$
\begin{align*}
\frac{x\mathord{:}T \in \Gamma}{\Gamma \vdash x \mathord{:} T} \quad &\text{(T-VAR)} \\
\\
\frac{\Gamma, x\mathord{:}T_1 \vdash t_2 \mathord{:} T_2}{\Gamma \vdash \lambda\mathord{:}T_1.t_2 : T_1 \rightarrow T_2} \quad &\text{(T-ABS)} \\
\\
\frac{{\Gamma \vdash t_1 \mathord{:} T_{11} \rightarrow T_{12}} \quad {\Gamma \vdash t_2 \mathord{:} T_{21}}}{\Gamma \vdash t_1\ t_2 \mathord{:} T_{12} \rightarrow T_{21}} \quad &\text{(T-APP)} \\
\\
\frac{}{\Gamma \vdash \mathrm{unit} : \mathrm{Unit}} \quad &\text{(T-UNIT)} \\
\\
\frac{}{\Gamma \vdash \mathrm{true} : \mathrm{Bool}} \quad &\text{(T-TRUE)} \\
\\
\frac{}{\Gamma \vdash \mathrm{false} : \mathrm{Bool}} \quad &\text{(T-FALSE)} \\
\\
\frac{\Gamma \vdash t_1 \mathord{:} \mathrm{Bool} \quad \Gamma \vdash t_2 \mathord{:} T \quad \Gamma \vdash t_3 \mathord{:} T}{\Gamma \vdash \mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 : T} \quad &\text{(T-IF)} \\
\end{align*}
$$

### examples
