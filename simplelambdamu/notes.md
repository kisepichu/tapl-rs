# Simply typed lambda mu calculus

\+ an infinite collection of uninterpreted base types

```
$ cargo run --bin simplelambdamu
```

(この言語は本文にはありません)

単純型付き λμ 計算。

$T \to \bot$ を $\neg T$ と表す。

## Syntax

`fn parse` in [`simplelambdamu/src/parser.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplelambdamu/src/parser.rs), `enum Term` in [`simplelambdamu/src/syntax/term.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplelambdamu/src/syntax/term.rs)

### Concrete syntax

```bnf
<term> ::= <app>
<app>  ::= <atom> <app> | <atom>
<atom> ::= <encl> | <labs> | <mabs> | <var>
<encl> ::= "(" <term> ")"
<labs> ::= "\:" <ty> "." <term> | "\" string ":" <ty> "." <term>
<mabs> ::= "/:" <ty> "." <term> | "/" string ":" <ty> "." <term>
<var> ::= number | string

<ty> ::= <tyarr>
<tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
<tyarrsub> ::= "->" <ty>
<tyatom> ::= <tyencl> | <tybot> | <tyneg> | <tyvar>
<tyencl> ::= "(" <ty> ")"
<tybot> ::= "Bot"
<tyneg> ::= "!" <ty>
<tyvar> ::= string
```

### Abstract syntax

```math
\begin{align*}
t ::=&   &\quad (\text{terms}) \\
  \quad \mid\ &x &\quad (\text{variable}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2  &\quad (\lambda\text{ abstraction}) \\
  \quad \mid\ &\mu\mathord{:}\neg T.t_2 &\quad (\mu\text{ abstraction}) \\
  \quad \mid\ &t_1\ t_2 &\quad (\text{application}) \\
  \\
v ::=&   &\quad (\text{values}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2 &\quad (\text{abstraction value}) \\
  \\
T ::=&   &\quad (\text{types}) \\
  \quad \mid\ &T_1 \rightarrow T_2 &\quad (\text{arrow}) \\
  \quad \mid\ & \bot &\quad (\text{bottom}) \\
  \quad \mid\ &\mathtt{A} &\quad (\text{base type}) \\
  \\
\Gamma ::=&   &\quad (\text{contexts}) \\
  \quad \mid\ &\varnothing &\quad (\text{empty}) \\
  \quad \mid\ &\uparrow^1\Gamma, 0\mathord{:}T &\quad (\text{term variable binding}) \\
\end{align*}
```

### parsing

- `<var>`, `<abs>` が、それぞれ対応する term に変換される。
- `<app>` は、 `<atom>` の列が左結合で application に変換される。
- `<tyarr>` と `<tyvar>` は、それぞれ対応する type に変換される。

## evaluation

`fn eval1` in [`simplebool/src/eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/eval.rs)

```math
\begin{align*}
\frac{}{(\lambda\mathord{:}T.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} t_{12}[0 \mapsto\ \uparrow^{1} v_2]} \quad &\text{(E-BETA)} \\
\\
\frac{}{\beta \mu\mathord: \neg A. t \rightarrow\ \uparrow^{-1} t[0 \mapsto\ \uparrow^{1} \beta]} \quad &\text{(E-MUBETA)} \\
\\
\frac{0 \notin \mathrm{FV}(t)}{\lambda\mathord{:}T.t\ 0 \rightarrow\ \uparrow^{-1} t} \quad &\text{(E-ETA)} \\
\\
\frac{0 \notin \mathrm{FV}(t)}{\mu\mathord: \neg A. 0\ t \rightarrow\ \uparrow^{-1} t} \quad &\text{(E-MUETA)} \\
\\
\frac{}{(\mu\mathord:\neg(A\mathord\to B).t_1)t_2 \to \mu\mathord:\neg B.t_1^*} \quad &\text{(E-STR)} \\
\text{where\ } t^* := t[\alpha u \mapsto \alpha' (u^*\ t_2)] \\
\\
\frac{}{v_1 \mu\mathord: \neg A. t_2 \to \mu\mathord: \neg B. t_2^\star} \quad &\text{(E-STR-V)} \\
\text{where\ } t_2^\star := t_2[\alpha u \mapsto \alpha' (v_1 u^\star)] \\
\\
\frac{t_2 \rightarrow t_2'}{v_1\ t_2 \rightarrow v_1\ t_2'} \quad &\text{(E-APP2)} \\
\\
\frac{t_1 \rightarrow t_1'}{t_1\ t_2 \rightarrow t_1'\ t_2} \quad &\text{(E-APP1)} \\
\\
\end{align*}
```

## typing

`fn type_of` in [`simplebool/src/typing.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/typing.rs)

```math
\begin{align*}
\frac{x\mathord{:}T \in \Gamma}{\Gamma \vdash x \mathord{:} T} \quad &\text{(T-VAR)} \\
\\
\frac{\uparrow^1 \Gamma, 0\mathord{:}T_1 \vdash t_2 \mathord{:} T_2}{\Gamma \vdash \lambda\mathord{:}T_1.t_2 : T_1 \mathord\rightarrow T_2} \quad &\text{(T-LABS)} \\
\\
\frac{\uparrow^1 \Gamma, 0\mathord{:}\neg T_1 \vdash t_2 \mathord{:} \bot}{\Gamma \vdash \mu\mathord: \neg T_1.t_2 : T_1} \quad &\text{(T-MABS)} \\
\\
\frac{{\Gamma \vdash t_1 : T_{11} \mathord\rightarrow T_{12}} \quad {\Gamma \vdash t_2 \mathord{:} T_{21}}}{\Gamma \vdash t_1\ t_2 \mathord{:} T_{12} \rightarrow T_{21}} \quad &\text{(T-APP)} \\
\\
\end{align*}
```

型チェック関数は、逆転補題を写すように実装する。 10.3(p.86), Lemma 9.3.1(p.79)

### examples

```
$ cargo run --bin simplebool
> \:P.\:Q.1
input= \:P.\:Q.1: P->Q->P
   ->* \:P.\:Q.1

> (\:A->A.0) \:A.0
input= (\:A->A.0) \:A.0: A->A
   ->* \:A.0

>
```
