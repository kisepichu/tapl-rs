# Simply typed lambda calculus($\lambda_{\rightarrow}$) + Bool

```
$ cargo run --bin simplebool
```

Figure 9-1(p.78), Figure 8-1(p.70), Figure 3-1(p.25)

単純型付き λ 計算は、型無し λ 計算を、型付けされた項のみ評価するようにし、関数の型 $\rightarrow$ を導入したもの。

## Syntax

`fn parse` in [`simplebool/src/parser.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/parser.rs), `enum Term` in [`simplebool/src/syntax/term.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/syntax/term.rs)

### Concrete syntax

```bnf
<term> ::= <app>
<app>  ::= <atom> <app> | <atom>
<atom> ::= <encl> | <abs> | <var> | <true> | <false> | <if>
<encl> ::= "(" <term> ")"
<abs> ::= "\:" <ty> "." <term>
<if> ::= "if" <term> "then" <term> "else" <term>
<var> ::= number
<true> ::= "true"
<false> ::= "false"

<ty> ::= <tyarr>
<tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
<tyarrsub> ::= "->" <ty>
<tyatom> ::= <tyencl> | <tybool>
<tyencl> ::= "(" <ty> ")"
<tybool> ::= "Bool"
```

### Abstract syntax

```math
\begin{align*}
t ::=&   &\quad (\text{terms}) \\
  \quad \mid\ &x &\quad (\text{variable}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2  &\quad (\text{abstraction}) \\
  \quad \mid\ &t_1\ t_2 &\quad (\text{application}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{constant true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{constant false}) \\
  \quad \mid\ &\mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 &\quad (\text{if}) \\
  \\
v ::=&   &\quad (\text{values}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2 &\quad (\text{abstraction value}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{false}) \\
  \\
T ::=&   &\quad (\text{types}) \\
  \quad \mid\ &T_1 \rightarrow T_2 &\quad (\text{arrow}) \\
  \quad \mid\ &\mathrm{Bool} &\quad (\text{boolean}) \\
  \\
\Gamma ::=&   &\quad (\text{contexts}) \\
  \quad \mid\ &\varnothing &\quad (\text{empty}) \\
  \quad \mid\ &\uparrow^1\Gamma, 0\mathord{:}T &\quad (\text{term variable binding}) \\
\end{align*}
```

### parsing

- `<var>`, `<abs>`, `<true>`, `<false>`, `<if>` が、それぞれ対応する term に変換される。
- `<app>` は、 `<atom>` の列が左結合で application に変換される。
- `<tybool>` は boolean に変換され、 `<tyarr>` は、 `<tyatom>` と "->" の列が右結合で arrow に変換される。

## evaluation

`fn eval1` in [`simplebool/src/eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/eval.rs)

```math
\begin{align*}
\frac{}{(\lambda\mathord{:}T.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} [0 \mapsto\ \uparrow^{1} v_2]t_{12}} \quad &\text{(E-APPABS)} \\
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

## typing

`fn type_of` in [`simplebool/src/typing.rs`](https://github.com/kisepichu/tapl-rs/blob/main/simplebool/src/typing.rs)

```math
\begin{align*}
\frac{x\mathord{:}T \in \Gamma}{\Gamma \vdash x \mathord{:} T} \quad &\text{(T-VAR)} \\
\\
\frac{\uparrow^1 \Gamma, 0\mathord{:}T_1 \vdash t_2 \mathord{:} T_2}{\Gamma \vdash \lambda\mathord{:}T_1.t_2 : T_1 \mathord\rightarrow T_2} \quad &\text{(T-ABS)} \\
\\
\frac{{\Gamma \vdash t_1 : T_{11} \mathord\rightarrow T_{12}} \quad {\Gamma \vdash t_2 \mathord{:} T_{21}}}{\Gamma \vdash t_1\ t_2 \mathord{:} T_{12} \rightarrow T_{21}} \quad &\text{(T-APP)} \\
\\
\frac{}{\Gamma \vdash \mathrm{true} : \mathrm{Bool}} \quad &\text{(T-TRUE)} \\
\\
\frac{}{\Gamma \vdash \mathrm{false} : \mathrm{Bool}} \quad &\text{(T-FALSE)} \\
\\
\frac{\Gamma \vdash t_1 \mathord{:} \mathrm{Bool} \quad \Gamma \vdash t_2 \mathord{:} T \quad \Gamma \vdash t_3 \mathord{:} T}{\Gamma \vdash \mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 : T} \quad &\text{(T-IF)} \\
\end{align*}
```

型チェック関数は、逆転補題を写すように実装する。 10.3(p.86), Lemma 9.3.1(p.79)

### examples

```
$ cargo run --bin simplebool
> (\:Bool.if 0 then \:Bool.\:Bool.1 else \:Bool.\:Bool.0) true
input: (\:Bool.if 0 then (\:Bool.\:Bool.1) else (\:Bool.\:Bool.0)) true: Bool->Bool->Bool
   ->* \:Bool.\:Bool.1

> (\:Bool->Bool->Bool.0 true false) \:Bool.\:Bool.0
input: (\:Bool->Bool->Bool.0 true false) \:Bool.\:Bool.0: Bool
   ->* false

>
```
