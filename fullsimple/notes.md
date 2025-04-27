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
<atom> ::= <encl> | <abs> | <let> | <if> | <var> | <unit> | <true> | <false>
<let> ::= "let" string "=" <term> "in" <term>
<encl> ::= "(" <term> ")"
<abs> ::= "\:" <ty> "." <term>
<if> ::= "if" <term> "then" <term> "else" <term>
<var> ::= number | string
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

```math
\begin{align*}
t ::=&   &\quad (\text{terms}) \\
  \quad \mid\ &x &\quad (\text{variable}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2  &\quad (\text{abstraction}) \\
  \quad \mid\ &t_1\ t_2 &\quad (\text{application}) \\
  \quad \mid\ &\mathrm{unit} &\quad (\text{constant unit}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{constant true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{constant false}) \\
  \quad \mid\ &\mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 &\quad (\text{if}) \\
  \quad \mid\ &\mathrm{let}\ v_1\ \mathrm{in}\ t_2 &\quad (\text{let}) \\
  \\

v ::=&   &\quad (\text{values}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2 &\quad (\text{abstraction value}) \\
  \quad \mid\ &\mathrm{unit} &\quad (\text{unit}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{false}) \\
  \\

T ::=&   &\quad (\text{types}) \\
  \quad \mid\ &T_1 \rightarrow T_2 &\quad (\text{arrow}) \\
  \quad \mid\ &\mathrm{Unit} &\quad (\text{unit type}) \\
  \quad \mid\ &\mathrm{Bool} &\quad (\text{boolean}) \\
  \\

\Gamma ::=&   &\quad (\text{contexts}) \\
  \quad \mid\ &\varnothing &\quad (\text{empty}) \\
  \quad \mid\ &\Gamma, x\mathord{:}T &\quad (\text{term variable binding}) \\
\end{align*}
```

- 注意: contexts は実装上は以下のようになっている。ただし、 $\uparrow^1 \Gamma$ は $\Gamma$ の変数を全て 1 つシフトしたものとする。これにより、評価規則や(T-VAR 以外の)型付け規則中の $x$ を全て $0$ に固定できる。

```math
\begin{align*}
\Gamma ::=&   &\quad (\text{contexts}) \\
  \quad \mid\ &\varnothing &\quad (\text{empty}) \\
  \quad \mid\ &\uparrow^1 \Gamma, 0\mathord{:}T &\quad (\text{term variable binding}) \\
\end{align*}
```

### Derived forms

```math
\begin{align*}
  \quad & \quad &\text{(derived forms)} \\
t_1; t_2 \stackrel{\mathrm{def}}{=}\ & (\lambda\mathord{:}\mathrm{Unit}.\uparrow^1 t_2) t_1 \quad & (\text{sequence}) \\
\\
\mathrm{let}\ x=t_1\ \mathrm{in}\ t_2 \stackrel{\mathrm{def}}{=}\ & \mathrm{let}\ t_1\ \mathrm{in}\ t_2 \quad & (\text{let'})
\end{align*}
```

補足:

- 本文では sequence は以下のようになっているが、シフトすることで名無し項で同じことをする実装にした。未証明

```math
\begin{align*}
t_1; t_2 \stackrel{\mathrm{def}}{=}\ & (\lambda x\mathord{:}\mathrm{Unit}.t_2) t_1  \\
   \quad & \text{where} \quad x \notin \mathrm{FV}(t_2) \quad & (\text{sequence})
\end{align*}
```

- let は abs を使った糖衣構文にはしない。11.5(p.95)

### parsing

- `<var>`, `<abs>`, `<true>`, `<false>`, `<unit>`, `<if>` が、それぞれ対応する term に変換される。
- `<app>` は、 `<atom>` の列が左結合で application に変換される。
- `<tybool>` は boolean に変換され、 `<tyarr>` は、 `<tyatom>` と "->" の列が右結合で arrow に変換される。
- 糖衣構文(syntactic sugar, derived forms)を含む。
  - `<seq>` は、`<app>` と ";" の列が左結合で sequence に変換され、脱糖衣される。
  - `<let>` は、let' に変換され、脱糖衣される。

## evaluation

`fn eval1` in [`fullsimple/src/eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/eval.rs)

```math
\begin{align*}
\frac{}{(\lambda\mathord{:}T.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} [x \mapsto\ \uparrow^{1} v_2]t_{12}} \quad &\text{(E-APPABS)} \\
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
\\
\frac{}{\mathrm{let}\ v_1\ \mathrm{in}\ t_2 \rightarrow \uparrow^{-1}[x\mapsto v_1]\uparrow^{1}t_2} \quad &(\text{E-LETV}) \\
\\
\frac{t_1\rightarrow t_1'}{\mathrm{let}\ t_1\ \mathrm{in}\ t_2 \rightarrow \mathrm{let}\ t_1'\ \mathrm{in}\ t_2} \quad &(\text{E-LET}) \\
\end{align*}
```

## typing

`fn type_of` in [`fullsimple/src/typing.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/typing.rs)

```math
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
\\
\frac{\Gamma \vdash t_1\mathord{:}T_1 \quad \Gamma, x\mathord{:}T_1 \vdash t_2\mathord{:}T_2}{\Gamma \vdash \mathrm{let}\ t_1\ \mathrm{in}\ t_2: T_2} \quad &(\text{T-LET}) \\
\end{align*}
```

### examples
