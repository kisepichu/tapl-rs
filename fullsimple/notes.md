# Extensions of simply typed lambda calculus

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
<app>  ::= <postfix> <app> | <postfix>
<postfix> ::= <atom> <projection> | <atom>
<projection> ::= "." <label>
<atom> ::= <encl> | <abs> | <let> | <if> | <var> | <unit> | <true> | <false> | <record>
<encl> ::= "(" <term> ")"
<abs> ::= "\:" <ty> "." <term> | "\" <bound> ":" <ty> "." <term>
<let> ::= "let" <bound> "=" <term> "in" <term>
<if> ::= "if" <term> "then" <term> "else" <term>
<tagging> ::= "<" <field> ">"
<record> ::= "{" <inner> "}"
<inner> ::= <fieldseq> | <notrailing>
<notrailing> ::= <fieldseq> <field>
<fieldseq> ::= <field> "," <fieldseq> | null
<field> ::= <label> "=" <term> | <term>
<label> ::= <ident>
<bound> ::= <ident>
<var> ::= number | <ident>
<unit> ::= "unit"
<true> ::= "true"
<false> ::= "false"
<ident> ::= <ident> (alphabet|digit) | alphabet

<type> ::= <tyarr>
<tyarr> ::= <tyarr> <tyarrsub> | <tyatom>
<tyarrsub> ::= "->" <ty>
<tyatom> ::= <tyencl> | <tyunit> | <tybool> | <tyrecord>
<tyencl> ::= "(" <ty> ")"
<tytagging>::= "<" <tyinner> ">"
<tyrecord> ::= "{" <tyinner> "}"
<tyinner> ::= <tyfieldseq> | <tynotrailing>
<tynotrailing> ::= <tyfieldseq> <tyfield>
<tyfieldseq> ::= <tyfield> "," <tyfieldseq> | null
<tyfield> ::= <label> ":" <ty> | <ty>
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
  \quad \mid\ &\{l_i\mathord:T_i,^{i\in1..n}\} &\quad (\text{record}) \\
  \quad \mid\ &t.l &\quad (\text{projection}) \\
  \quad \mid\ &\mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 &\quad (\text{if}) \\
  \quad \mid\ &\mathrm{let}\ v_1\ \mathrm{in}\ t_2 &\quad (\text{let}) \\
  \quad \mid\ & \langle l\mathord=t\rangle\ \mathrm{as}\ T &\quad (\text{tagging}) \\
  \quad \mid\ & \mathrm{case}\ t\ \mathrm{of}\ \langle l_i\mathord=v_i\rangle \Rightarrow t_i^{i\in 1..n} &\quad (\text{case}) \\
  \\
  \\

% p ::=&   &\quad (\text{patterns}) \\
%   \quad \mid\ & x &\quad (\text{variable pattern}) \\
%   \quad \mid\ & \{l_i\mathord=p_i^{i\in1..n}\} &\quad (\text{record pattern}) \\
% \\

v ::=&   &\quad (\text{values}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2 &\quad (\text{abstraction value}) \\
  \quad \mid\ &\mathrm{unit} &\quad (\text{unit}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{false}) \\
  \quad \mid\ &\{l_i\mathord=v_i,^{i\in1..n}\} &\quad (\text{record value}) \\
  \quad \mid\ & \langle l\mathord=v\rangle\ \mathrm{as}\ T &\quad (\text{tagging value}) \\
  \\

T ::=&   &\quad (\text{types}) \\
  \quad \mid\ &T_1 \rightarrow T_2 &\quad (\text{arrow}) \\
  \quad \mid\ &\mathrm{Unit} &\quad (\text{unit type}) \\
  \quad \mid\ &\mathrm{Bool} &\quad (\text{boolean}) \\
  \quad \mid\ &\{l_i\mathord:T_i,^{i\in1..n}\} &\quad (\text{record type}) \\
  \quad \mid\ & \langle l_i\mathord:T_i,^{i \in 1..n}\rangle &\quad (\text{variant type}) \\
  \\

\Gamma ::=&   &\quad (\text{contexts}) \\
  \quad \mid\ &\varnothing &\quad (\text{empty}) \\
  \quad \mid\ &\uparrow^1 \Gamma, 0\mathord{:}T &\quad (\text{term variable binding}) \\
\end{align*}
```

- contexts の定義を本文と変えている。 $\uparrow^1 \Gamma$ は $\Gamma$ の変数を全て 1 つシフトしたものとする(独自の記法、このような表し方の情報求む)。本文では名無し項を使うのを(少なくとも抽象構文上の想定では)辞めているが、こうすることで評価規則や(T-VAR 以外の)型付け規則中の $x$ を消去できて、名無し項のまま抽象構文で表せて、そのまま実装できる。しかしこの先に出てくる拡張等でこの方法で形式化できなくなるということかもしれないので様子見。

### Derived forms

```math
\begin{align*}
  \quad & \quad &\text{(derived forms)} \\
t_1; t_2 \stackrel{\mathrm{def}}{=}\ & (\lambda\mathord{:}\mathrm{Unit}.\uparrow^1 t_2) t_1 \quad & (\text{sequence}) \\
\\
\mathrm{let}\ x=t_1\ \mathrm{in}\ t_2 \stackrel{\mathrm{def}}{=}\ & \mathrm{let}\ t_1\ \mathrm{in}\ [x\mapsto 0]t_2 \quad & (\text{let'}) \\
\\
\lambda x:T.t_2 \stackrel{\mathrm{def}}{=}\ & \lambda T.[x\mapsto 0]t_2 \quad & (\text{abs'})
\end{align*}
```

補足:

- それぞれ名無し項用に本文と定義を変えている。同じになることは未証明
- let は abs を使った糖衣構文にはしない。11.5(p.95)
- 名前 $x$ を置換する $[x\rightarrow 0]$ の実装は `subst_name` in [`fullsimple/src/parser.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/parser.rs)

### parsing

- `<var>`, `<true>`, `<false>`, `<unit>`, `<if>`, `<record>` が、それぞれ対応する term に変換される。
  - `<record>` は、ラベルがない形式(tuple) も parse し、 0 から自動でラベルを付けて record に変換する。
- `<app>` は、 `<atom>` の列が左結合で application に変換される。
- `<tybool>` は boolean に変換され、 `<tyarr>` は、 `<tyatom>` と "->" の列が右結合で arrow に変換される。
- 糖衣構文(syntactic sugar, derived forms)を含む。
  - `<seq>` は、`<app>` と ";" の列が左結合で sequence に変換され、脱糖衣される。
  - `<let>` は、let' に変換され、脱糖衣される。
  - `<abs>` は abs に変換されるか、 abs' に変換され脱糖衣される。

## evaluation

`fn eval1` in [`fullsimple/src/eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/eval.rs)

```math
\begin{align*}
\frac{}{(\lambda\mathord{:}T.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} ([0 \mapsto\ \uparrow^{1} v_2]t_{12})} \quad &\text{(E-APPABS)} \\
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
\frac{}{\mathrm{let}\ v_1\ \mathrm{in}\ t_2 \rightarrow \uparrow^{-1}[0\mapsto v_1]\uparrow^{1}t_2} \quad &(\text{E-LETV}) \\
\\
\frac{t_1\rightarrow t_1'}{\mathrm{let}\ t_1\ \mathrm{in}\ t_2 \rightarrow \mathrm{let}\ t_1'\ \mathrm{in}\ t_2} \quad &(\text{E-LET}) \\
\\
\frac{}{\{l_i\mathord=v_i,^{i\in 1..n}\}.l_j \rightarrow v_j} \quad &(\text{E-PROJRCD}) \\
\\
\frac{t_1\rightarrow t_1'}{t_1.l \rightarrow t_1'.l} \quad &(\text{E-PROJ}) \\
\\
\frac{t_j\rightarrow t_j'}{\{l_i\mathord=v_i^{i\in 1..j-1}, l_j\mathord=t_j, l_k\mathord=t_k^{k\in j+1..n}\} \\ \rightarrow \{l_i\mathord=v_i^{i\in 1..j-1}, l_j\mathord=t_j', l_k\mathord=t_k^{k\in j+1..n}\}} \quad &(\text{E-RCD}) \\
\end{align*}
```

## typing

`fn type_of` in [`fullsimple/src/typing.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/typing.rs)

```math
\begin{align*}
\frac{x\mathord{:}T \in \Gamma}{\Gamma \vdash x \mathord{:} T} \quad &\text{(T-VAR)} \\
\\
\frac{\uparrow^1\Gamma, 0\mathord{:}T_1 \vdash t_2 \mathord{:} T_2}{\Gamma \vdash \lambda\mathord{:}T_1.t_2 : T_1 \rightarrow T_2} \quad &\text{(T-ABS)} \\
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
\frac{\Gamma \vdash t_1\mathord{:}T_1 \quad \uparrow^1\Gamma, 0\mathord{:}T_1 \vdash t_2\mathord{:}T_2}{\Gamma \vdash \mathrm{let}\ t_1\ \mathrm{in}\ t_2: T_2} \quad &(\text{T-LET}) \\
\\
\frac{\forall i, \Gamma \vdash t_i\mathord:T_i}{\Gamma \vdash \{l_i\mathord=t_i,^{i\in 1..n}\}: \{l_i\mathord=T_i,^{i\in 1..n}\}} \quad &\text{(T-RCD)} \\
\\
\frac{\Gamma \vdash t_1 \mathord: \{l_i\mathord=T_i,^{i\in 1..n}\}}{\Gamma \vdash t_1.l_j : T_j} \quad &\text{(T-PROJ)} \\
\\
\end{align*}
```

- Abstract syntax 注意参照。

### examples
