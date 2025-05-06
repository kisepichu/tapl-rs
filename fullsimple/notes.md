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
<atom> ::= <encl> | <abs> | <if> | <let> | <case> | <var> | <unit> | <true> | <false> | <record> | <tagging>
<encl> ::= "(" <term> ")"
<abs> ::= "\:" <ty> "." <term> | "\" <bound> ":" <ty> "." <term>
<if> ::= "if" <term> "then" <term> "else" <term>
<let> ::= "let" <bound> "=" <term> "in" <term>
<case> ::= "case" <term> "of" <branches>
<branches> ::= "|" <pat> "=>" <term> <branches> | null
<record> ::= "{" <inner> "}"
<inner> ::= <fieldseq> | <notrailing>
<notrailing> ::= <fieldseq> <field>
<fieldseq> ::= <field> "," <fieldseq> | null
<field> ::= <label> "=" <term> | <term>
<tagging> ::= <ty> ":::" <label>
<label> ::= <ident>
<bound> ::= <ident>
<var> ::= number | <ident>
<unit> ::= "unit"
<true> ::= "true"
<false> ::= "false"
<ident> ::= <ident> (alphabet|digit) | alphabet

<pat> ::= <bound> ":" <ty> | <patrecord> | <pattagging>
<patrecord> ::= "{" <patinner> "}"
<patinner> ::= <patfieldseq> | <patnotrailing>
<patnotrailing> ::= <patfieldseq> <patfield>
<patfieldseq> ::= <patfield> "," <patfieldseq> | null
<patfield> ::= <label> ":" <pat> | <pat>
<pattagging> ::= <ty> ":::" <label> | <pattagging> <pat>

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
  \quad \mid\ &\mathrm{p}\lambda p.t_2  &\quad (\text{pattern abstraction}) \\
  \quad \mid\ &t_1\ t_2 &\quad (\text{application}) \\
  \quad \mid\ &\mathrm{unit} &\quad (\text{constant unit}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{constant true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{constant false}) \\
  \quad \mid\ &\{l_i\mathord:T_i,^{i\in1..n}\} &\quad (\text{record}) \\
  \quad \mid\ &t.l &\quad (\text{projection}) \\
  \quad \mid\ & T\mathord{:::}l &\quad (\text{tagging}) \\
  \quad \mid\ &\mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 &\quad (\text{if}) \\
  \quad \mid\ &\mathrm{let}\ t_1\ \mathrm{in}\ t_2 &\quad (\text{let}) \\
  \quad \mid\ &\mathrm{plet}\ p = t_1\ \mathrm{in}\ t_2 &\quad (\text{pattern let}) \\
  \quad \mid\ & \mathrm{case}\ t\ \mathrm{of} \ p_i \Rightarrow t_i \ ^{i\in 1..n} &\quad (\text{case}) \\
  \\
  \\

v ::=&   &\quad (\text{values}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2 &\quad (\text{abstraction value}) \\
  \quad \mid\ &\mathrm{unit} &\quad (\text{unit}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{false}) \\
  \quad \mid\ &\{l_i\mathord=v_i,^{i\in1..n}\} &\quad (\text{record value}) \\
  \quad \mid\ & v_\mathrm{tag} &\quad (\text{tagging value}) \\
v_{\mathrm{tag}} ::=& &\quad (\text{tagging value}) \\
  \quad \mid\ &T\mathord{:::}l &\quad (\text{tagging}) \\
  \quad \mid\ &v_\mathrm{tag}\ v &\quad (\text{tagging application}) \\
  \\


p ::=&   &\quad (\text{patterns}) \\
  \quad \mid\ & x\mathord{:}T &\quad (\text{variable pattern}) \\
  \quad \mid\ & \{l_i\mathord=p_i^{i\in1..n}\} &\quad (\text{record pattern}) \\
  \quad \mid\ & p_\mathrm{tag} &\quad (\text{tagging pattern}) \\
p_{\mathrm{tag}} ::=& &\quad (\text{tagging pattern}) \\
  \quad \mid\ &T\mathord{:::}l &\quad (\text{tagging}) \\
  \quad \mid\ &p_\mathrm{tag}\ p &\quad (\text{tagging application}) \\
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
- この段階では、 record のフィールドの順序の違いを区別する。11.8(p.99)

### Derived forms

```math
\begin{align*}
  \quad & \quad & \text{(derived forms)} \\
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

### Parsing

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
\frac{}{(\lambda\mathord{:}T.t_{12})\ v_2 \rightarrow\ \uparrow^{-1} ([0 \mapsto\ \uparrow^{1} v_2]t_{12})} \quad & \text{(E-APPABS)} \\
\\
\frac{t_2 \rightarrow t_2'}{v_1\ t_2 \rightarrow v_1\ t_2'} \quad & \text{(E-APP2)} \\
\\
\frac{t_1 \rightarrow t_1'}{t_1\ t_2 \rightarrow t_1'\ t_2} \quad & \text{(E-APP1)} \\
\\
\frac{}{\mathrm{if} \ \mathrm{true} \ \mathrm{then} \ t_2 \ \mathrm{else} \ t_3 \rightarrow t_2} \quad & \text{(E-IFTRUE)} \\
\\
\frac{}{\mathrm{if} \ \mathrm{false} \ \mathrm{then} \ t_2 \ \mathrm{else} \ t_3 \rightarrow t_3} \quad & \text{(E-IFFALSE)} \\
\\
\frac{t_1 \rightarrow t_1'}{\mathrm{if} \ t_1 \ \mathrm{then} \ t_2 \ \mathrm{else} \ t_3 \rightarrow \mathrm{if} \ t_1' \ \mathrm{then} \ t_2 \ \mathrm{else} \ t_3} \quad & \text{(E-IF)} \\
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
\\
\frac{}{\mathrm{case}\ v_{\mathrm{tag}j}\ \mathrm{of}\ p_{\mathrm{tag}i} \Rightarrow t_i\ ^{i\in 1..n} \rightarrow \mathit{match}(p_{\mathrm{tag}j}, v_{\mathrm{tag}j})t_j} \quad &(\text{E-CASEVARIANT}) \\
\begin{align*}
\text{where }v_{\mathrm{tag}j} &:= T\mathord{:::}l_j\ v_1\ v_2\ ...\ v_n &(n \ge 0), \\
p_{\mathrm{tag}i} &:= T\mathord{:::}l_i\ p_{i1}\ p_{i2}\ ...\ p_{in_i} &(n_i \ge 0). \\
\end{align*}
\\
\frac{t \rightarrow t'}{\mathrm{case}\ t\ \mathrm{of}\ p_i \Rightarrow t_i\ ^{i\in 1..n} \rightarrow
\mathrm{case}\ t'\ \mathrm{of}\ p_i \Rightarrow t_i\ ^{i\in 1..n}} \quad &(\text{E-CASE}) \\
\end{align*}
```

### Pattern matching

```math
\begin{align*}
\mathit{match}(x\mathord:T, v) &\stackrel{\mathrm{def}}{=} \mathrm{let}\ x=v\ \mathrm{in} \quad & (\text{M-VAR}) \\
\\
\mathit{match}(\{l_i\mathord=p_i\}, \{l_i\mathord=v_i\}) &\stackrel{\mathrm{def}}{=} \mathrm{let}\ l_i = v_i\ \mathrm{in}\ ^{i\in n} \quad & (\text{M-RCD}) \\
\\
\mathit{match}(T\mathord{:::}l, T\mathord{:::}l) &\stackrel{\mathrm{def}}{=} () \quad & (\text{M-TAG}) \\
\\
\mathit{match}(p_{\mathrm{tag}}\ p, v_{\mathrm{tag}}\ v) &\stackrel{\mathrm{def}}{=} \mathit{match}(p_{\mathrm{tag}}, v_{\mathrm{tag}}) \mathrm{plet}\ p=v\ \mathrm{in} \quad & (\text{M-TAGAPP}) \\
\end{align*}
```

## typing

`fn type_of` in [`fullsimple/src/typing.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/typing.rs)

```math
\begin{align*}
\frac{x\mathord{:}T \in \Gamma}{\Gamma \vdash x \mathord{:} T} \quad & \text{(T-VAR)} \\
\\
\frac{\uparrow^1\Gamma, 0\mathord{:}T_1 \vdash t_2 \mathord{:} T_2}{\Gamma \vdash \lambda\mathord{:}T_1.t_2 : T_1 \rightarrow T_2} \quad & \text{(T-ABS)} \\
\\
\frac{{\Gamma \vdash t_1 \mathord{:} T_{11} \rightarrow T_{12}} \quad {\Gamma \vdash t_2 \mathord{:} T_{21}}}{\Gamma \vdash t_1\ t_2 \mathord{:} T_{12} \rightarrow T_{21}} \quad & \text{(T-APP)} \\
\\
\frac{}{\Gamma \vdash \mathrm{unit} : \mathrm{Unit}} \quad & \text{(T-UNIT)} \\
\\
\frac{}{\Gamma \vdash \mathrm{true} : \mathrm{Bool}} \quad & \text{(T-TRUE)} \\
\\
\frac{}{\Gamma \vdash \mathrm{false} : \mathrm{Bool}} \quad & \text{(T-FALSE)} \\
\\
\frac{\Gamma \vdash t_1 \mathord{:} \mathrm{Bool} \quad \Gamma \vdash t_2 \mathord{:} T \quad \Gamma \vdash t_3 \mathord{:} T}{\Gamma \vdash \mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 : T} \quad & \text{(T-IF)} \\
\\
\frac{\Gamma \vdash t_1\mathord{:}T_1 \quad \uparrow^1\Gamma, 0\mathord{:}T_1 \vdash t_2\mathord{:}T_2}{\Gamma \vdash \mathrm{let}\ t_1\ \mathrm{in}\ t_2: T_2} \quad &(\text{T-LET}) \\
\\
\frac{\forall i, \Gamma \vdash t_i\mathord:T_i}{\Gamma \vdash \{l_i\mathord=t_i,^{i\in 1..n}\}: \{l_i\mathord=T_i,^{i\in 1..n}\}} \quad & \text{(T-RCD)} \\
\\
\frac{\Gamma \vdash t_1 \mathord: \{l_i\mathord=T_i,^{i\in 1..n}\}}{\Gamma \vdash t_1.l_j : T_j} \quad & \text{(T-PROJ)} \\
\\
\frac{}{\Gamma \vdash \langle l_i\mathord:T_i,^{i \in 1..n}\rangle\mathord{:::}l_j : T_j} \quad & \text{(T-VARIANT)} \\
\\

\end{align*}
```

- Abstract syntax 注意参照。

## Pattern typing

パターン $p$ の型付けは $p:T\mathord\Rightarrow\varDelta$ という形をしている。 $T$ はマッチする項の型で、 $\varDelta$ は生成する文脈( $\Gamma$ と同じ型[^1])。
[^1]: 一般的な語の。

```math
\begin{align*}
\frac{}{\Gamma \vdash x\mathord{:}T :  T \mathord\Rightarrow x\mathord: T} \quad & \text{(PT-VAR)} \\
\\
\frac{\{l_i\ ^{i\in 1..n}\}\subseteq \{k_i\ ^{i\in 1..m}\} \quad \forall^{i\in 1..n}\exists^{i\in 1..n}\ l_i=k_j\ \land\ \vdash p_i : T_j \mathord\Rightarrow \varDelta_i}{\vdash \{l_i\mathord=p_i\ ^{i \in 1..n} \}:\{k_j\mathord:T_j\ ^{j \in 1..m} \}\mathord\Rightarrow \varDelta_1, \varDelta_2, \dots,\varDelta_n} \quad & \text{(PT-RCD')} \\
\\
\frac{}{\vdash T\mathord{:::}l : T \mathord\Rightarrow \varnothing} \quad & \text{(PT-TAG)} \\
\\
\frac{\vdash p_1 : T_{11}\mathord\rightarrow T_{12}\mathord\Rightarrow \varDelta_1 \quad \vdash p_2: T_{11}\mathord\Rightarrow x_2\mathord: T_2}{\vdash p_1\ p_2 : T_{12} \mathord\Rightarrow \varDelta_1, x_2\mathord:T_2} \quad & \text{(PT-APP)} \\
\end{align*}
```

### examples
