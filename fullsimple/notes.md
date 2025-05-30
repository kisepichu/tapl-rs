# Extensions of simply typed lambda calculus

```
$ cargo run --bin fullsimple
```

11(p.89)

単純型付き λ 計算の拡張は、(型付けされたまま、型安全なまま)表現できるものの範囲を拡張したもの。

- Unit
- Nat
- let
- letrec
- pattern let
- record
- variants, case

## Syntax

`fn parse` in [`fullsimple/src/parser.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/parser.rs), `enum Term` in [`fullsimple/src/syntax/term.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/syntax/term.rs)

### Concrete syntax

```bnf
<term> ::= <seq>
<seq> ::= <app> ";" <seq> | <app>
<app>  ::= <postfix> <app> | <postfix>
<postfix> ::= <atom> <projection> | <atom>
<projection> ::= "." <labelorindex>
<atom> ::= <encl> | <abs> | <if> | <let> | <plet> | <case> | <var> | <unit> | <true> | <false> | <record> | <tagging> | <lettype>
<encl> ::= "(" <term> ")"
<abs> ::= "\:" <ty> "." <term> | "\" <bound> ":" <ty> "." <term>
<letrec> ::= "letrec" <bound> ":" <ty> "=" <term> "in" <term>
<fix> ::= "fix" <term>
<lettype> ::= "type" <ident> "=" <type> "in" <term>
<case> ::= "case" <term> "of" <armes>
<let> ::= "let" <bound> "=" <term> "in" <term>
<plet> ::= "let" <pat> "=" <term> "in" <term>
<if> ::= "if" <term> "then" <term> "else" <term>
<arms> ::= "|" <pattagging> "=>" <term> <arms> | null
<record> ::= "{" <inner> "}"
<inner> ::= <fieldseq> | <notrailing>
<notrailing> ::= <fieldseq> <field>
<fieldseq> ::= <field> "," <fieldseq> | null
<field> ::= <label> "=" <term> | <term>
<tagging> ::= <tyatom> ":::" <labelorindex>
<pred> ::= "pred" <term>
<succ> ::= "succ" <term>
<iszero> ::= "iszero" <term>
<labelorindex> ::= <label> | number
<label> ::= <ident>
<bound> ::= <ident>
<var> ::= number | <ident>
<unit> ::= "unit"
<true> ::= "true"
<false> ::= "false"
<zero> ::= "zero"
<ident> ::= <ident> (alphabet|digit) | alphabet


<pat> ::= <patvar> | <patrecord> | <pattagging>
<patvar> ::= <bound> ":" <ty>
<patrecord> ::= "{" <patinner> "}"
<patinner> ::= <patfieldseq> | <patnotrailing>
<patnotrailing> ::= <patfieldseq> <patfield>
<patfieldseq> ::= <patfield> "," <patfieldseq> | null
<patfield> ::= <label> ":" <pat> | <pat>
<pattagging> ::= <ty> ":::" <parse_labelorindex> | <pattagging> <ident>


<ty> ::= <tyarr>
<tyarr> ::= <tysum> "->" <tyarr> | <tysum>
<tysumorprod> ::= <tysum> | <typrod>
<tysum> ::= <tyvariant> "+" <tysum> | <tyvariant> "+" <tyvariant>
<tyvariant> ::= <tyvariant> <typrod> | <label>
<typrod> ::= <tyatom> "*" <typrod> | <tyatom>
<tyatom> ::= <tyencl> | <tyunit> | <tybool> | <tynat> | <tyvar> | <tyrecord> | <tytagging> | <tyself>
<tyencl> ::= "(" <ty> ")"
<tytagging>::= "<" <tyinner> ">"
<tyrecord> ::= "{" <tyinner> "}"
<tyinner> ::= <tyfieldseq> | <tynotrailing>
<tynotrailing> ::= <tyfieldseq> <tyfield>
<tyfieldseq> ::= <tyfield> "," <tyfieldseq> | null
<tyfield> ::= <label> ":" <ty> | <ty>
<tyvar> ::= <ident>
<tyunit> ::= "Unit"
<tybool> ::= "Bool"
<tynat> ::= "Nat"
<tyself> ::= "Self"
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
  \quad \mid\ &\mathrm{zero} &\quad (\text{constant zero}) \\
  \quad \mid\ &\mathrm{succ}\ t &\quad (\text{successor}) \\
  \quad \mid\ &\mathrm{pred}\ t &\quad (\text{predecessor}) \\
  \quad \mid\ &\mathrm{iszero}\ t &\quad (\text{iszero}) \\
  \quad \mid\ &\{l_i\mathord:T_i,^{i\in1..n}\} &\quad (\text{record}) \\
  \quad \mid\ &t.l &\quad (\text{projection}) \\
  \quad \mid\ & T\mathord{:::}l &\quad (\text{tagging}) \\
  \quad \mid\ &\mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 &\quad (\text{if}) \\
  \quad \mid\ &\mathrm{let}\ t_1\ \mathrm{in}\ t_2 &\quad (\text{let}) \\
  \quad \mid\ &\mathrm{plet}\ p = t_1\ \mathrm{in}\ t_2 &\quad (\text{pattern let}) \\
  \quad \mid\ & \mathrm{case}\ t\ \mathrm{of} \ p_{\mathrm{tag}i} \Rightarrow t_i \ ^{i\in 1..n} &\quad (\text{case}) \\
  \quad \mid\ &\mathrm{fix}\ t &\quad (\text{fixed point})
  \\
  \\

v ::=&   &\quad (\text{values}) \\
  \quad \mid\ &\lambda\mathord{:}T.t_2 &\quad (\text{abstraction value}) \\
  \quad \mid\ &\mathrm{unit} &\quad (\text{unit}) \\
  \quad \mid\ &\mathrm{true} &\quad (\text{true}) \\
  \quad \mid\ &\mathrm{false} &\quad (\text{false}) \\
  \quad \mid \ &v_{\mathrm{n}} &\quad (\text{numeric value}) \\
  \quad \mid\ &\{l_i\mathord=v_i,^{i\in1..n}\} &\quad (\text{record value}) \\
  \quad \mid\ & v_{\mathrm{tag}} &\quad (\text{tagging value}) \\
v_{\mathrm{n}} ::=& &\quad (\text{numeric values}) \\
  \quad \mid\ &\mathrm{zero} &\quad (\text{zero}) \\
  \quad \mid\ &\mathrm{succ}\ v_{\mathrm{n}} &\quad (\text{successor}) \\
v_{\mathrm{tag}} ::=& &\quad (\text{tagging value}) \\
  \quad \mid\ &T\mathord{:::}l &\quad (\text{tagging}) \\
  \quad \mid\ &v_{\mathrm{tag}}\ v &\quad (\text{tagging application}) \\
  \\


p ::=&   &\quad (\text{patterns}) \\
  \quad \mid\ & x\mathord{:}T &\quad (\text{variable pattern}) \\
  \quad \mid\ & \{l_i\mathord=p_i^{i\in1..n}\} &\quad (\text{record pattern}) \\
  \quad \mid\ & p_{\mathrm{tag}} &\quad (\text{tagging pattern}) \\
p_{\mathrm{tag}} ::=& &\quad (\text{tagging pattern}) \\
  \quad \mid\ & T\mathord{:::}l\ n\mathord-1\ n\mathord-2\ \dots \ 0 &\quad (\text{tagging}) \\
  \\

T ::=&   &\quad (\text{types}) \\
  \quad \mid\ &T_1 \rightarrow T_2 &\quad (\text{arrow}) \\
  \quad \mid\ &\mathrm{Unit} &\quad (\text{unit type}) \\
  \quad \mid\ &\mathrm{Bool} &\quad (\text{boolean}) \\
  \quad \mid\ &\mathrm{Nat} &\quad (\text{natural number type}) \\
  \quad \mid\ &\{l_i\mathord:T_i,^{i\in1..n}\} &\quad (\text{record type}) \\
  \quad \mid\ & \langle l_i\mathord:T_i,^{i \in 1..n}\rangle &\quad (\text{variant type}) \\
  \\

\Gamma ::=&   &\quad (\text{contexts}) \\
  \quad \mid\ &\varnothing &\quad (\text{empty}) \\
  \quad \mid\ &\uparrow^1\Gamma, 0\mathord{:}T &\quad (\text{term variable binding}) \\
\end{align*}
```

- フレッシュな変数を 0 に固定できるように、 contexts の定義を本文と変えている。 $\uparrow^n \Gamma$ を $\Gamma$ の変数を全て $n$ 個シフトしたものとして(独自の記法)、本文の $\Gamma, x\mathord{:}T$ は $\uparrow^1 \Gamma, 0\mathord:T$ のように書けば、名無し項のまま表せて、このまま実装できる。
- この段階では、 variant type や record のフィールドの順序の違いを区別する。11.8(p.99)

### Derived forms

```math
\begin{align*}
  \quad & \quad & \text{(derived forms)} \\
t_1; t_2 \stackrel{\mathrm{def}}{=}\ & (\lambda\mathord{:}\mathrm{Unit}.\uparrow^1 t_2) t_1 \quad & (\text{sequence}) \\
\\
\mathrm{let}\ x=t_1\ \mathrm{in}\ t_2 \stackrel{\mathrm{def}}{=}\ & \mathrm{let}\ t_1\ \mathrm{in}\ [x\mapsto 0]t_2 \quad & (\text{let'}) \\
\\
\lambda x:T.t_2 \stackrel{\mathrm{def}}{=}\ & \lambda T.[x\mapsto 0]t_2 \quad & (\text{abs'})
\\
\\
\mathrm{lettype}\ x=T_1\ \mathrm{in}\ t_2 \stackrel{\mathrm{def}}{=}\ & [x \mapsto T_1]t_2 \quad & (\text{lettype}) \\
\\
T_1 \times T_2 \times \dots \times T_n \stackrel{\mathrm{def}}{=}\ & \{i\mathord:T_i,^{i \in 1..n}\} \quad & (\text{product type}) \\
\\
l_1\ T_{11}\ T_{12} \dots T_{1m_1} + \dots & \\
+\ l_n\ T_{n1}\ T_{n2} \dots T_{nm_n} \stackrel{\mathrm{def}}{=}\ & \langle l_i\mathord: T_{i1} \mathord{\rightarrow} \dots \mathord{\rightarrow} T_{im_i} \mathord{\rightarrow} \mathrm{Self},^{i \in 1..n}\rangle \quad & (\text{sum type}) \\
\\
\mathrm{letrec}\ x\mathord{:}T = t_1\ \mathrm{in}\ t_2 \stackrel{\mathrm{def}}{=}\ & \mathrm{let}\ x = \mathrm{fix}\ (\lambda\mathord{:}T.t_1)\ \mathrm{in}\ t_2 \quad & (\text{letrec}) \\
\end{align*}
```

補足:

- それぞれ名無し項用に本文と定義を変えている。同じになることは未証明
- let は abs を使った糖衣構文にはしない。11.5(p.95)
- 名前 $x$ を置換する $[x\rightarrow 0]$ の実装は `subst_name` in [`fullsimple/src/parser.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/src/parser.rs)
- sum のために Self を variants 全体に展開するものとして実装したら、本文にまだ出てきていないのに再帰型を書けるようになってしまった。 `type N = Zero + Succ Self` ができて、 case で場合分けできる。性質をちゃんと考えていないので例等には入れないでおく(sum の糖衣構文の展開後だけに表れるものとする)。

### Parsing

- `<var>`, `<true>`, `<false>`, `<unit>`, `<if>`, `<record>` が、それぞれ対応する term に変換される。
  - `<record>` は、ラベルがない形式(tuple) も parse し、 0 から自動でラベルを付けて record に変換する。
- `<app>` は、 `<atom>` の列が左結合で application に変換される。
- `<tybool>` は boolean に変換され、 `<tyarr>` は、 `<tyatom>` と "->" の列が右結合で arrow に変換される。
- 上記の糖衣構文(syntactic sugar, derived forms)を含む。
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
\frac{t_1 \rightarrow t_1'}{\mathrm{succ}\ t_1 \rightarrow \mathrm{succ}\ t_1'} \quad & \text{(E-SUCC)} \\
\\
\frac{}{\mathrm{pred} \ \mathrm{zero} \rightarrow \mathrm{zero}} \quad & \text{(E-PREDZERO)} \\
\\
\frac{}{\mathrm{pred} \ (\mathrm{succ}\ v_1) \rightarrow v_1} \quad & \text{(E-PREDSUCC)} \\
\\
\frac{t_1 \rightarrow t_1'}{\mathrm{pred}\ t_1 \rightarrow \mathrm{pred}\ t_1'} \quad & \text{(E-PRED)} \\
\\
\frac{}{\mathrm{iszero} \ \mathrm{zero} \rightarrow \mathrm{true}} \quad & \text{(E-ISZEROZERO)} \\
\\
\frac{}{\mathrm{iszero} \ (\mathrm{succ}\ v_1) \rightarrow \mathrm{false}} \quad & \text{(E-ISZEROSUCC)} \\
\\
\frac{t_1 \rightarrow t_1'}{\mathrm{iszero}\ t_1 \rightarrow \mathrm{iszero}\ t_1'} \quad & \text{(E-ISZERO)} \\
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
\frac{}{\mathrm{plet}\ p=v_1\ \mathrm{in}\ t_2 \rightarrow \mathit{match}(p, v_1)t_2} \quad &(\text{E-PLETV}) \\
\\
\frac{t_1\rightarrow t_1'}{\mathrm{let}\ t_1\ \mathrm{in}\ t_2 \rightarrow \mathrm{plet}\ p=t_1'\ \mathrm{in}\ t_2} \quad &(\text{E-PLET}) \\
\\
\frac{}{\{l_i\mathord=v_i,^{i\in 1..n}\}.l_j \rightarrow v_j} \quad &(\text{E-PROJRCD}) \\
\\
\frac{t_1\rightarrow t_1'}{t_1.l \rightarrow t_1'.l} \quad &(\text{E-PROJ}) \\
\\
\frac{t_j\rightarrow t_j'}{\{l_i\mathord=v_i^{i\in 1..j-1}, l_j\mathord=t_j, l_k\mathord=t_k^{k\in j+1..n}\} \\ \rightarrow \{l_i\mathord=v_i^{i\in 1..j-1}, l_j\mathord=t_j', l_k\mathord=t_k^{k\in j+1..n}\}} \quad &(\text{E-RCD}) \\
\\
\frac{}{\mathrm{case}\ v_{\mathrm{tag}j}\ \mathrm{of}\ p_{\mathrm{tag}i} \Rightarrow t_i\ ^{i\in 1..n} \rightarrow \mathit{match}(p_{\mathrm{tag}j}, v_{\mathrm{tag}j})t_j} \quad &(\text{E-CASEVARIANT}) \\
% \begin{align*}
%   \text{where }v_{\mathrm{tag}j} &:= T\mathord{:::}l_j\ v_1\ v_2\ \dots\ v_n &(n \ge 0), \\
%   p_{\mathrm{tag}i} &:= T\mathord{:::}l_i\ n_i\mathord-1\ n_i\mathord-2\ \dots\ 0 &(n_i \ge 0). \\
% \end{align*}
\begin{aligned}
  \text{where }v_{\mathrm{tag}j} &:= T\mathord{:::}l_j\ v_1\ v_2\ \dots\ v_n &(n \ge 0), \\
  p_{\mathrm{tag}i} &:= T\mathord{:::}l_i\ n_i\mathord-1\ n_i\mathord-2\ \dots\ 0 &(n_i \ge 0). \\
\end{aligned}\\
\\
\frac{t \rightarrow t'}{\mathrm{case}\ t\ \mathrm{of}\ p_i \Rightarrow t_i\ ^{i\in 1..n} \rightarrow
\mathrm{case}\ t'\ \mathrm{of}\ p_i \Rightarrow t_i\ ^{i\in 1..n}} \quad &(\text{E-CASE}) \\
\\
\frac{}{\mathrm{fix}\ (\lambda\mathord{:}T.t) \rightarrow \uparrow^{-1} ([0 \mapsto \mathrm{fix}(\lambda\mathord{:}T.t)]t)} \quad &(\text{E-FIXBETA}) \\
\\
\frac{t \rightarrow t'}{\mathrm{fix}\ t \rightarrow \mathrm{fix}\ t'} \quad &(\text{E-FIX}) \\
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
\mathit{match}(p_{\mathrm{tag}}\ x, v_{\mathrm{tag}}\ v) &\stackrel{\mathrm{def}}{=} \mathit{match}(p_{\mathrm{tag}}, v_{\mathrm{tag}}) \mathrm{let}\ x=v\ \mathrm{in} \quad & (\text{M-TAGAPP}) \\
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
\frac{}{\Gamma \vdash \mathrm{zero} : \mathrm{Nat}} \quad & \text{(T-ZERO)} \\
\\
\frac{\Gamma \vdash t_1 \mathord{:} \mathrm{Nat}}{\Gamma \vdash \mathrm{succ}\ t_1 : \mathrm{Nat}} \quad & \text{(T-SUCC)} \\
\\
\frac{\Gamma \vdash t_1 \mathord{:} \mathrm{Nat}}{\Gamma \vdash \mathrm{pred}\ t_1 : \mathrm{Nat}} \quad & \text{(T-PRED)} \\
\\
\frac{\Gamma \vdash t_1 \mathord{:} \mathrm{Nat}}{\Gamma \vdash \mathrm{iszero}\ t_1 : \mathrm{Bool}} \quad & \text{(T-ISZERO)} \\
\\
\frac{\Gamma \vdash t_1 \mathord{:} \mathrm{Bool} \quad \Gamma \vdash t_2 \mathord{:} T \quad \Gamma \vdash t_3 \mathord{:} T}{\Gamma \vdash \mathrm{if}\ t_1\ \mathrm{then}\ t_2\ \mathrm{else}\ t_3 : T} \quad & \text{(T-IF)} \\
\\
\frac{\Gamma \vdash t_1\mathord{:}T_1 \quad \uparrow^1\Gamma, 0\mathord{:}T_1 \vdash t_2\mathord{:}T_2}{\Gamma \vdash \mathrm{let}\ t_1\ \mathrm{in}\ t_2: T_2} \quad &(\text{T-LET}) \\
\\
\frac{\Gamma \vdash t_1\mathord{:}T_1 \quad \vdash p: T_1\mathord\Rightarrow\varDelta \quad \uparrow^{|\varDelta|}\Gamma, \varDelta \vdash t_2\mathord{:}T_2}{\Gamma \vdash \mathrm{plet}\ p = t_1 \ \mathrm{in}\ t_2: T_2} \quad &(\text{T-PLET}) \\
\\
\frac{\forall i, \Gamma \vdash t_i\mathord:T_i}{\Gamma \vdash \{l_i\mathord=t_i,^{i\in 1..n}\}: \{l_i\mathord=T_i,^{i\in 1..n}\}} \quad & \text{(T-RCD)} \\
\\
\frac{\Gamma \vdash t_1 \mathord: \{l_i\mathord=T_i,^{i\in 1..n}\}}{\Gamma \vdash t_1.l_j : T_j} \quad & \text{(T-PROJ)} \\
\\
\frac{}{\Gamma \vdash \langle l_i\mathord:T_i,^{i \in 1..n}\rangle\mathord{:::}l_j : T_j} \quad & \text{(T-VARIANT)} \\
\\
\frac{\vdash t\mathord:\langle l_i\mathord:T_i,^{i \in 1..n}\rangle \quad \forall ^{i\in n}\vdash p_i : \langle l_i\mathord:T_i,^{i \in 1..n}\rangle\mathord\Rightarrow \varDelta_i\ \land\ \uparrow^{|\varDelta_i|}\Gamma, \varDelta_i \vdash t_i\mathord:T'}{\Gamma \vdash \mathrm{case}\ t\ \mathrm{of}\ p_i \Rightarrow t_i\ ^{i\in 1..n} : T'} \quad & \text{(T-CASE)} \\
\\
\frac{\Gamma \vdash t\mathord{:}T\mathord{\rightarrow}T}{\Gamma \vdash \mathrm{fix}\ t : T} \quad & \text{(T-FIX)} \\
\end{align*}
```

## Pattern typing

パターン $p$ の型付けは $p:T\mathord\Rightarrow\varDelta$ という形をしている。 $T$ はマッチする項の型で、 $\varDelta$ は生成する文脈。名無し項のため、 $\Gamma, \varDelta$ は、 $\varDelta$ のサイズを $n$ として、 $\uparrow^n \Gamma, \varDelta$ のように実装する。

```math
\begin{align*}
\frac{}{\Gamma \vdash x\mathord{:}T :  T \mathord\Rightarrow x\mathord: T} \quad & \text{(PT-VAR)} \\
\\
\frac{\{l_i\ ^{i\in 1..n}\}\subseteq \{k_i\ ^{i\in 1..m}\} \quad \forall^{i\in 1..n}\exists^{i\in 1..n}\ l_i=k_j\ \land\ \vdash p_i : T_j \mathord\Rightarrow \varDelta_i}{\vdash \{l_i\mathord=p_i\ ^{i \in 1..n} \}:\{k_j\mathord:T_j\ ^{j \in 1..m} \}\mathord\Rightarrow \varDelta_1, \varDelta_2, \dots,\varDelta_n} \quad & \text{(PT-RCD')} \\
\\
\frac{}{\vdash T\mathord{:::}l : T \mathord\Rightarrow \varnothing} \quad & \text{(PT-TAG)} \\
\\
\frac{\vdash p_{\mathrm{tag}1} : T_{11}\mathord\rightarrow T_{12}\mathord\Rightarrow \varDelta_1 \quad \Gamma \vdash x_2: T_{11}\mathord\Rightarrow x\mathord:T_{11}}{\Gamma \vdash p_{\mathrm{tag}1}\ x_2 : T_{12} \mathord\Rightarrow \varDelta_1, x\mathord:T_{11}} \quad & \text{(PT-TAGAPP)} \\
\end{align*}
```

### examples

```
$ cargo run --bin fullsimple
> type OptionNat = Some Nat + None in
letrec iseven: OptionNat->Bool = \o:OptionNat.
  case o of
    | OptionNat:::Some n =>
      if iszero n then
        true
      else if iszero (pred n) then
        false
      else
        iseven (OptionNat:::Some (pred (pred n)))
    | OptionNat:::None => false
in
{
  iseven (OptionNat:::Some zero),
  iseven (OptionNat:::Some (succ zero)),
  iseven (OptionNat:::Some (succ (succ zero))),
  iseven OptionNat:::None
}

input= let 0 = fix \:<Some:Nat->Self, None:Self>->Bool.\:<Some:Nat->Self, None:Self>.case 0 of | <Some:Nat->Self, None:Self>:::Some 0 => if iszero 0 then true else if iszero pred (0) then false else 2 (<Some:Nat->Self, None:Self>:::Some pred (pred (0)))| <Some:Nat->Self, None:Self>:::None => false in {0=0 (<Some:Nat->Self, None:Self>:::Some zero), 1=0 (<Some:Nat->Self, None:Self>:::Some succ zero), 2=0 (<Some:Nat->Self, None:Self>:::Some succ (succ zero)), 3=0 <Some:Nat->Self, None:Self>:::None}
     : {0:Bool, 1:Bool, 2:Bool, 3:Bool}
   ->* {0=true, 1=false, 2=true, 3=false}

>
```

[`fullsimple/tests/parse_typing_eval.rs`](https://github.com/kisepichu/tapl-rs/blob/main/fullsimple/tests/parse_typing_eval.rs)
