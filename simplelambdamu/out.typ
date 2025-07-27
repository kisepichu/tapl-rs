#import "@preview/curryst:0.5.1": rule, prooftree

#let impl-i = rule.with(name: $scripts(->)_"I"$)
#let impl-e = rule.with(name: $scripts(->)_"E"$)
#let bot-c = rule.with(name: $scripts(⊥)_"C"$)

#prooftree(
  rule(
    name: $R$,
    $C_1 or C_2 -> C_3$,
    rule(
      name: $A$,
      $C_1 or C_2 or L$,
      rule(
        $C_1 or L$,
        $Pi_1$,
      ),
    ),
    impl-i(
      $C_2 or overline(L) -> A$,
      $bot$,
    ),
    bot-c(
      $C_3$,
      $Pi_1$,
    ),
  )
)

#prooftree(
  rule(
    name: $scripts(->)_"I", "todo"$,
    $A$,
    $[A]^0$
  )
)



#prooftree(
  rule(
    name: $scripts(->)_"I", "todo"$,
    $(N->N)->N->N$,
    rule(
      name: $scripts(->)_"I", "todo"$,
      $N->N$,
      rule(
        name: $bot_"C", "todo"$,
        $N$,
        rule(
          name: $scripts(->)_"E"$,
          $bot$,
          $[N->bot]^0$,
          rule(
            name: $scripts(->)_"E"$,
            $N$,
            $[N->N]^2$,
            rule(
              name: $scripts(->)_"E"$,
              $N$,
              $[N->N]^2$,
              rule(
                name: $bot_"C", "todo"$,
                $N$,
                rule(
                  name: $scripts(->)_"E"$,
                  $bot$,
                  $[N->bot]^1$,
                  $[N]^2$,
                ),
              ),
            ),
          ),
        ),
      ),
    ),
  )
)

---

#prooftree(
  rule(
    name: $scripts(->)_"I", "todo"$,
    $(N->N)->N->N$,
    rule(
      name: $scripts(->)_"I", "todo"$,
      $N->N$,
      $[N]^0$,
    ),
  )
)
