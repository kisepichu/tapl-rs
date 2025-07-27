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


--- before

#prooftree(
  rule(
    name: $scripts(->)_"E"$,
    $N->N$,
    rule(
      name: $scripts(->)_"I", 0$,
      $(((N->N)->bot)->N->N)->N->N$,
      rule(
        name: $bot_"C", 0$,
        $N->N$,
        rule(
          name: $scripts(->)_"E"$,
          $bot$,
          $[(N->N)->bot]^0$,
          rule(
            name: $scripts(->)_"E"$,
            $N->N$,
            $[((N->N)->bot)->N->N]^1$,
            rule(
              name: $scripts(->)_"I", 0$,
              $(N->N)->bot$,
              rule(
                name: $scripts(->)_"E"$,
                $bot$,
                $[(N->N)->bot]^1$,
                $[N->N]^0$,
              ),
            ),
          ),
        ),
      ),
    ),
    rule(
      name: $scripts(->)_"I", 0$,
      $((N->N)->bot)->N->N$,
      rule(
        name: $bot_"C", 0$,
        $N->N$,
        rule(
          name: $scripts(->)_"E"$,
          $bot$,
          $[(N->N)->bot]^0$,
          rule(
            name: $scripts(->)_"I", 0$,
            $N->N$,
            $[N]^0$,
          ),
        ),
      ),
    ),
  )
)

--- cbv

#prooftree(
  rule(
    name: $bot_"C", 0$,
    $N->N$,
    rule(
      name: $scripts(->)_"E"$,
      $bot$,
      $[(N->N)->bot]^0$,
      rule(
        name: $scripts(->)_"E"$,
        $N->N$,
        rule(
          name: $scripts(->)_"I", 0$,
          $((N->N)->bot)->N->N$,
          rule(
            name: $bot_"C", 0$,
            $N->N$,
            rule(
              name: $scripts(->)_"E"$,
              $bot$,
              $[(N->N)->bot]^0$,
              rule(
                name: $scripts(->)_"I", 0$,
                $N->N$,
                $[N]^0$,
              ),
            ),
          ),
        ),
        rule(
          name: $scripts(->)_"I", 0$,
          $(N->N)->bot$,
          rule(
            name: $scripts(->)_"E"$,
            $bot$,
            $[(N->N)->bot]^1$,
            $[N->N]^0$,
          ),
        ),
      ),
    ),
  )
)

--- normalorder

#prooftree(
  rule(
    name: $scripts(->)_"I", 0$,
    $N->N$,
    $[N]^0$,
  )
)



---

--- before


#prooftree(
  rule(
    name: $scripts(->)_"I", 0$,
    $(N->N)->N->N$,
    rule(
      name: $scripts(->)_"I", 0$,
      $N->N$,
      rule(
        name: $bot_"C", 0$,
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
                name: $bot_"C", 0$,
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
normalorder

#prooftree(
  rule(
    name: $scripts(->)_"I", 0$,
    $(N->N)->N->N$,
    rule(
      name: $scripts(->)_"I", 0$,
      $N->N$,
      $[N]^0$,
    ),
  )
)


---

--- before

#prooftree(
  rule(
    name: $scripts(->)_"I", 0$,
    $((A->bot)->bot)->A$,
    rule(
      name: $scripts(->)_"E"$,
      $A$,
      rule(
        name: $scripts(->)_"I", 0$,
        $((A->bot)->A)->A$,
        rule(
          name: $bot_"C", 0$,
          $A$,
          rule(
            name: $scripts(->)_"E"$,
            $bot$,
            $[A->bot]^0$,
            rule(
              name: $scripts(->)_"E"$,
              $A$,
              $[(A->bot)->A]^1$,
              rule(
                name: $scripts(->)_"I", 0$,
                $A->bot$,
                rule(
                  name: $scripts(->)_"E"$,
                  $bot$,
                  $[A->bot]^1$,
                  $[A]^0$,
                ),
              ),
            ),
          ),
        ),
      ),
      rule(
        name: $scripts(->)_"I", 0$,
        $(A->bot)->A$,
        rule(
          name: $bot_"C", 0$,
          $A$,
          rule(
            name: $scripts(->)_"E"$,
            $bot$,
            $[(A->bot)->bot]^2$,
            $[A->bot]^1$,
          ),
        ),
      ),
    ),
  )
)

--- normalorder

#prooftree(
  rule(
    name: $scripts(->)_"I", 0$,
    $((A->bot)->bot)->A$,
    rule(
      name: $bot_"C", 0$,
      $A$,
      rule(
        name: $scripts(->)_"E"$,
        $bot$,
        $[(A->bot)->bot]^1$,
        $[A->bot]^0$,
      ),
    ),
  )
)
