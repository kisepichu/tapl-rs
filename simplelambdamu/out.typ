#import "@preview/curryst:0.5.1": rule, prooftree

--- before

#prooftree(
  rule(
    name: $scripts(->)_"E"$,
    $N->N$,
    rule(
      name: $scripts(->)_"I", 1$,
      $(((N->N)->bot)->N->N)->N->N$,
      rule(
        name: $bot_"C", 2$,
        $N->N$,
        rule(
          name: $scripts(->)_"E"$,
          $bot$,
          $[(N->N)->bot]^2$,
          rule(
            name: $scripts(->)_"E"$,
            $N->N$,
            $[((N->N)->bot)->N->N]^1$,
            rule(
              name: $scripts(->)_"I", 3$,
              $(N->N)->bot$,
              rule(
                name: $scripts(->)_"E"$,
                $bot$,
                $[(N->N)->bot]^2$,
                $[N->N]^3$,
              ),
            ),
          ),
        ),
      ),
    ),
    rule(
      name: $scripts(->)_"I", 4$,
      $((N->N)->bot)->N->N$,
      rule(
        name: $bot_"C", 5$,
        $N->N$,
        rule(
          name: $scripts(->)_"E"$,
          $bot$,
          $[(N->N)->bot]^5$,
          rule(
            name: $scripts(->)_"I", 6$,
            $N->N$,
            $[N]^6$,
          ),
        ),
      ),
    ),
  )
)

--- cbv
#prooftree(
  rule(
    name: $bot_"C", 1$,
    $N->N$,
    rule(
      name: $scripts(->)_"E"$,
      $bot$,
      $[(N->N)->bot]^1$,
      rule(
        name: $scripts(->)_"E"$,
        $N->N$,
        rule(
          name: $scripts(->)_"I", 2$,
          $((N->N)->bot)->N->N$,
          rule(
            name: $bot_"C", 3$,
            $N->N$,
            rule(
              name: $scripts(->)_"E"$,
              $bot$,
              $[(N->N)->bot]^3$,
              rule(
                name: $scripts(->)_"I", 4$,
                $N->N$,
                $[N]^4$,
              ),
            ),
          ),
        ),
        rule(
          name: $scripts(->)_"I", 5$,
          $(N->N)->bot$,
          rule(
            name: $scripts(->)_"E"$,
            $bot$,
            $[(N->N)->bot]^1$,
            $[N->N]^5$,
          ),
        ),
      ),
    ),
  )
)

--- normal order

#prooftree(
  rule(
    name: $scripts(->)_"I", 1$,
    $N->N$,
    $[N]^1$,
  )
)

---

--- before

#prooftree(
  rule(
    name: $scripts(->)_"I", 1$,
    $(N->N)->N->N$,
    rule(
      name: $scripts(->)_"I", 2$,
      $N->N$,
      rule(
        name: $bot_"C", 3$,
        $N$,
        rule(
          name: $scripts(->)_"E"$,
          $bot$,
          $[N->bot]^3$,
          rule(
            name: $scripts(->)_"E"$,
            $N$,
            $[N->N]^1$,
            rule(
              name: $scripts(->)_"E"$,
              $N$,
              $[N->N]^1$,
              rule(
                name: $bot_"C", 4$,
                $N$,
                rule(
                  name: $scripts(->)_"E"$,
                  $bot$,
                  $[N->bot]^3$,
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

--- normal order


#prooftree(
  rule(
    name: $scripts(->)_"I", 1$,
    $(N->N)->N->N$,
    rule(
      name: $scripts(->)_"I", 2$,
      $N->N$,
      $[N]^2$,
    ),
  )
)


---

--- before

#prooftree(
  rule(
    name: $scripts(->)_"I", 1$,
    $((A->bot)->bot)->A$,
    rule(
      name: $scripts(->)_"E"$,
      $A$,
      rule(
        name: $scripts(->)_"I", 2$,
        $((A->bot)->A)->A$,
        rule(
          name: $bot_"C", 3$,
          $A$,
          rule(
            name: $scripts(->)_"E"$,
            $bot$,
            $[A->bot]^3$,
            rule(
              name: $scripts(->)_"E"$,
              $A$,
              $[(A->bot)->A]^2$,
              rule(
                name: $scripts(->)_"I", 4$,
                $A->bot$,
                rule(
                  name: $scripts(->)_"E"$,
                  $bot$,
                  $[A->bot]^3$,
                  $[A]^4$,
                ),
              ),
            ),
          ),
        ),
      ),
      rule(
        name: $scripts(->)_"I", 5$,
        $(A->bot)->A$,
        rule(
          name: $bot_"C", 6$,
          $A$,
          rule(
            name: $scripts(->)_"E"$,
            $bot$,
            $[(A->bot)->bot]^1$,
            $[A->bot]^5$,
          ),
        ),
      ),
    ),
  )
)

--- normal order

#prooftree(
  rule(
    name: $scripts(->)_"I", 1$,
    $((A->bot)->bot)->A$,
    rule(
      name: $bot_"C", 2$,
      $A$,
      rule(
        name: $scripts(->)_"E"$,
        $bot$,
        $[(A->bot)->bot]^1$,
        $[A->bot]^2$,
      ),
    ),
  )
)
