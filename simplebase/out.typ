#import "@preview/curryst:0.5.1": rule, prooftree

#prooftree(
  rule(
    name: $scripts(->)_"I", 1$,
    $(A->A)->A->A$,
    rule(
      name: $scripts(->)_"I", 2$,
      $A->A$,
      $[A]^2$,
    ),
  )
)
