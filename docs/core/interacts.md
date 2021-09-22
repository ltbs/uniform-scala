---
layout: docs
title: Interacting
---

# Interacting

```
def interact[Tell, Ask](stepId: String, value: Tell): Uniform[Ask]
```

`interact` is simply the combination of an `ask` and a `tell`
together. 

It has a type parameter for both the datatype being presented to the
user (`Tell`) and being asked of the user (`Ask`). A confirmation page
presenting a user with an address and asking them for a Yes/No answer
could be modeled as `interact[Address, Boolean]("confirm-address", addr)`. 

Both `ask` and `tell` can be considered as being defined in terms of `interact` -

```scala
def tell[A](stepId: String, value: A) = 
  interact[A, Unit](stepId, value)

def ask[A](stepId: String): F[A] = 
  interact[Unit, A](stepId, ())
```

