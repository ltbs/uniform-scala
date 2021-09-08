---
layout: docs
title: Interacting
---

# Interacting

`interact` is simply the combination of an `ask` and a `tell`
together into a single step. In fact we can consider both `ask` and `tell` 
to be specialised forms of `interact`. 

It has a type parameter for both the datatype being presented to the
user (`Tell`) and being asked of the user (`Ask`). A confirmation page
presenting a user with an address and asking them for a Yes/No answer
could be modeled as `interact[Address, Boolean]("confirm-address", addr)`. 

# Relationship with `ask` and `tell`

Recall that a uniform step has three type parameters - 

1. Tracks the types used throughout the whole journey
2. The type the user is being told (`Unit` in case of `ask`)
3. The type the user is being told (`Unit` in case of `tell`)

Using some simplified pseudocode and ignoring the 1st type paramater
we can see that both `ask` and `tell`
can be considered as being defined in terms of `interact` -

```scala
def interact[T,A](stepId: String, value: T): Uniform[T, A]

def tell[T](stepId: String, value: T): Uniform[T, Unit] = 
  interact[T, Unit](stepId, value)

def ask[A](stepId: String): Uniform[Unit, A] = 
  interact[Unit, A](stepId, ())
```

`ask` is simply an `interact` with no data being presented to the
user, and `tell` is just an interact with no data being requested of
the user.

We will actually see this when we look at the type signatures of `ask`
and `tell` -

```scala
import ltbs.uniform._

val aTell = tell("tell", 123)
// aTell: Uniform[Needs.Interact[Int, Unit], Int, Unit] = Interact(
//   "tell",
//   123,
//   None,
//   <function1>,
//   Map(),
//   Tag[Int],
//   Tag[Unit]
// )
val anAsk = ask[String]("ask")
// anAsk: Uniform[Needs.Interact[Unit, String], Unit, String] = Interact(
//   "ask",
//   (),
//   None,
//   <function1>,
//   Map(),
//   Tag[Unit],
//   Tag[String]
// )
val anInteract = interact[Boolean]("interact", Option(0L))
// anInteract: Uniform[Needs.Interact[Option[Long], Boolean], Option[Long], Boolean] = Interact(
//   "interact",
//   Some(0L),
//   None,
//   <function1>,
//   Map(),
//   Tag[Option[+Long]],
//   Tag[Boolean]
// )

val composition = for {
  _  <- aTell
  as <- anAsk
  in <- anInteract
} yield (as, in)
// composition: Uniform[Needs.Interact[Int, Unit] with Needs.Interact[Unit, String] with Needs.Interact[Option[Long], Boolean], Int with Unit with Option[Long], (String, Boolean)] = FlatMap(
//   Interact("tell", 123, None, <function1>, Map(), Tag[Int], Tag[Unit]),
//   <function1>
// )
```
