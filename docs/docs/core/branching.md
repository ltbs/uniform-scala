---
layout: docs
title: Branching
---

> **Any logic you can use in Scala to branch can also be applied to Uniform journeys.**

> **`when`, `unless` and their monoid variants provide convenient shorthand for some common interactions.**

# Branching

If no steps in a journey are dependent upon the result of a previous
one then using a for comprehension may be overkill and you may prefer
using `cats.Applicative` instead -

```scala

import ltbs.uniform._
import cats.implicits._

case class Person(name: String, age: Int)

def askPerson(id: String) = (
  ask[String](s"$id-name"),
  ask[Int](s"$id-age")
).tupled

(askPerson("sender"), askPerson("receiver")).tupled
```

But lets suppose we want an optional third `Person` in our tuple, 
and we want to use branching -

```scala
(
  askPerson("sender"),
  askPerson("receiver"),
  ask[Boolean]("use-cc") flatMap {
    case true  => askPerson("cc") map {_.some}
    case false => pure(none[Person])
  }
).tupled
```

In this case the journey would ask the user the same 4 questions
initially as `senderAndReceiverApplicative`, however it would then ask the user
for a `Boolean`. If they answer no then the journey would end with
`_3` being `None`. If the user picked yes however then they would be
asked again for a name and age and this time `_3` would be defined
(`Some`).

This could be used for all sorts of branching - you are not confined
to booleans, or to using pattern matching.

```scala
def bigSpender = for {
  spendAny    <- ask[Boolean]("spendAny")
  spendAmount <- if (spendAny) {
                   ask[Int]("spendAmount")
                 } else {
                   pure(0)
                 }
  optSpender  <- if (spendAmount > 100000)
                 (
                   ask[String]("name"),
                   ask[Int]("age")
                 ).mapN(Person).map{_.some}
                 else
                   pure(none[Person])
} yield optSpender
```

# Simplified branching

The specific use-case of using a `Boolean` to control an
`Option` comes up a lot, so uniform offers a special syntax for it.

## when

`when` is a construct that can take either a `Boolean` directly or a 
interaction that returns a `Boolean` (such as `ask[Boolean]`). 

Used directly with a boolean it emits an option in the same behaviour as 
`optSpender` above - that is it returns a `Some[A]` where when the 
predicate is `true`, and a `None` when the predicate is `false`. 
`when` will short-circuit the journey and not execute the `ask[A]` 
in the event that the predicate returns `false`. 

```scala
for {
  add    <- ask[Boolean]("add-person")
  person <- ask[Person]("person") when add 
} yield person
```

When taking a journey that returns a boolean the approach is the same 
but essentially it does not need an intermediary variable - 

```scala
ask[Person]("person") when ask[Boolean]("add-person")
```

## unless 

`unless` is just `when` but with the predicate inverted. 

```scala
sealed trait Booze
case object Beer extends Booze
case class Martini(olive: Boolean) extends Booze

ask[Booze]("choose-drink") unless ask[Int]("age").map(_ < 18)
```

## emptyWhen and emptyUnless

 Similar to `when` and `unless` is `emptyWhen` and `emptyUnless`, 
 this however only works if the
 datatype you are asking for is a `Monoid`, in which case it will give
`empty` instead of `None`. The return datatype is kept the same 
 as the underlying `ask`.

For example -

```scala
ask[Int]("hoursWorked") emptyWhen ask[Boolean]("retired")
```

We can apply this in the context of our earlier program in order to
simplify the code -

```scala
def bigSpender2 = for {
  spendAmount <- ask[Int]("spendAmount") emptyUnless
                   ask[Boolean]("spendAny")
  optSpender  <- (
                   ask[String]("name"),
                   ask[Int]("age")
                 ).mapN(Person) when (spendAmount > 100000)
} yield optSpender

```
