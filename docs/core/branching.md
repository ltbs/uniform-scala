---
layout: docs
title: Branching
---

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
// res0: Uniform[Needs.Interact[Unit, Int] with Needs.Interact[Unit, String], Unit, ((String, Int), (String, Int))] = FlatMap(
//   FlatMap(
//     FlatMap(
//       FlatMap(
//         Interact(
//           "sender-name",
//           (),
//           None,
//           <function1>,
//           Map(),
//           Tag[Unit],
//           Tag[String]
//         ),
//         cats.FlatMap$$Lambda$23180/691449511@af3b46f
//       ),
//       cats.Monad$$Lambda$23183/1892195148@4d841ab5
//     ),
//     cats.FlatMap$$Lambda$23180/691449511@270c05a9
//   ),
//   cats.Monad$$Lambda$23183/1892195148@5224bafd
// )
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
// res1: Uniform[Needs.Interact[Unit, Boolean] with Needs.Interact[Unit, Int] with Needs.Interact[Unit, String], Unit, ((String, Int), (String, Int), Option[Product with Serializable with Object])] = FlatMap(
//   FlatMap(
//     FlatMap(
//       FlatMap(
//         Interact(
//           "sender-name",
//           (),
//           None,
//           <function1>,
//           Map(),
//           Tag[Unit],
//           Tag[String]
//         ),
//         cats.FlatMap$$Lambda$23180/691449511@285fd7a1
//       ),
//       cats.Monad$$Lambda$23183/1892195148@6f39ae4e
//     ),
//     cats.FlatMap$$Lambda$23180/691449511@183d1206
//   ),
//   cats.Monad$$Lambda$23183/1892195148@4462fef7
// )
```

In this case the` journey would ask the user the same 4 questions
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
// res2: Uniform[Needs.Interact[Unit, Boolean] with Needs.Interact[Unit, Person], Unit, Option[Person]] = FlatMap(
//   Interact("add-person", (), None, <function1>, Map(), Tag[Unit], Tag[Boolean]),
//   <function1>
// )
```

When taking a journey that returns a boolean the approach is the same 
but essentially it does not need an intermediary variable - 

```scala
ask[Person]("person") when ask[Boolean]("add-person")
// res3: Uniform[Needs.Interact[Unit, Person] with Needs.Interact[Unit, Boolean], Unit, Option[Person]] = FlatMap(
//   Interact("add-person", (), None, <function1>, Map(), Tag[Unit], Tag[Boolean]),
//   ltbs.uniform.Uniform$$Lambda$23188/917427409@30bf5ccf
// )
```

## unless 

`unless` is just `when` but with the predicate inverted. 

```scala
sealed trait Booze
case object Beer extends Booze
case class Martini(olive: Boolean) extends Booze

ask[Booze]("choose-drink") unless ask[Int]("age").map(_ < 18)
// res4: Uniform[Needs.Interact[Unit, Booze] with Needs.Interact[Unit, Int], Unit, Option[Booze]] = FlatMap(
//   Map(
//     Map(
//       Interact("age", (), None, <function1>, Map(), Tag[Unit], Tag[Int]),
//       <function1>
//     ),
//     ltbs.uniform.Uniform$$Lambda$23189/1940088186@40e2d5fc
//   ),
//   ltbs.uniform.Uniform$$Lambda$23188/917427409@13241e51
// )
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
// res5: Uniform[Needs.Interact[Unit, Int] with Needs.Interact[Unit, Boolean], Unit, Int] = FlatMap(
//   Map(
//     Interact("retired", (), None, <function1>, Map(), Tag[Unit], Tag[Boolean]),
//     ltbs.uniform.Uniform$$Lambda$23190/871055235@370e61d3
//   ),
//   ltbs.uniform.Uniform$$Lambda$23191/1118011747@7d9d1ad9
// )
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
