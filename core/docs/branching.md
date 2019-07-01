---
layout: docs
title: Branching
---

# Branching

If no steps in a journey are dependent upon the result of a previous
one then `cats.Monad` may be overkill and you should consider using
`cats.Applicative` instead -

```tut:silent
import ltbs.uniform._
import cats.implicits._
import scala.language.higherKinds

case class Person(name: String, age: Int)

type AskTypes1 = String :: Int :: NilTypes

def senderAndReceiverApplicative[F[_] : cats.Applicative](
  interpreter: Language[
    F,
    NilTypes,
    AskTypes1
  ]
): F[(Person, Person)] = {
  import interpreter._

  def askPerson(id: String): F[Person] = (
    ask[String](s"$id-name"),
    ask[Int](s"$id-age")
  ).mapN(Person)

  (askPerson("sender"), askPerson("receiver")).tupled
}
```

But lets suppose we want an optional third `Person` in our tuple. We
could literally add an extra type (`Option[Person]`) into our
`AskTypes`, but lets use branching instead -

```tut:silent
// we now need Booleans too
type AskTypes2 = Boolean :: AskTypes1

def senderAndReceiverOptCC[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    AskTypes2
  ]
): F[(Person, Person, Option[Person])] = {
  import interpreter._

  def askPerson(id: String): F[Person] = (
    ask[String](s"$id-name"),
    ask[Int](s"$id-age")
  ).mapN(Person)

  (
    askPerson("sender"),
    askPerson("receiver"),
    ask[Boolean]("use-cc") flatMap {
      case true  => askPerson("cc") map {_.some}
      case false => none[Person].pure[F]
    }
  ).tupled
}
```


In this case the `senderAndReceiverOptCC` journey would ask the user the same 4 questions
initially as `senderAndReceiverApplicative`, however it would then ask the user
for a `Boolean`. If they answer no then the journey would end with
`_3` being `None`. If the user picked yes however then they would be
asked again for a name and age and this time `_3` would be defined
(`Some`).

This could be used for all sorts of branching - you are not confined
to booleans, or to using pattern matching.

```tut:silent
type BigSpenderAskTypes = Boolean :: String :: Int :: NilTypes

def bigSpender[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
	BigSpenderAskTypes
  ]
): F[Option[Person]] = {
  import interpreter._
  for {

    spendAny    <- ask[Boolean]("spendAny")
    spendAmount <- if (spendAny) {
                     ask[Int]("spendAmount")
                   } else {
                     0.pure[F]
                   }
    optSpender  <- if (spendAmount > 100000)
                     (
                      ask[String]("name"),
                       ask[Int]("age")
                     ).mapN(Person).map{_.some}
                   else
                     none[Person].pure[F]
  } yield optSpender
}
```

# Simplified branching

The specific use-case of using a `Boolean` to control an
`Option` comes up a lot, so uniform offers a special syntax for it.

## when

The first is `when`. `when` is a construct that can take either an `F[Boolean]` or a
`Boolean` where `F` is any `Monad`. Used directly with a boolean it
emits an option in the same behaviour as `optSpender` above - that is
it returns a `Some[A]` where when the predicate is `true`, and a
`None` when the predicate is `false`. `when` will short-circuit the
journey and not execute the `ask[A]` in the event that the predicate
returns `false`. 

```tut
import scala.concurrent._, duration._
implicit val ec = ExecutionContext.global

Await.result(
  Future{Thread.sleep(2000); 12} when false,
  1.second)
```

`when` can also be used with an `F[Boolean]`, in which case it behaves
as expected. 

```
Await.result(
  Future{Thread.sleep(2000); 12} when false.pure[Future],
  1.second)
```

You may wish to employ `when` in this manner in order to write 

```
ask[Person]("person") when ask[Boolean]("add-person")
```

instead of 

```
for {
  add    <- ask[Boolean]("add-person")
  person <- ask[Person]("person") when add 
} yield person
```

## emptyUnless

 Similar to `when` is `emptyUnless`, this however only works if the
datatype you are asking for is a `Monoid`, in which case it will give
`empty` should the user answer no. The return datatype is kept the
same as the underlying `ask`. 

For example -

```tut:silent
Await.result(
  Future{Thread.sleep(2000); 12} emptyUnless false,
  1.second)
```

We can apply this in the context of our earlier program in order to
simplify the code -

```tut:silent
def bigSpender2[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    BigSpenderAskTypes
  ]
): F[Option[Person]] = {
  import interpreter._
  for {
    spendAmount <- ask[Int]("spendAmount") emptyUnless
                     ask[Boolean]("spendAny")
    optSpender  <- (
                     ask[String]("name"),
                     ask[Int]("age")
                   ).mapN(Person) when (spendAmount > 100000)
  } yield optSpender
}
```
