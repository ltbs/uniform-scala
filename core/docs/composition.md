---
layout: docs
title: Composing journeys
---

# Composing journeys together

It is easy to create subjourneys and compose them into a larger
journey provided you are using the same interpreter.

Lets start with an example that collects two `Person` records from a
user -

```tut:silent
import ltbs.uniform._
import cats.implicits._
import scala.language.higherKinds

case class Person(name: String, age: Int)

type AskTypes1 = Person :: NilTypes

def senderAndReceiver1[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    AskTypes1
  ]
): F[(Person, Person)] = {
  import interpreter._

  for {
    sender   <- ask[Person]("sender")
    receiver <- ask[Person]("receiver")
  } yield (sender, receiver)
}
```

This is fine, but if we wanted to ask the user for the name and age
separately we would have some duplication in our code -

```tut:silent
type AskTypes2 = String :: Int :: NilTypes

def senderAndReceiver2[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    AskTypes2
  ]
): F[(Person, Person)] = {
  import interpreter._

  for {
    senderName   <- ask[String]("sender-name")
    senderAge    <- ask[Int]("sender-age")
    receiverName <- ask[String]("receiver-name")
    receiverAge  <- ask[Int]("receiver-age")
  } yield (
    Person(senderName, senderAge),
    Person(receiverName, receiverAge)
  )
}
```

We can separate out the collection of the person record into a local
function -

```tut:silent
def senderAndReceiver3[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    AskTypes2
  ]
): F[(Person, Person)] = {
  import interpreter._

  def askPerson(id: String): F[Person] = for {
    name <- ask[String](s"$id-name")
    age  <- ask[Int](s"$id-age")
  } yield Person (name, age)

  for {
    sender   <- askPerson("sender")
    receiver <- askPerson("receiver")
  } yield (sender, receiver)
}
```
