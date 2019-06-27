---
layout: docs
title: Writing a journey
position: 2
---

# Imports

You will need to import the core uniform library, and you will need
cats. You do not need to import any interpreters when merely defining the journey.



```tut:silent
import ltbs.uniform._
import cats.implicits._
```

We will also be using higher-kinded types - 

```tut:silent
import scala.language.higherKinds
```

# Writing a journey

The journey itself defines the user-journey or interaction you wish to
model. It is completely abstract at this point and the interpreter
gets to decide how to represent the questions.

## Ask function (`ask`)

The most basic interaction with the user is the `ask` function - it
needs a name and a data type -

```
ask[Int]("favouriteNumber")
```

However in order to run this query, there must be an interpreter
available. 

First you must specify the data types you wish to ask of the user and
the data types you wish to present back to the user. 

```tut
type TellTypes = NilTypes
type AskTypes = Int :: NilTypes
```

Here we are defining that our journey needs to ask the user for
integers, and doesn't have any data types which it needs to present to
the user.

This forms a contract between the journey and the interpreter -
the uniform language supports any data type but
attempting to use an interpreter that does not support a given
data-type required by the journey will cause a compilation error. 

Fortunately the interpreters are extensible to support new data-types
and in many cases can infer support for your custom case classes and
other data-types.

Next we can write the actual journey itself -

```tut:silent
def additionJourney[F[_] : cats.Monad](
  interpreter: Language[F, TellTypes, AskTypes]
): F[Int] = {
  import interpreter._

  for {
    a <- ask[Int]("a")
    b <- ask[Int]("b")
  } yield a + b
}
```

Notice that this is very similar to a tagless final program - except
that we are taking our `TellTypes` and our `AskTypes` as additional
parameters into our interpreter (telling the interpreter what we
expect it to support), and that the `interpreter.ask` method
accepts a type parameter which controls it's output type. 

In this case the journey represents asking the user for two `Int`'s
and it returns the sum of the two values (hence the return type is
`F[Int]`.

The names `a` and `b` simply represent normal scala variables inside
the for comprehension and are not visible outside the code.

The strings `"a"` and `"b"` in this case are used as the field
identifiers - these are presented to the user but the exact form will
depend upon the interpreter used. It may for example be used to
construct messages telling the user which question they are answering,
for forming URL's or for persistence of data.

Reusing the variable name would just result in the normal, harmless,
shadowing behaviour. However the field identifiers must be kept unique
for a given journey as otherwise the interpreters can get confused as
they do know the current place in the journey.

======================================== TODO ========================================

## Composition

It is easy to create subjourneys and compose them into a larger
journey provided you are using the same interpreter.

Lets start with an example that collects two `Person` records from a
user - 

```tut:silent
case class Person(name: String, age: Int)

def senderAndReceiver1[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    Person :: NilTypes
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
case class Person(name: String, age: Int)

def senderAndReceiver2[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    String :: Int :: NilTypes
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

def senderAndReceiver2[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    String :: Int :: NilTypes
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

Notice that we are using the 

## Branching

Notice how no questions are dependent upon the result of a previous
question? Perhaps a `cats.Monad` here is overkill and we should use
`cats.Applicative` instead -

```tut:silent
def senderAndReceiverApplicative[F[_] : cats.Applicative](
  interpreter: Language[
    F,
    NilTypes,
    String :: Int :: NilTypes
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
def senderAndReceiver4[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    Boolean :: String :: Int :: NilTypes
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

In this case the journey would ask the user the same 4 questions
initially as `senderAndReceiver2`, however it would then ask the user
for a `Boolean`. If they answer no then the journey would end with
`_3` being `None`. If the user picked yes however then they would be
asked again for a name and age and this time `_3` would be defined
(`Some`).

This could be used for all sorts of branching - you are not confined
to booleans, or to using pattern matching.

```tut:silent
def bigSpender[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    Boolean :: String :: Int :: NilTypes
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

However the specific use-case of asking a `Boolean` to control an
option comes up a lot, so uniform offers a special syntax for it. 

The `when` construct can take either an `F[Boolean]` or a
`Boolean` where `F` is any `Monad`. Similar to `when` is `emptyUnless`
- this however only works if the datatype you are asking for is a
`Monoid`, in which case it will give `empty` should the user answer
no.

For example -

```tut
import scala.concurrent._, duration._
implicit val ec = ExecutionContext.global

Await.result(
  Future{Thread.sleep(2000); 12} when false,
  1.second)

Await.result(
  Future{Thread.sleep(2000); 12} when false.pure[Future],
  1.second)

Await.result(
  Future{Thread.sleep(2000); 12} emptyUnless false,
  1.second)
```

```tut:silent
def bigSpender[F[_] : cats.Monad](
  interpreter: Language[
    F,
    NilTypes,
    Boolean :: String :: Int :: NilTypes
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