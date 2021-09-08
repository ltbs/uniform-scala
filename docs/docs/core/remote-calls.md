---
layout: docs
title: Remote Calls
---

# Remote Calls 

Sometimes it is necessary to perform some sort of out-of-band
interaction during a journey, for example it might be that you need
the user to input a code and you need to call an API to look up a
value based upon that code. 

To illustrate this with an example let us take the journey to
calculate the number of days a person has been alive from earlier - 

```scala

import ltbs.uniform._

import java.time._, format._

def dateOfBirth = for {
  dateOfBirth <- ask[LocalDate]("date-of-birth")
  daysAlive = LocalDate.now.toEpochDay - dateOfBirth.toEpochDay
  _           <- tell[Long]("days-alive", daysAlive)
} yield dateOfBirth.format(
  DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL)
)
```

If we wanted to modify this such that the calculation for `daysAlive` is
done via some remote process we can instead define a 'server' like so - 

```scala
import scala.concurrent.Future

trait Server {
    def calculate(dob: LocalDate): Future[Long]
}
```

We can now pass the server as a parameter into the function. 
In order to adapt the `Future` to whatever type we end up
interpreting to we can use the `convert` method - 

```scala
def dateOfBirthRemote(server: Server) = for {
  dateOfBirth <- ask[LocalDate]("date-of-birth")
  daysAlive   <- convert(server.calculate(dateOfBirth))
  _           <- tell[Long]("days-alive", daysAlive)
} yield dateOfBirth.format(
  DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL)
)
```

In this case we now call our server in our program. We must
now provide a server instance when calling `dateOfBirthRemote`

# Converters

Our interpreter will need to know how to handle the conversion from a
`Future` (in the case of our example) to whatever higher-kinded type
the interpreter uses. 

For example, suppose our interpreter targets the `Option`
higher-kinded type. We would need to
show how to convert a `Future[LocalDate]` into a `Option[LocalDate]`. 
Many interpreters will ship with support for common datatypes (for
example the generic web interpreter provides support for converting
`Future` into `WebMonad`).

If you have to provide your own conversion method, there are three
main ways to do it. 

## Using a natural transformation

The most generic way to show the interpreter how to handle the
conversion is to provide a natural transformation (for example 
`Future ~> Option`) in implicit scope.

In this instance the same logic will be applied for converting a
`Future[LocalDate]` to a `Option[LocalDate]` as would be used to convert a
`Future[Customer]` to a `Option[Customer]`

```scala
import cats.~>
implicit val converterOne = new (Future ~> Option) { 
	def apply[A](in: Future[A]): Option[A] = ???
}
```

Once this instance is in scope you will be able to interpret any
journey that uses `convert` from a `Future` with an interpreter
targeting `Option`.

## Using a function

This method provides a more fine-grained control than using natural
transformations. For example if we wanted to use different logic for
converting a `LocalDate` as for a `Customer`.

For this situation we instead define an implicit function from one
type to the other.

```scala

case class Customer(name: String, age: Int)

implicit def f: Function[Future[LocalDate],Option[LocalDate]] = ???
implicit def f2: Function[Future[Customer],Option[Customer]] = ???
```

## Using a keyed and typed converter 

This approach provides the most flexibility but is the least
generic. In this instance we can control the logic for the conversion
not only based upon types but also based upon the step ID. 

In order to get a step ID we need to provide one in our journey using
the `convertWithKey` function - 

```scala
def dateOfBirthRemoteStepped(server: Server) = for {
  dateOfBirth <- ask[LocalDate]("date-of-birth")
  daysAlive   <- convertWithKey("remote-call-dob")(
                   server.calculate(dateOfBirth)
                 )
  _           <- tell[Long]("days-alive", daysAlive)
} yield dateOfBirth.format(
  DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL)
)
```

We can now implement a `Converter`, and use the step ID to discern
between "remote-call-dob" and any other similar call.

```scala
implicit val converter = new Converter[Future, Option, LocalDate] {
  def apply(
    key: String, 
	in: () => Future[LocalDate]
  ): Option[LocalDate] = key match { 
    case "remote-call-dob" => ???
    case _                 => ???
  }
}
```

We often use this third approach when we need to encode and cache 
the result (for example with web interpreters).

For many interpreters this may be sufficient for production. However
care must be taken when using web-based interpreters as they may need
to memoise the result to avoid hammering the server. For example the
Play Framework Interpreter will replay the logic on every page load -
generally this is desirable but a `Server[WebMonad]` that makes a
remote call and wraps and returns the result would hit the server on every step after that point in the program unless memoisation is used.

In the example of the Play Interpreter Uniform provides special
methods to support memoising the result within the play interpreters
internal data structure. 
