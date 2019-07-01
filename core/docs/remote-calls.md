---
layout: docs
title: Remote Calls
---

```tut:invisible
import ltbs.uniform._
import cats.implicits._
import scala.language.higherKinds
```

# Remote Calls 

Sometimes it is necessary to perform some sort of out-of-band
interaction during a journey, for example it might be that you need
the user to input a code and you need to call an API to look up a
value based upon that code. 

The preferred way to do this is to write your journey function to
accept an additional parameter alongside the interpreter. 

To illustrate this with an example let us take the journey to
calculate the number of days a person has been alive from earlier - 

```tut:silent

import ltbs.uniform._
import cats.implicits._
import scala.language.higherKinds

import java.time._, format._

type TellTypes = Long :: NilTypes
type AskTypes = LocalDate :: NilTypes

def dateOfBirth[F[_] : cats.Monad](
  interpreter: Language[F, TellTypes, AskTypes]
): F[String] = {
  import interpreter._

  for {
    dateOfBirth <- ask[LocalDate]("date-of-birth")
    daysAlive = LocalDate.now.toEpochDay - dateOfBirth.toEpochDay
    _           <- tell[Long]("days-alive", daysAlive)
  } yield dateOfBirth.format(
    DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL)
  )
}
```

If we wanted to modify this such that the calculation for `daysAlive` is
done via some remote process we can instead define a tagless final
interface like so - 

```tut:silent
trait Server[F[_]] {
    def calculate(dob: LocalDate): F[Long]
}
```

This can now be used alongside `interpreter` - 

```tut:silent
def dateOfBirthRemote[F[_] : cats.Monad](
  interpreter: Language[F, TellTypes, AskTypes], 
  server: Server[F]
): F[String] = {
  import interpreter._

  for {
    dateOfBirth <- ask[LocalDate]("date-of-birth")
    daysAlive   <- server.calculate(dateOfBirth)
    _           <- tell[Long]("days-alive", daysAlive)
  } yield dateOfBirth.format(
    DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL)
  )
}
```

In this case we now call our abstract server in our program. We must
now provide a server instance when calling `dateOfBirthRemote`, and for
testing we can simply generate one automatically for any
`cats.Applicative` (which also includes `cats.Monad` instances)- 

```tut:silent
def testServer[F[_]: cats.Applicative] = new Server[F] { 
    def calculate(dateOfBirth: LocalDate): F[Long] = (
      LocalDate.now.toEpochDay - dateOfBirth.toEpochDay
    ).pure[F]
}
```

We can now invoke a test server for whatever environment we want - 

```tut
testServer[util.Try].calculate(
  LocalDate.now.minusDays(100)
)

testServer[cats.Id].calculate(
  LocalDate.now.minusDays(100)
): Long
```

For many interpreters this may be sufficient for production. However
care must be taken when using web-based interpreters as they may need
to memoise the result to avoid hammering the server. For example the
Play Framework Interpreter will replay the logic on every page load -
generally this is desirable but a `Server[WebMonad]` that makes a
remote call and wraps and returns the result would hit the server on
step after that point in the program unless memoisation is used. 

In the example of the Play Interpreter Uniform provides special
methods to support memoising the result within the play interpreters
internal data structure. 
