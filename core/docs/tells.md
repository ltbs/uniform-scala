---
layout: docs
title: Telling
---

```tut:invisible
import ltbs.uniform._
import cats.implicits._
import scala.language.higherKinds
```

# tells

```
def tell[A](stepId: String, value: A): F[Unit]
```

A call to `tell` represents passing some typed data to the user. The
specifics of exactly how this should be done depends upon the
interpreter that is running the program and in particular how it
chooses to handle that datatype. 

If we call `tell[A]("step-name", presentationData)` we will get an
`F[Unit]` emitted. `presentationData` must be of type `A` and `A` must
be in the types supported by the interpreter.

`"step-name"` in this case is the step
identifier - these are presented to the user but the exact form will
depend upon the interpreter used. It may for example be used to
construct messages telling the user which question they are answering,
for forming URL's or for persistence of data.

Step identifiers should be unique not only to all the `tell`s in the
journey but across all the `ask`s and `interact`s too as otherwise the
interpreters can get confused as they do not know the current place in the
journey.

## Type aliases

In our date-of-birth example we created a type alias called `TellTypes`
which defines the types we are allowed to tell our user.

Given this `TellTypes` definition - 

```tut:silent
type TellTypes = Long :: NilTypes
type AskTypes = NilTypes
```

This code will fail to compile - 

```tut:fail
def badType[F[_] : cats.Monad](
  interpreter: Language[F, TellTypes, AskTypes]
): F[Unit] = {
  interpreter.tell("oh-no", java.time.LocalDate.now)
}
```

The reason here is that we are attempting to present a value of type
`java.time.LocalDate` to the user via a `tell`, but
`java.time.LocalDate` is not in `TellTypes`, only `Long` is.

In order to get this to work we could modify our program to send the
user a Long instead - 

```tut:silent
def goodType[F[_] : cats.Monad](
  interpreter: Language[F, TellTypes, AskTypes]
): F[Unit] = {
  interpreter.tell("tell-a-long", java.time.LocalDate.now.toEpochDay)
}
```

Or we could add a requirement that the interpreter must know how to
present a `java.time.LocalDate` back to the user -

```tut:silent
type TellTypes = java.time.LocalDate :: Long :: NilTypes
```

Because we do not ask the user for a `Long` in the above program we
can remove it from `TellTypes` (though it being there will not prevent
the program from compiling) - 

```tut:silent
type TellTypes = java.time.LocalDate :: NilTypes
```
