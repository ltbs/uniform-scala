---
layout: docs
title: Writing a journey
position: 2
---

# Writing a basic journey 

To get our feet wet we're going to start with a simple user journey that
does a few things - first it will ask the user for their date of
birth, secondly it will calculate the number of days the user has been
alive and finally it will return their date of birth as a string. 

## Imports

You will need to import the core uniform library, and you will need
cats. You do not need to import any interpreters when merely defining the journey.

```tut:silent
import ltbs.uniform._
import cats.implicits._
```

We will also be using higher-kinded types, here we have enabled this as an import - 

```tut:silent
import scala.language.higherKinds
```

We're going to use the old `java.time.LocalDate` in our program
too along with some code for formatting. This is specific to our
example here and not likely needed for your project.

```tut:silent
import java.time._, format._
```

## Type declarations

Next we have the type declarations for the 'tells' -

```tut:silent
type TellTypes = Long :: NilTypes
```
In this instance we want to be able to present the user with `Long`
values. This means that any interpreter running our journey must know
how to present an `Long` to the user (for example a web-based
interpreter would probably need to know how to render this datatype
into HTML). We'll use this to tell the user the number of days they
have been alive for. 

If our journey needed to be able to present `Address`es, `Name`s and
lists of `Colour`s to the user we would assign `TellTypes` to
`Address :: Name :: List[Colour] :: NilTypes`.

Sometimes we don't need to present anything back to the user in which
case we would just use `NilTypes` on its own.

Next we need a similar type declaration for the 'asks' -

```tut:silent
type AskTypes = LocalDate :: NilTypes
```

Here we are saying that an interpreter must know how to prompt the
user to enter a `java.time.LocalDate`. This will be used to prompt the
user for their date of birth. 

## Journey definition

Now we can create our main journey body.

```tut:silent
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

Notice that this is very similar to a tagless final program - except
that we are taking our `TellTypes` and our `AskTypes` as additional
parameters into our interpreter (telling the interpreter what we
expect it to support), and that the `interpreter.ask` method
accepts a type parameter which controls its output type. 

We created `TellTypes` and `AskTypes` to supply them into our
journey. We could have just inlined these types but generally it is
useful to create type aliases as it can help with debugging and makes
your code a bit clearer.

Notice that we are creating a `String` type in our journey, but we
don't need to have this type declared in either `TellTypes` or
`AskTypes`.

The journey itself defines the user-journey or interaction you wish to
model. It is completely abstract at this point and the interpreter
gets to decide how to represent the questions.
