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

```scala
import ltbs.uniform._
```

We're going to use the old `java.time.LocalDate` in our program
too along with some code for formatting. This is specific to our
example here and not likely needed for your project.

```scala
import java.time._, format._
```

## Journey definition

Now we can create our main journey body.

```scala
def dateOfBirth = for {
  dateOfBirth <- ask[LocalDate]("date-of-birth")
  daysAlive   = LocalDate.now.toEpochDay - dateOfBirth.toEpochDay
   _          <- tell[Long]("days-alive", daysAlive)
} yield dateOfBirth.format(
  DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL)
)
```

Notice that this is very similar to a tagless final program. 

The journey itself defines the user-journey or interaction you wish to
model. It is completely abstract at this point and the interpreter
gets to decide how to represent the questions.

TODO: Why do we need to import cats or higher kinds here?
