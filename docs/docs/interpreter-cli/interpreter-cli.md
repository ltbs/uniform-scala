---
layout: docs
title: CLI Interpreter
---

# CLI Interpreter

Probably the simplest implementation of an interactive user interface
is a text-only, monolingual command-line application.

We're unlikely to use this in production, but it can form an extremely
cheap-but-cheerful way to explain or verify logic or user journeys
with stakeholders in the early stages of a project.

Because of its simplicity it is also a great way to see the
fundamentals of uniform in action.

## Starting with a program

First of all you need a program. Lets start with a simple greeting application -

```scala
import ltbs.uniform._
import cats.implicits._
import scala.language.higherKinds

type TellTypes = NilTypes
type AskTypes = String :: NilTypes

def helloProgram[F[_] : cats.Applicative](
  interpreter: Language[F, TellTypes, AskTypes]
): F[String] = {
  import interpreter._

  (
    ask[String]("forename"),
    ask[String]("surname")
  ).mapN("Hello " + _ + " " + _)
}
```

Notice we are only using a single data type here - `String`. While you
can have as many data-types as you want in your program each
interpreter needs to understand how to handle that data-type before it
can operate on your program.

Generally therefore it's best to start off with just a few basic
data-types and add more specialisation as you go.

Some interpreters (such as the Play interpreter) will infer compound
data-types, but the CLI interpreter does not have this capability at
present.

## Importing the interpreter

We need to add an extra import to include the new interpreter -

```
libraryDependencies +=
  "com.luketebbs.uniform" %% "interpreter-cli" % "{{ site.last-stable-version }}"
```

## Running the program

We need to import the interpreter -

```scala
import ltbs.uniform.interpreters.cli._
```

The program can now be executed using the interpreter.

If invoked `helloProgram` will now prompt the user for a forename,
then a surname and then give the expected greeting.

```scala
def runner = helloProgram(new CliInterpreter[TellTypes, AskTypes])
```
