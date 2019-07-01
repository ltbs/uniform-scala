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

```
import org.atnos.eff._
import ltbs.uniform._

def helloProgram[S
  : _uniformCore
  : _uniformAsk[String,?]
] : Eff[S,String] = for {
  forename <- ask[String]("forename")
  surname  <- ask[String]("surname")
} yield s"Hello $forename $surname"
```

Notice we are only using a single data type here - `String`. While you
can have as many data-types as you want in your program each
interpreter needs to understand how to handle that data-type before it
can operate on your program.

Generally therefore it's best to start off with just a few basic
data-types and add more specialisation as you go.

## Importing the interpreter

We need to add an extra import to include the new interpreter -

```
libraryDependencies +=
  "com.luketebbs.uniform" %% "interpreter-cli" % "{{ site.last-stable-version }}"
```

## Running the program

We need to import the interpreter -

```
import ltbs.uniform.interpreters.cli._
```

We also need to define a value for the Stack we are using. This will
consist of all the `UniformAsk` and `UniformSelect` usages, plus
possibly some other monads that the interpreter itself needs. Our CLI
interpreter uses the `Eval` monad from the cats library.

```
type Stack = Fx.fx3[
  UniformAsk[String,?],
  cats.Eval,
  cats.data.State[UniformCore,?]
]
```

The program can now be executed using the interpreter.

```
import org.atnos.eff.syntax.all._ // provides runEval and run

def runHelloProgram = helloProgram[Stack].
  using(identity). // String => String
  runState(UniformCore()).
  runEval.
  run
```

If invoked `runHelloProgram` will now prompt the user for a forename,
then a surname and then give the expected greeting.


## Adding new field types

We'll update our program to include a new field type -

```
def helloProgram2[S
  : _uniformCore
  : _uniformAsk[String,?]
  : _uniformAsk[Boolean,?]
] : Eff[S,String] = for {
  forename <- ask[String]("forename")
  surname  <- ask[String]("surname")
  cheese  <- ask[Boolean]("likeCheese")
} yield {
  if (cheese)
    s"Here $forename $surname, have some stilton!"
  else
    s"Hello $forename $surname"
}
```

And add a new stack -

```
type Stack2 = Fx.fx4[
  UniformAsk[Boolean,?],
  UniformAsk[String,?],
  cats.Eval,
  cats.data.State[UniformCore, ?]
]
```

The program is perfectly valid, but our interpreter doesn't know how
to handle a boolean -

```
def runHelloProgram2 = helloProgram2[Stack2].
  using(identity). // String => String
  runState(UniformCore()).
  runEval.
  run
```

We must write a `String ⇒ Boolean` function and provide
it to our interpreter so the interpreter knows how to take the users
input (a `String`) and convert it into a valid `Boolean`.

```
def runHelloProgram2 = helloProgram2[Stack2].
  using(identity).			// String => String
  using(_.toLowerCase.startsWith("y")). // Boolean => String
  runState(UniformCore()).
  runEval.
  run
```
