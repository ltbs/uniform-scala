---
layout: docs
title: Writing a program
position: 1
---

# Imports

Uniform is built upon *monad stacks*, and uses the [Eff Monad
Library](https://github.com/atnos-org/eff) to do the heavy
lifting. When writing a program you need to import this library -

```tut:silent
import org.atnos.eff._
```

You will also need to import the core uniform library. You do not need
to import any interpreters when merely defining the program.

```tut:silent
import ltbs.uniform._
```

# Writing a program

The program itself defines the user-journey or interaction you wish to
model. It is completely abstract at this point and the interpreter
gets to decide how to represent the questions.

## Ask function (`uask`)

The most basic interaction with the user is the `uask` function - it
needs a name and a data type -

```
uask[Stack,Int]("favouriteNumber")
```

However this code alone will fail -

```tut:fail
uask[Stack,Int]("favouriteNumber")
```

In order to run our program must be
applied to a monad stack. And this
stack must know about every data-type you will ask the users.

```tut:silent
def intOnlyProgram[Stack : _uniformCore : _uniformAsk[Int,?]] =
  ask[Int]("favouriteNumber")
```

This represents prompting the user for a value - in this case an
`Int`.

Optional fields are treated as you would expect, as are lists -

```tut:silent
def intListProgram[S : _uniformCore : _uniformAsk[List[Int],?]] =
  ask[List[Int]]("favouriteNumbers")

def stringOptProgram[S : _uniformCore : _uniformAsk[Option[String],?]] =
  ask[Option[String]]("optionalDescription")
```

## Composition

Each statement in the program (such as `uask`) is monadic - you can
compose the questions inside a for comprehension, assign them to
variables and even build big programs from smaller programs.

Suppose we have a case class called `Pizza`

```tut:silent
case class Pizza(size: Int, toppings: Iterable[String], base: Int)
```

We could then write a program to collect the values, it has a return
type of `Eff[S, Pizza]`. We must however be careful to include *all*
the values you want to include in the stack -

```tut:fail
def pizzaProgram[S
      : _uniformCore
	  : _uniformAsk[Int]
    ]: Eff[S, Pizza] = for
{
  size     <- ask[Int]("size")
  toppings <- ask[List[String]]("toppings")
  base     <- ask[Int]("base")
} yield Pizza(size, toppings, base)
```

This error message is telling us that we are trying to create a
program that needs to be able to ask the user for `Int`'s and
`List[String]`'s, but we want it to be executable by interpreters that
don't need to know how to ask the user for a `List[String]`. Obviously
this cannot work, so we must add `_uniform[List[String], ?]` to our
stack -

```tut:silent
def pizzaProgram[S
      : _uniformCore
	  : _uniformAsk[Int,?]
	  : _uniformAsk[List[String],?]
    ]: Eff[S, Pizza] = for
{
  size     <- ask[Int]("size")
  toppings <- ask[List[String]]("toppings")
  base     <- ask[Int]("base")
} yield Pizza(size, toppings, base)
```

Alternatively we can use the [Cats](https://typelevel.org/cats/)
library and take advantage of the applicative syntax for something a
bit terser. You will however need to modify the syntax slightly, the following
will not work -

```tut:fail
def pizzaProgram2[S
  : _uniformCore
  : _uniformAsk[Int, ?]
  : _uniformAsk[List[String], ?]
]: Eff[S, Pizza] = (
  ask[Int]("size"),
  ask[List[String]]("toppings"),
  ask[Int]("base")
).mapN(Pizza)
```

In this case because we are constructing a tuple first
of all the scala type inference does not know what stack you want the uniform
interactions to be part of. You can help it by calling `.in[MYSTACK]` on
the uniform instruction -

```tut:silent
import cats.implicits._

def pizzaProgram2[S
  : _uniformCore
  : _uniformAsk[Int, ?]
  : _uniformAsk[List[String], ?]
]: Eff[S, Pizza] = (
  ask[Int]("size").in[S],
  ask[List[String]]("toppings").in[S],
  ask[Int]("base").in[S]
).mapN(Pizza)
```

## Branching Logic

If you want to conditionally ask a question there are a number of ways
to do it. The most flexible is to use the normal scala `if` or `case`
statements
-

```tut:silent
def conditionalProgram[S
  : _uniformCore
  : _uniformAsk[Int, ?]
  : _uniformAsk[String, ?]
]: Eff[S, (Int,String)] = for {
  size     <- ask[Int]("size")
  discount <- if (size >= 3) ask[String]("discountCode").in[S]
              else           Eff.pure[S,String]("")
} yield (size, discount)
```

In this example the user will only be prompted for a discount code if
they have asked for a large pizza, if they do not provide one the
discount code will be set to the empty string (`""`).

## Optional fields (`when`)

There are two functions you can use to simplify this. The first is
`when` and is used as follows -

```tut
def conditionalProgram2[S
  : _uniformCore
  : _uniformAsk[Int, ?]
  : _uniformAsk[String, ?]
]: Eff[S, (Int,Option[String])] = for {
  size     <- ask[Int]("size")
  discount <- ask[String]("discountCode").in[S] when (size >= 3)
} yield (size, discount)
```

This is similar to the previous example, except that it returns an
`Option[String]` rather than a string. The argument to `when` can be
either a normal predicate, or another uniform journey provided that that
journey returns a `Boolean`. For example if we wanted to first ask the
user if they have a discount code and then only if they say yes to
prompt them for the code itself -

```tut
def discountProgram[S
  : _uniformCore
  : _uniformAsk[Boolean, ?]
  : _uniformAsk[String, ?]
]: Eff[S, Option[String]] =
  ask[String]("discountCode").in[S] when ask[Boolean]("haveDiscountCode")
```

## Monoid fields (`emptyUnless`)

If we wanted to keep the previous behaviour of returning an empty
string in the event of the user ordering a small pizza there is
likewise a number of ways we can achieve this. A clumbsy option would be to use
the `map` function -

```
def discountProgram2[S
  : _uniformCore
  : _uniformAsk[Boolean, ?]
  : _uniformAsk[String, ?]
]: Eff[S, String] =
  (ask[String]("discountCode") when ask[Boolean]("haveDiscountCode"))
    .map(_.getOrElse(""))
```

This works because unform journeys are monadic and all monads are also
functors. Because functors provide `map` as long as you have a
journey of type `Uniform[A]` and a function of type `A ⇒ B` you can
therefore produce a `Uniform[B]`.

However there is a convenience method for this too. If you have a
`Uniform[A]` and you know that `A` is a Monoid you can use the
`emptyUnless` function.

```tut
def discountProgram3[S
  : _uniformCore
  : _uniformAsk[Boolean, ?]
  : _uniformAsk[String, ?]
]: Eff[S, String] =
  ask[String]("discountCode") emptyUnless ask[Boolean]("haveDiscountCode")
```

Otherwise `emptyUnless` works identically to `when` except that it
returns an `A` rather than an `Option[A]`.
