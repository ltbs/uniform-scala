---
layout: docs
title: Writing a program
position: 2
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
def intOnlyProgram[Stack : _uniform[Int,?]] = 
  uask[Stack,Int]("favouriteNumber")
```

This represents prompting the user for a value - in this case an
`Int`. 

Optional fields are treated as you would expect, as are lists - 

```tut:silent
def intListProgram[S : _uniform[List[Int],?]] = 
  uask[S,List[Int]]("favouriteNumbers")

def stringOptProgram[S : _uniform[Option[String],?]] = 
  uask[S,Option[String]]("optionalDescription")
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
def pizzaProgram[S : _uniform[Int, ?]]: Eff[S, Pizza] = for {
  size     <- uask[S,Int]("size")
  toppings <- uask[S,List[String]]("toppings")
  base     <- uask[S,Int]("base")
} yield Pizza(size, toppings, base)
```

This error message is telling us that we are trying to create a
program that needs to be able to ask the user for `Int`'s and
`List[String]`'s, but we want it to be executable by interpreters that
don't need to know how to ask the user for a `List[String]`. Obviously
this cannot work, so we must add `_uniform[List[String], ?]` to our
stack -

```tut:silent
def pizzaProgram[S : _uniform[Int, ?] : _uniform[List[String], ?]]: Eff[S, Pizza] = for {
  size     <- uask[S,Int]("size")
  toppings <- uask[S,List[String]]("toppings")
  base     <- uask[S,Int]("base")
} yield Pizza(size, toppings, base)
```

Alternatively we can use the [Cats](https://typelevel.org/cats/)
library and take advantage of the applicative syntax for something a
bit terser -

```tut:silent
import cats.implicits._

def pizzaProgram2[S : _uniform[Int, ?] : _uniform[List[String], ?]]: Eff[S, Pizza] = 
(
  uask[S,Int]("size"), 
  uask[S,List[String]]("toppings"),
  uask[S,Int]("base")
).mapN(Pizza)
```

## Selection functions (`uaskOneOf` and `uaskNOf`)

Sometimes we only want to offer the user a subset of the possible
values contained within a datatype to choose from. For this we have
two dedicated functions -

```tut:silent
def pizzaProgram3[S : _uniformSelect[Int, ?] : _uniformSelect[String, ?] : _uniform[Int,?]]: Eff[S, Pizza] = 
(
  uaskOneOf[S,Int]("size", Set(1,2,3)), 
  uaskNOf[S, String]("toppings", Set("olives", "capers", "pineapple")).map(_.toList),
  uask[S,Int]("base")
).mapN(Pizza)
```

Notice that we have a separate `_uniform[Int,?]` and
`_uniformSelect[Int,?]` in the type signature - they are not the same 
thing because the interpreter will probably want to ask the user for
these values in a different way.

## Branching Logic

If you want to conditionally ask a question there are a number of ways
to do it. The most flexible is to use the normal scala `if` or `case`
statements
-

```tut:silent
def conditionalProgram[S : _uniformSelect[Int, ?] : _uniform[String, ?]]: Eff[S, (Int,String)] =
for {
  size     <- uaskOneOf[S,Int]("size", Set(1,2,3))
  discount <- if (size == 3) uask[S,String]("discountCode")
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
def conditionalProgram2[S : _uniformSelect[Int, ?] : _uniform[String, ?]]: Eff[S, (Int,Option[String])] =
for {
  size     <- uaskOneOf[S,Int]("size", Set(1,2,3))
  discount <- uask[S,String]("discountCode") when (size == 3)
} yield (size, discount)
```

This is similar to the previous example, except that it returns an
`Option[String]` rather than a string. The argument to `when` can be
either a normal predicate, or another uniform journey provided that that
journey returns a `Boolean`. For example if we wanted to first ask the
user if they have a discount code and then only if they say yes to
prompt them for the code itself -

```tut
def discountProgram[S : _uniform[Boolean, ?] : _uniform[String, ?]]: Eff[S, Option[String]] =
  uask[S,String]("discountCode") when uask[S,Boolean]("haveDiscountCode")
```

## Monoid fields (`emptyUnless`)

If we wanted to keep the previous behaviour of returning an empty
string in the event of the user ordering a small pizza there is
likewise a number of ways we can achieve this. A clumbsy option would be to use
the `map` function -

```
def discountProgram2[S : _uniform[Boolean, ?] : _uniform[String, ?]]: Eff[S, String] =
  (uask[S,String]("discountCode") when uask[S,Boolean]("haveDiscountCode"))
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
def discountProgram3[S : _uniform[Boolean, ?] : _uniform[String, ?]]: Eff[S, String] =
  uask[S,String]("discountCode") emptyUnless uask[S,Boolean]("haveDiscountCode")
```

Otherwise `emptyUnless` works identically to `when` except that it
returns an `A` rather than an `Option[A]`.

