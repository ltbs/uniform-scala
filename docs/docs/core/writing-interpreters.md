---
layout: docs
title: Writing Interpreters
---

# Writing Interpreters

So far we have discussed how to consume existing interpreters, this
section is meant to serve as a guide as to how to write your own interpreter.

This is not necessary (or recommended) for day-to-day usage of Uniform
but it opens up lots of possibilities should you have somewhat unusual
requirements.

## Unnatural Transformations

Uniform journeys are essentially tagless final programs with one
important twist - the methods accept type parameters. As such we
can call `ask[Int]` rather than needing `askInt`, `askString`, etc for
every possible datatype we might want to consume.

If we did have hundreds of `askN` methods one for every
possible datatype our interpreters would need to provide an
implementation of every datatype we wanted, we would need to
update the uniform language itself if we wanted to add a new type and
therefore every interpreter (if all other users wanted to use your new
datatype or not).

Using `ask[T]: UF[T]` presents its own problems however - when using tagless
final we transform our `UF[T]` into our desired datatype,
`WebMonad[T]` perhaps. `UF ~> WebMonad` is therefore the natural
transformation that is being applied.

But natural transformations are total - for every possible `A` we must
be able to convert `UF[A]` into `WebMonad[A]`. Therefore the only methods
we would be able to invoke for `A` are ones that belong to `Any` such
as `toString`.

Ordinarily we would add a typeclass to represent the support for a
given datatype, for example `ask[A: WebMonadSupport]` but then our
typeclasses are bound to the journey itself at compile time rather
than being specific to a given interpreter.

We need something that is _not quite_ a natural transformation. Our
journey should be able to refer to any datatype it needs but the
typeclass instance should be owned and retrieved by the interpreter.

The journey must declare the types it uses, and the interpreter must
provide support for each datatype in the journey it interprets by way
of a typeclass. The journey must know nothing of the typeclasses used
by any given interpreter.

I found this to be a surprisingly difficult problem to solve, and
several techniques were explored in different versions of uniform.

The four approaches I found were - 

1. use the Eff monad 
   (effective but makes the syntax much more convoluted)
2. using runtime reflection 
   (prevents ScalaJS from working correctly)
3. using hetrogenous lists of typeclass instances 
   (works well, but leads to a complex syntax) 
4. using macros together with tags to inhibit type erasure
   (the current approach)

## Monad Interpreters 

The quickest and easiest type of interpreter to create is one based
off of monads. The interpreter will chain together the steps of the
journey into instances of your given datatype and then use `flatMap`
to bind them into each other.

When taking this approach you only need to specify how to map
interact, etc onto your chosen type. 

```scala

import ltbs.uniform._
import ltbs.uniform.validation.Rule
import cats.{Id, Monoid, Monad}

// we need to know how to tell and ask together
// we don't care about tell, and our ask is a Monoid
// so we just wrap the Monoid instance in a type
// with two parameters
case class Zero[T, A](monoid: Monoid[A]) { 
  def value = monoid.empty
}

// wherever there is an implicit Monoid we want an
// implicit Zero instance. 
implicit def zeroInstance[T,A: Monoid] = Zero[T,A](implicitly)

object ZeroInterpreter extends MonadInterpreter[Id, Zero, Noop] { 
  def monadInstance = implicitly[Monad[Id]]
  
  override def interactImpl[T, A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String, List[Any])],
    interaction: Zero[T,A]
  ): Id[A] = interaction.value

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => Id[A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String,List[Any])],
    asker: Noop[A]
  ): Id[List[A]] = Nil
}
```

Lets create a simple journey in order to test our interpreter -

```scala
val journey = for {
  a <- ask[String]("a")
  c <- ask[Int]("c")
} yield (a,c)
```

We can now process the journey through the interpreter - 

```scala
// we want an implicit instance of Monoid for String and Int
import cats.implicits._  

// run the journey using the interpreter
ZeroInterpreter.interpret(journey)
// res0: (String, Int) = ("", 0)
```
