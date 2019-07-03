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
`WebMonad[T]` perhaps. `UF ⇝ WebMonad` is therefore the natural
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

The three approaches I found were to use the Eff monad (which is
effective but makes the syntax much more convoluted), using runtime
reflection (which prevents ScalaJS from working correctly) and the
current approach - using hetrogenous lists of typeclass instances
together with custom macros to inhibit type erasure.

Using the latest approach we do need a typeclass on the `ask` method
itself - simplified our language looks like this -

```
import shapeless.HList
import scala.language.higherKinds

trait Language[UF[_], SupportedAsk <: HList]{

  def ask[A](id: String)(
    implicit selectorAsk : IndexOf[SupportedAsk, A]
  ): UF[A]

}
```

The `SupportedAsk` type is a plain shapeless `HList` of supported
types. The interpreter will map this into a `HList` of typeclass
instances - one `TC` for every element of `SupportedAsk`.

The `IndexOf` allows the interpreter to find the index of
the type `A` in `SupportedAsk`, this index is then used to find
`TC[A]` in the mapped hetrogenous list.

Suppose we want an interpreter that returns dummy values - for any
`ask[A]` it should return a canned value of type `A`.

We would create a typeclass like the following -

```tut:silent
trait Example[A] { def value: A }
```

And we would now give some instances -

```tut:silent
implicit val intExample = new Example[Int] { def value: Int = 12 }
implicit val stringExample = new Example[String] { def value: String = "test" }
```

We can now create an interpreter. First we must decide what higher
kinded type our interpreter works with. To keep things simple we're
just going to use the `Id` monad here.

```tut:silent
import ltbs.uniform._

import cats.implicits._
import cats.{Id, Monad}
import shapeless.{Id ⇒ _, _}
import scala.language.higherKinds

class ExampleValuesInterpreter[SupportedTell <: HList, SupportedAsk <: HList](
  implicit askSummoner: TypeclassList[SupportedAsk, Example]
) extends Language[Id, SupportedTell, SupportedAsk] {

  def interact[Tell, Ask](
    id: String,
    tell: Tell,
    default: Option[Ask] = None,
    validation: List[List[Rule[Ask]]] = Nil,
    customContent: Map[String,(String,List[Any])] = Map.empty
  )(
    implicit selectorTell : IndexOf[SupportedTell, Tell],
      selectorAsk : IndexOf[SupportedAsk, Ask]
  ): Id[Ask] = askSummoner.forType[Ask].value
}
```

Because `ask` and `tell` are actually just invoking `interact` we only
need to define that method.

`TypeclassList` is a special construct in uniform, for any `HList`
it will give you a mapped HList of its implicit typeclass
instances. For example if we use `Int` and `String` -

```tut
val summoner = TypeclassList[Int :: String :: HNil, Example]
summoner.forType[Int].value
summoner.forType[String].value
```

We can now create a simple program and test our new interpreter -

```tut:silent
type TellTypes = NilTypes
type AskTypes = Int :: String :: HNil

def program[F[_]: Monad](
  interpreter: Language[F, TellTypes, AskTypes]
): F[(String, Int)] = {
  import interpreter._
  for {
    a ← ask[String]("astring")
    b ← ask[Int]("anint")
  } yield ((a, b + 1))
}
```

```tut
program(
  new ExampleValuesInterpreter[TellTypes, AskTypes]
)
```
