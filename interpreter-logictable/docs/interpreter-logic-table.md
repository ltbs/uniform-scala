---
layout: docs
title: Logic Table Interpreter
---

# Logic Table Interpreter

The logic table interpreter is another very simple interpreter.

Put simply if given a series of values for the questions a user would
be asked it shows the interactions the user would experience and the
final output of the program upon completion.

Despite being simple the logic table interpreter is however quite
useful as can be readily hooked into unit tests or converted into a
spreadsheet to be shown to stakeholders or Business Analysts.

## Starting with a program

For this example we will write a deliberately convoluted program in
order to test the logic -

```tut:silent
import ltbs.uniform._
import cats.implicits._
import scala.language.higherKinds

type Money = Int

type AskTypes = Int :: Boolean :: NilTypes
type TellTypes = NilTypes

def greasySpoon[F[_]: cats.Monad](
  interpreter: Language[F, TellTypes, AskTypes]
) : F[Money] = {
  import interpreter._
  for {
    age   <- ask[Int]("age")
    food  <- ask[Boolean]("wantFood")
    tea   <- ask[Boolean]("wantTea")
    baconCost <- ask[Int]("bacon").map(_ * 12) emptyUnless food
    eggsCost  <- ask[Int]("eggs").map(_ * 24) emptyUnless food
    foodCost = baconCost + eggsCost
    teaCost <- ask[Int]("sugar").map(_ * 10 + 50) emptyUnless tea
    youngDiscount = if (age < 16)
                      teaCost / 10
            else 0
    oldDiscount   = if (age > 60)
                      (teaCost + foodCost) * (Math.min(age - 60,25) / 100)
            else 0
  } yield (foodCost + teaCost + youngDiscount + oldDiscount)
}
```

So we have a program for a fastfood restaurant. The user is first
asked for their age, then if they want food and if they want tea. They
are charged 12 pence per slice of bacon (or 12 pounds if you are in
Shoreditch - I'll leave it to the user to determine the currency), 24p
per egg, 50p for a mug of tea plus 10p per dose of sugar. Finally
young customers get 10% off tea and older customers get 1% off both
food and drink per year over 60 up to a maximum of 25%.

We want to test -

1. The user is asked the correct questions (only being asked
  about food if we say we want food, for example)
2. The calculations are correct

Obviously this is rather silly - normally we would extract the
calculation logic out into a function, but for the sake of
illustrating the testing we will leave the financial code tangled into
our user-interaction program.

## Setup

We need to add an extra import to include the new interpreter -

```
libraryDependencies +=
  "com.luketebbs.uniform" %% "interpreter-logictable" % "{{ site.last-stable-version }}"
```

And we need to import the logictable library -

```tut:silent
import ltbs.uniform.interpreters.logictable._
```

## Providing sample input

We now need to provide some sample data for the interpreter to consume.

```tut:silent
implicit val intSamples = new SampleData[Int] {
  def apply(key: String): List[Int] = key match {
    case "age" => List(10,50,100)
    case _     => List(1,2,3)
  }
}

implicit val booleanSamples = new SampleData[Boolean] {
  def apply(key: String): List[Boolean] =
    List(true, false)
}
```

We can now execute our program with the logic table interpreter -

```tut
val output: List[(List[String], Either[ErrorTree,Int])] = greasySpoon(
  new LogicTableInterpreter[TellTypes, AskTypes]
).value.run
```

The output is a complex type at first glance, but what this represents
is the list of question the user would be asked with the answer they
gave paired with the outcome (either an error or a completed journey
result).

```tut
output.take(5).foreach{ case (messages, outcome) =>
  println(messages.mkString("\n"))
  println(s"   => $outcome")
}
```
