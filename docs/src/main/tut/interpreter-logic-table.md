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

For this example we will include a slightly more complex program in
order to test the logic -

```
import org.atnos.eff._
import ltbs.uniform._
import cats.implicits._

type Money = Int

def greasySpoon[S
  : _uniformCore
  : _uniformAsk[Int,?]
  : _uniformAsk[Boolean,?]
] : Eff[S,Money] = for {
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

And we need to import

```
import ltbs.uniform.interpreters.logictable._
```

And we need to define a stack for our program. The logic table
interpreter needs a few monads in its stack, but it defines its own
type for the stack. We need to define the stack for our program and
join it to the stack needed by the logic table interpreter -

```
type GreasyStack = Fx2[UniformAsk[Int,?], UniformAsk[Boolean,?]]
type FullStack = FxAppend[GreasyStack, LogicTableStack]
```

## Providing sample input

```
import org.atnos.eff.syntax.all._

val output = greasySpoon[FullStack].
  giveExamples(List(true,false)).
  giveExamples{
    case "age" => List(10,50,100)
    case _     => List(1,2,3)
  }.
  runState(UniformCore()).
  runEither.runWriter.runList.run
```
