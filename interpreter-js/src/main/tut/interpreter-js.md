---
layout: docs
title: Static Site Interpreter
---

Sometimes it is useful to have a standalone version of a journey
that doesn't require deployment. We use this for communicating early
designs to stakeholder and for user-testing without the need for a
full deployment environment. 

Using the same program for our journey it is possible to use scalajs
to compile to an entirely client-side implementation that exists
within a single HTML page and simply swaps the content of an element
on the page out, thus creating the illusion of a multi-page journey.  

We need somewhere to keep the state. The simplest place is in a `var`
on the page, like so -

```tut:silent
import cats.implicits._
import cats.Monoid
import ltbs.uniform.prototype.JsInterpreter._

import org.atnos.eff.syntax.all._
import org.atnos.eff._

var state: DB = Monoid[DB].empty
```

`DB` is actually just `Map[String, String]`. 

We need to combine the stacks for the JS Interpreter and our program -

```tut:silent
import ltbs.uniform.sampleprograms.LitreageTest._

type CombinedStack = FxAppend[TestProgramStack, JsStack]
```

We need to define how to handle the data-types we are interested
in. This is similar to what we did with the play interpreter, but
before whereas we needed to write code to extract the data from the
POST request we now need to define how to remove the values from the
DOM. 

```tut:silent
import ltbs.uniform.datapipeline.Tree
import org.querki.jquery.JQuery 

val booleanForm = new Form[Boolean] {

    def render(
      key: String,
      existing: Option[Tree[String,List[String]]],
      errors: ErrorTree
    ): String = ???

    def fromNode(
      key: String,
      fieldSet: JQuery
    ): Either[ErrorTree, Boolean] = ???

    def encode(in: Boolean): Encoded = ??? 

    def decode(out: Encoded): Either[ErrorTree,Boolean] = ???

    def toDataTree(in: Boolean): Tree[String,List[String]] = ???
  }
  
val litresForm = new Form[Litres] { 
    def render(
      key: String,
      existing: Option[Tree[String,List[String]]],
      errors: ErrorTree
    ): String = ???

    def fromNode(
      key: String,
      fieldSet: JQuery
    ): Either[ErrorTree, Litres] = ???

    def encode(in: Litres): Encoded = ??? 

    def decode(out: Encoded): Either[ErrorTree,Litres] = ???

    def toDataTree(in: Litres): Tree[String,List[String]] = ???
  }
```

Here the `render` method defines the HTML that is to be swapped in for a
page, and `fromNode` defines how to extract the data the user has
entered back out again. `encode` and `decode` define how to store the
values in the state and get them back again (`Encoded` is a type alias
for `String`). Finally `toDataTree` specifies how to turn the value
retrieved from the store into something that be placed on the form
(for example if the user goes back to a previous page).

We can now execute our program and write the state back - 

```tut:silent

@scalajs.js.annotation.JSExportTopLevel("runProgram")
def runProgram(action: Action): Unit = {
  val ((result,newState),newBreadcrumbs) = 
    program[FxAppend[TestProgramStack, JsStack]] 
      .useForm(booleanForm)
      .useForm(litresForm)
      .runReader(action)
      .runEither
      .runState(state)
      .runState(List.empty[String])
      .runEval
      .run
	
//  state = newState	
  result match { 
	case Left(htmlPage) => 
	  // the user is still in journey - update the page 
	case Right(value)   => 
	  // the user has completed the journey 
  }
}
```

`Action` can be either `Back` or `Submit`, `Back` being roughly the
same as a GET request and `Submit` being the same as a POST request. 

Each time the user submits a page `runProgram(Submit(pageId))` should
be called, and if the user clicks back (or on a breadcrumb) then
`runProgram(Back(oldPageId))` should be run. 

