---
layout: docs
title: Web interpreters 
---

We can convert our journey into a web interaction using one of the web
interpreters. 

The web interpreters all use a common `GenericWebInterpreter` behind
the scenes. This interpreter takes an input (`PageIn`) and
returns an outcome which could be a representation one of the following - 

- `GotoPath` - redirect to a new part of the journey
- `Payload` - present a new page to the user
- `Success` - conclude the journey giving a result value

(This is of course a bit of a simplification, for example it tracks
state, position in the journey allowing backtracking, etc). 

A normal HTTP server interpreter (such as the Play interpreter) would
wrap `GenericWebInterpreter`, mapping the input from a HTTP post or
the request URL into a format that the `GenericWebInterpreter` can
understand. Likewise it would take the output and (for example)
convert a `GotoPath` result into a HTTP 301 redirection. 

The scalaJS interpreter does the same thing but instead of reading a
form Post payload and returning a HTTP response would read the form
directly from the page (calling the `serialize()` method) and render
the result by altering the DOM to dynamically redraw the form.

The GenericWebInterpreter has a rendering type. If you wanted to use
twirl for rendering forms and views this type might be `Html`. If you
wanted to use scalatags it might be `Tag` or `Frag`. 

For the examples in this chapter we will be using the scalajs
interpreter as this allows me to show you the effect of our actions
directly on the page without any need for setting up a server, but
the same techniques are portable across to anything that implements
`GenericWebInterpreter`. Indeed you can use more than one - for
example the scalajs interpreter could be used for design prototypes
and the play interpreter used for production. If you do use several
interpreters you can benefit from sharing the assets between them -
for example you can design a form for inputting an address data type
and use it in both the JS prototype and the production version.

Lets start by taking our simple journey from the previous chapter - 
```scala mdoc:js:shared
import scala.language.higherKinds

import ltbs.uniform._
import cats.implicits._
import java.time._, format._

type TellTypes = NilTypes
type AskTypes = String :: NilTypes

def simpleJourney[F[_] : cats.Monad](
  interpreter: Language[F, TellTypes, AskTypes]
): F[(String, String)] = {
  import interpreter._

  for {
    forename <- ask[String]("forename")
    surname <- ask[String]("surname")	
  } yield (forename, surname)
}
```

Next we need a usable JS interpreter. We'll use the scalatags `Tag`
datatype and our interpreter must know how to handle all ask and tell
types.

```scala mdoc:js:shared
import ltbs.uniform._, interpreters.js._, common.web._
import scalatags.JsDom.all._
import scala.concurrent._
import org.querki.jquery._

implicit val ec: ExecutionContext = ExecutionContext.global

trait InterpreterFactory extends JsInterpreter[Tag] {

    implicit val tellUnit = new WebTell[Unit] {
      def render(in: Unit, key: String, messages: UniformMessages[Tag]): Tag = span("")
    }


  implicit val unitField = new FormField[Unit,Tag] {
    def decode(out: Input): Either[ltbs.uniform.ErrorTree,Unit] = Right(())
    def encode(in: Unit): Input = Input.empty
    def render(
      key: List[String],
      path: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Tag]
    ): Tag = span(cls:="unit")("")

  }

  implicit val stringField = new FormField[String,Tag] {
    def decode(out: Input): Either[ErrorTree,String] = out.toStringField().toEither
    def encode(in: String): Input = Input.one(List(in))

    def render(
      key: List[String],
      path: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Tag]
    ): Tag = {
      val existingValue: String = data.valueAtRoot.flatMap{_.headOption}.getOrElse("")
      input(
        id    := key.mkString("_"),
        name  := key.mkString("."),
        value := existingValue
      )
    }
  }



	def renderFrame(
      key: List[String],
      frame: JQuery,
      tell: Tag,
      ask: Tag,
      breadcrumbs: Breadcrumbs, 
      errors: ErrorTree,
      messages: UniformMessages[Tag]
    ): Future[Unit] = Future { 
	  frame.html(tell.toString + ask.toString ) 
	}
}
```

We can now create a new interpreter for our desired datatypes - 

```

object OF extends InterpreterFactory {

implicit def formToWebMonad[A](
  implicit ff: FormField[A, Tag]
): WMC[A] = PostAndGetPage(ff)

val i = create[TellTypes, AskTypes](UniformMessages.echo.map{span(_)})

val j = simpleJourney[WM](i)

def runner = {
  new JsRunner[(String, String)](
    j,
    $("#uniform"),
	diagnostics = true
  ){name => 
    Future(())
  }
}

}


```

```scala mdoc:js:shared

import scala.scalajs._, js.annotation.JSExportTopLevel

object BeardTaxApp {

  val interpreter = new JsInterpreter[Tag] with InferFormFieldProduct[Tag] with InferFormFieldCoProduct[Tag] with examples.Widgets {

    implicit val tellTwirlUnit = new WebTell[Unit] {
      def render(in: Unit, key: String, messages: UniformMessages[Tag]): Tag = span("")
    }

    def renderFrame(
      key: List[String],
      frame: JQuery,
      tell: Tag,
      ask: Tag,
      breadcrumbs: Breadcrumbs, 
      errors: ErrorTree,
      messages: UniformMessages[Tag]
    ): Future[Unit] = Future {

      $(".govuk-heading-xl").html(messages(key.mkString(".")).toString)

      if (errors.nonEmpty) {
        $(".govuk-error-summary").replaceWith(errorSummary(key, errors, messages).toString)
        $(".govuk-error-summary").show()
      } else {
        $(".govuk-error-summary").html("")        
        $(".govuk-error-summary").hide()
      }

      breadcrumbs.drop(1).headOption match {
        case Some(link) => 
          $(".govuk-back-link").html(messages({link :+ "back"}.mkString(".")).toString)
          $(".govuk-back-link").show()
        case _ =>
          $(".govuk-back-link").html("")
          $(".govuk-back-link").hide()
          
      }

      if (errors.nonEmpty) {
        $(".govuk-error-summary").replaceWith(errorSummary(key, errors, messages).toString)
        $(".govuk-error-summary").show()
      } else {
        $(".govuk-error-summary").html("")        
        $(".govuk-error-summary").hide()
      }
      frame.html(tell.toString + ask.toString)
      ()
    }
  }

  import interpreter._

  val i = interpreter.create[TellTypes, AskTypes](UniformMessages.echo.map{span(_)})

  val runner = {
    new interpreter.JsRunner[(String,String)](
    simpleJourney[interpreter.WM](i),
    $("#uniform"),
      diagnostics = true
    )(output => Future($("#uniform").html(output.toString)))
  }

//  @JSExportTopLevel("back")
  def backLink(): Future[Unit] = runner.goBack()

//  @JSExportTopLevel("simplejourneysubmit")
  def submit(): Future[Unit] = runner.submit()

}
```
