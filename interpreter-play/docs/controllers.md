---
layout: docs
title: Controllers
---

# Controllers

If you have a controller you wish to use uniform with, you must first
extend the `PlayInterpreter[Html]` where `Html` is some play writeable type
(for our example we'll use `play.twirl.api.Html`, but you could use
[ScalaTags](http://www.lihaoyi.com/scalatags/) or any other representation you wish)

There are a few things we will need to configure before we can use our
controller.
## Imports

```tut:silent
import ltbs.uniform._, interpreters.playframework._

import play.api.i18n.{Messages ⇒ _, _}
import play.api.mvc._
import play.twirl.api.{Html, HtmlFormat}
```

## Messages

We need to tell Uniform where to get messages (i18n messages)
from. Because Uniform is interpreter-agnostic it does not use Play
messages automatically as you may wish to share messages between
different interpreters.

For now we'll just use the 'best guess' messages. We'll create a
function to populate our messages like so -

```tut:silent
  def messages(
    request: Request[AnyContent],
    customContent: Map[String,(String, List[Any])]
  ): UniformMessages[Html] =
    UniformMessages.bestGuess.map(HtmlFormat.escape)
```

## Page Chrome

The term 'chrome' is used here to mean all HTML on the page
surrounding the form.

We need to tell the interpreter what chrome to use. For our Twirl
based example we'd probably want to defer to a view, but for our
simple example we'll just leave it undefined for now.

```tut:silent
  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    tell: Html,
    ask: Html,
    breadcrumbs: Path,
    request: Request[AnyContent],
    messages: UniformMessages[Html]
  ): Html = ???
```

## Selection of fields

Sometimes we may want to offer the user a choice between several
fields or sets of fields. For example an `Either[String,Int]` or a
sealed trait hierarchy. In this situation you may want to use
Javacript to control hiding and revealing form elements, perhaps
toggled by radio buttons.

The `selectionOfFields` method takes a list of options and a function
to control the Html that should be presented when that option is
active.

```tut:silent
  def selectionOfFields(
    inner: List[(
      String,
      (List[String], Path, Option[Input], ErrorTree, UniformMessages[Html]) ⇒ Html
    )]
  )(
    key: List[String],
    path: Path,
    values: Option[Input],
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html = ???
```

## Invoking a journey

We also need an actual action that will invoke the journey.

```scala
def myjourney(targetId: String) =
  Action.async { implicit request: Request[AnyContent] ⇒

    // interpret our journey using the play interpreter
    val playJourney = myJourney(
      new FuturePlayInterpreter[TellTypes, AskTypes],
    )

    // Run the journey using the request and the targetId
    run(playJourney, targetId){ output ⇒
        // final code to be run upon successful completion
        // of the journey
        println(output)
      Future.successful(Ok("Completed"))
    }
  }
```

In our routes file we would write something like this -

```
GET         /myjourney/           controllers.ExampleController.myjourney(id = "")
GET         /myjourney/*id        controllers.ExampleController.myjourney(id: String)
POST        /myjourney/           controllers.ExampleController.myjourney(id = "")
POST        /myjourney/*id        controllers.ExampleController.myjourney(id: String)
```

## Putting it all together

Combining these elements we get an outline of our interpreter setup

```scala
import ltbs.uniform._, interpreters.playframework._

import play.api.i18n.{Messages ⇒ _, _}
import play.api.mvc._
import play.twirl.api.{Html, HtmlFormat}

class ExampleController (
  implicit val messagesApi: MessagesApi,
  ec: concurrent.ExecutionContext
) extends PlayInterpreter[Html] with I18nSupport {

  def messages(
    request: Request[AnyContent],
    customContent: Map[String,(String, List[Any])]
  ): UniformMessages[Html] =
    UniformMessages.bestGuess.map(HtmlFormat.escape)

  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    tell: Html,
    ask: Html,
    breadcrumbs: Path,
    request: Request[AnyContent],
    messages: UniformMessages[Html]
  ): Html = ???

  def selectionOfFields(
    inner: List[(
      String,
      (List[String], Path, Option[Input], ErrorTree, UniformMessages[Html]) ⇒ Html
    )]
  )(
    key: List[String],
    path: Path,
    values: Option[Input],
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html = ???

  def myjourney(targetId: String) =
    Action.async { implicit request: Request[AnyContent] ⇒

      // interpret our journey using the play interpreter
      val playJourney = myJourney(
        new FuturePlayInterpreter[TellTypes, AskTypes],
      )

      // Run the journey using the request and the targetId
      run(playJourney, targetId){ output ⇒
        // final code to be run upon successful completion
        // of the journey
        println(output)
        Future.successful(Ok("Completed"))
      }
    }
}
```
