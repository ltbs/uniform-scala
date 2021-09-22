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

Lets start with a simple journey like so - 

```scala
import ltbs.uniform._

val journey = for {
  a <- ask[Int]("a")
  b <- interact[String]("b", a)
} yield (b)
```

## Routes file

In order to hook this into a controller we first need to set up our routes file - 

```
GET         /journey/           controllers.SimpleController.simpleAction(id = "")
GET         /journey/*id        controllers.SimpleController.simpleAction(id: String)
POST        /journey/           controllers.SimpleController.simpleAction(id = "")
POST        /journey/*id        controllers.SimpleController.simpleAction(id: String)
```

We need to capture all the GETS and the POST's, and direct them
through to the same controller action. The routing between pages
within the journey is handled by uniform itself. 

## Simple controller

In order for a controller to interpret a uniform journey it mush mix
in the `PlayInterpreter[A]` where `A` is the type you want to use for
HTML rendering (such as `play.twirl.api.Html` or
`scalatags.Text.all.Tag` if you'd prefer to use scalatags). 

A simple example using twirl could look something like this - 

```scala
import cats.implicits._
import ltbs.uniform._, common.web._, interpreters.playframework._, validation._
import play.api.mvc._
import play.twirl.api.Html

// in order to interpret our journey we need to be able to 
// ask the user for an Int (step a), and present the user 
// with an Int while asking them for a String (step b)
implicit def askInt: WebInteraction[Html,Unit,Int] = ???
implicit def interactIntString: WebInteraction[Html,Int,String] = ???

class SimpleController(
  implicit ec: scala.concurrent.ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController with PlayInterpreter[Html] {

  // i18n - we can get messages from several sources and combine them
  // together. 
  // In this example we first use the messages provided by the play 
  // framework then fall back to using the 'best guess' algorithm
  implicit def messages(
    implicit request: Request[AnyContent]
  ): UniformMessages[Html] =
    { messagesApi.preferred(request).convertMessages() |+|
      UniformMessages.bestGuess }.map(Html.apply)

  // How should we 'wrap' the generated form? 
  // If we're using twirl we would likely pass these values into 
  // a regular page template that contains our branding, JS, etc.
  def pageChrome(
    key: List[String], 
    errors: ErrorTree,
    formHtml: Option[Html], 
    breadcrumbs: List[String],
    request: Request[AnyContent], 
    messages: UniformMessages[Html]
  ): Html = ???

  // our main controller method
  def simpleAction(stepId: String) = Action.async {
    implicit request: Request[AnyContent] =>

    // how should we store the data between pages? 
    // In this example we retain the data in the users session.
    implicit val persistence: PersistenceEngine[Request[AnyContent]] =
      SessionPersistence("simple")

    // interpret the journey into a WebMonad
    val wm: WebMonad[Html,String] = interpret(journey)
    
    // run the WebMonad using the stepId and implicit request
    wm.runSync(stepId){ onCompletion: String =>
      // what should we do upon completion of the journey? 
      Ok(s"Done: ${onCompletion}")
    }
  }
}
```

The `PlayInterpreter` trait provides the `interpret` method that will
return a `WebMonad`.

The `WebMonad` can then be run, and will return the `Future[Action]`. 
There are two methods you can use here - `run` or `runSync` (which
allows you to instead return a `Future[Action]` in the block of code
to execute on completion. 

## Internationalisation

We need to tell Uniform where to get messages (i18n messages)
from. Because Uniform is interpreter-agnostic it does not use Play
messages automatically as you may wish to share messages between
different interpreters. 

You can however access the Play Framework messages file as shown
above. If you do not use a fallback as shown you will get an exception
thrown when attempting to access content that is not defined - this
may be desirable (for example when testing all content is translated
to the target language). 

Apart from the ability to convert the play messages file
`UniformMessages` behaves the same in the Play Interpreter as
described in the core.

## WebInteraction

In order for our example to work we will also have to tell our
interpreter how to render forms and encode values. 

Although you can construct a `WebInteraction` directly, typically we
would define a `WebAsk` instance for the basic types (`String`s,
`Int`s, `Boolean`s, etc).

This is covered in more detail in the common code section, but a basic
example for a string datatype is as follows -

```scala
implicit val stringField = new WebAsk[Html,String] {

  // we need to turn serialized Input (a representation 
  // of our POST request) into our desired data type
  def decode(out: Input): Either[ErrorTree,String] = 
    out.toStringField().toEither
    
  // we need to turn our data type back into its serialized
  // format
  def encode(in: String): Input = 
    Input.one(List(in))

  // how to render a Html form
  def render(
    pageKey: List[String],
    fieldKey: List[String],
    tell: Option[Html], 
    path: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Option[Html] = Some{

    val existingValue: String = decode(data).toOption.getOrElse("")
    
    ??? // we might want to use a twirl view to produce the resulting Html  
  }
}
```

Often we will want to have uniform create `WebAsk` instances for us
automatically. For this purpose we can use `InferWebAsk` to generate
forms and codecs for more complex datatypes via typeclass derivation.

In addition to the `WebAsk` instances there are also `WebTell`s. These 
are comparatively simple (as no codec is needed) - 

```scala
implicit val tellInt = new WebTell[Html, Int] {
  def render(
	in: Int, 
	key: String, 
	messages: UniformMessages[Html]
  ): Option[Html] = ???
}
```

An implicit `WebTell[Html,T]` and an implicit `WebAsk[Html,A]` can combine to
produce an implicit `WebInteraction[Html,T,A]` for consumption by the
interpreter.

# old!!!
# old!!!
# old!!!




## Selection of fields

Sometimes we may want to offer the user a choice between several
fields or sets of fields. For example an `Either[String,Int]` or a
sealed trait hierarchy. In this situation you may want to use
Javacript to control hiding and revealing form elements, perhaps
toggled by radio buttons.

The `selectionOfFields` method takes a list of options and a function
to control the Html that should be presented when that option is
active.

```scala
  def selectionOfFields(
    inner: List[(
      String,
      (List[String], Path, Input, ErrorTree, UniformMessages[Html]) => Html
    )]
  )(
    key: List[String],
    path: Path,
    values: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html = ???
```

## Invoking a journey

We also need an actual action that will invoke the journey.

```scala
def myjourney(targetId: String) =
  Action.async { implicit request: Request[AnyContent] =>

    // interpret our journey using the play interpreter
    val playJourney = myJourney(
      new FuturePlayInterpreter[TellTypes, AskTypes],
    )

    // Run the journey using the request and the targetId
    run(playJourney, targetId){ output =>
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

import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import play.twirl.api.{Html, HtmlFormat}

class ExampleController (
  implicit val messagesApi: MessagesApi,
  ec: concurrent.ExecutionContext
) extends PlayInterpreter[Html] with I18nSupport {

  def messages(
    request: Request[AnyContent]
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
      (List[String], Path, Input, ErrorTree, UniformMessages[Html]) => Html
    )]
  )(
    key: List[String],
    path: Path,
    values: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html = ???

  def myjourney(targetId: String) =
    Action.async { implicit request: Request[AnyContent] =>

      // interpret our journey using the play interpreter
      val playJourney = myJourney(
        new FuturePlayInterpreter[TellTypes, AskTypes],
      )

      // Run the journey using the request and the targetId
      run(playJourney, targetId){ output =>
        // final code to be run upon successful completion
        // of the journey
        println(output)
        Future.successful(Ok("Completed"))
      }
    }
}
```
