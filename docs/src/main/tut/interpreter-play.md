---
layout: docs
title: Play Framework Interpreter
---

# Play Interpreter

## Setup

If you are using play 2.6 you will need the following dependency in your build.sbt

```
libraryDependencies +=
  "com.luketebbs.uniform" %% "interpreter-play26" % "{{ site.last-stable-version }}"
```

Or if you are using play 2.5 -

```
libraryDependencies +=
  "com.luketebbs.uniform" %% "interpreter-play25" % "{{ site.last-stable-version }}"
```

## Controller Setup

Next you will need to extend your controller using `PlayController`.

```
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.web._
import ltbs.uniform._

import cats.Monoid
import cats.data.Validated
import cats.implicits._
import concurrent.{Future, ExecutionContext}
import org.atnos.eff._
import play.api._, mvc._, i18n.{Messages => _, _}
import play.api.data._, Forms._
import play.twirl.api.Html
import javax.inject._

@Singleton
class ExampleController @Inject()(implicit val messagesApi: MessagesApi) extends Controller with PlayInterpreter with I18nSupport {

  def messages(request: Request[AnyContent]): Messages =
    convertMessages(messagesApi.preferred(request))

  def renderForm(
    key: List[String],
	errors: ErrorTree,
	form: Html,
	breadcrumbs: List[String],
	request: Request[AnyContent],
	messages: Messages
  ): Html = ???

}
```

This makes the common play interpreter available.

The `messages` method is used for internationalisation, if you are
using the play messages you can convert them using the
`convertMessages` function. It is also possible to use `NoopMessages`
which simply returns the message key - this is rarely useful however as
even with monolingual you will likely want to supply titles, content
and error messages rather than use the text generated from uniform
(for example if you have a field called `userAge` it will literally
label the field with that).

`renderForm` is used for the 'chrome' around the form - i.e. all the
other HTML. Typically this will be a call to the main template view.

Once these are in place you will need to tell uniform how to ask the
user for all the datatypes you have in the program stack. For this you
need to supply an instance of the `PlayForm` trait. For example if we
wanted to handle allowing the user to input a `String` you can enter
the following -

```
val myForm = new PlayForm[String] {

}
```

These are contained in
the `WebMonadForm` class. If we use the
`GreasySpoon` program from the examples we need to be able to ask the user for
an `Int` and for a `Boolean` and as such we must provide an instance of
`WebMonadForm[Int]` and `WebMonadForm[Boolean]`. `WebMonadForm` has the
following structure -

```
trait WebMonadForm[T] {
  def encode(in: T): Encoded
  def decode(out: Encoded): T

  def playForm(
    key: String,
    validation: T => Validated[ValidationError, T]
  ): Form[T]

  def render(
    key: String,
    existing: ValidatedData[T],
    request: Request[AnyContent]
  ): Html
}
```

`Encoded` is just a type-alias for `String`. `encode` and `decode` are necessary so that uniform
can persist the data between requests. You could use Java serialisation here,
JSON encoding using `play-json` or perhaps one of the Scala pickling libraries.
For simplicity here we'll just roll our own.

`render` defines how the form should appear, and `playForm` defines how the form
(and mapping) should be defined, and also how to hook in any validation defined
in the uniform journey itself.

```
class ExampleController2 @Inject()(implicit val messagesApi: MessagesApi) extends Controller with PlayInterpreter with I18nSupport {
  import ltbs.uniform.sampleprograms.GreasySpoon._

  val booleanForm = new WebMonadForm[Boolean] {

    def decode(out: Encoded): Boolean = out == "true"
    def encode(in: Boolean): Encoded = in.toString

    def playForm(
      key: String,
      validation: Boolean => Validated[ValidationError,Boolean]
    ): Form[Boolean] = Form(single(key -> boolean))

    def render(
      key: String,
      existing: ValidatedData[Boolean],
      request: Request[AnyContent]
    ): Html = {
      val form = existing match {
        case Some(Validated.Invalid(e)) => Form(single(key -> boolean)).withError("", e)
        case _ => Form(single(key -> boolean))
      }

      ??? // replace with your view
    }
  }

  val intForm: WebMonadForm[Int] = ???

}
```

Next we need a persistence provider. Ordinarily we'd have something keyed
against the session, or perhaps the users credentials if they are logged in.
This can be used to provide the capabilities for the user to log out and come
back later. For testing purposes however we'll just use a crude in-memory map.

Note that this combines data between ALL users - for obvious reasons you
shouldn't do this for a real service.

```
type DB = Map[String,String]

def persistence(implicit ec: ExecutionContext) = new Persistence {
  private var data: DB = Monoid[DB].empty
  def dataGet: Future[DB] = Future.successful(data)
  def dataPut(dataIn: DB): Future[Unit] =
    Future(data = dataIn).map{_ => ()}
}
```

Now we have our persistence mechanism, we can construct the actual journey as
needed -

```
class ExampleController3(
  implicit val ec: ExecutionContext
) extends Controller with PlayInterpreter {

  import ltbs.uniform.sampleprograms.GreasySpoon._

  val booleanForm: WebMonadForm[Boolean] = ???
  val intForm: WebMonadForm[Int] = ???

  // combine the source and the target stacks
  type CombinedStack = FxAppend[GreasyStack, PlayStack]

  // start from greasyspoon
  val convertedProgram = greasySpoon[CombinedStack]
        .useForm(intForm)      // map Int fields
        .useForm(booleanForm)  // map Boolean fields

  def greasy(key: String) = Action.async { implicit request =>

    // run our program
    runWeb(
      convertedProgram,
      key,
      request,
      persistence
    )( a =>
      // handle the output
      Future.successful(Ok(s"Your bill is $a groats"))
    )
  }

}
```

The `key` argument is the page the user is requesting, taken from the URI. If
the user is attempting to go back in the journey then uniform knows to render an
old page. If the user tries to skip forward however uniform will not permit them
to advance without answering the requisite questions correctly.

The `request` argument is needed to capture the values submitted by the user.

The `persistence` argument tells uniform how to store the state of the users
journey.

The `program` argument is the most interesting. This takes the webmonad program
to be executed. Our `greasySpoon` program must first be converted, intuitively
what we are doing here is telling uniform how each datatype it is interested in
should be obtained from the user.

In practice we first create a new combined monad stack (we have called it
`CombinedStack`) using `FxAppend` - this stack
contains everything we are converting from (`GreasyStack`) and everything we are
converting to (`PlayStack`).

We then apply `intForm` and `booleanForm` to our `greasySpoon` program - each
call of `useForm` eliminates a layer of the monad stack by converting it into
monads that exist inside `PlayStack` (our target). Once we have eliminated
everything from `GreasyStack` the result is then a program we then can run by
calling `runWeb`.
