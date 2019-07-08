---
layout: docs
title: Views
---

# Views

## Different View Types

Uniform is agnostic about how views are constructed. In the examples
here we have used twirl Html, but you can use any representation you
wish, as long as it is both `Writeable` and a `Monoid`.

If you wanted to use scalatags, for example, you could do something
like this -

```scala
import scalatags.Text.all._
import play.api.http.{Writeable, ContentTypeOf, ContentTypes}
import play.api.mvc.Codec
import cats.Monoid
import cats.implicits._

implicit val fragContentType: ContentTypeOf[Frag] = {
  ContentTypeOf[Frag](Some(ContentTypes.HTML))
}

implicit def fragWriteable(implicit codec: Codec): Writeable[Frag] = {
  Writeable(frag ⇒ codec.encode("<!DOCTYPE html>\n" + frag.render))
}

implicit val fragMonoid = new Monoid[Frag] {
    def empty: Frag = RawFrag("")
    def combine(a: Frag, b: Frag): Frag = RawFrag(a.render + b.render)
}
```

## How fields are rendered and encoded

Views for compound types are produced using typeclass derivation. For
example, suppose you tell the play interpreter how to construct a view
for a `String` field like so -

```scala

import ltbs.uniform._, interpreters.playframework._

implicit val scalatagsStringField = new FormField[String,Frag] {

  def decode(out: Input): Either[ErrorTree,String] = {
    val root: Option[String] = out.valueAtRoot
      .flatMap(_.filter(_.trim.nonEmpty).headOption)

    root match {
      case None ⇒ Left(ErrorMsg("required").toTree)
      case Some(data) ⇒ Right(data)
    }
  }

  def encode(in: String): Input =
    Input.one(List(in))

  def render(
    key: List[String],
    path: Path,
    data: Option[Input],
    errors: ErrorTree,
    messages: UniformMessages[Frag]
  ): Frag = {
    val existingValue: String =
      data.flatMap(_.valueAtRoot.flatMap{_.headOption}).getOrElse("")

    input(`type` := "text", name := key.mkString(".")) { existingValue }
  }
}
```

`decode` is taking an `Input` and
converting it to either a resulting correct `String` or an
`ErrorTree`. Here the only built-in validation we are doing is to
check the string is non-empty. Normally we would only apply the
validation here to ensure that the data can be mapped to the correct
type (it's debatable as to if we should even check for non-empty
here). We can always add validation to the actual pages if we want.

`encode` takes a value and turns it back into an Input. This cannot go
wrong so there is no `ErrorTree`.

`render` is probably the most interesting part here - this emits some
Html (a scalatags `Frag` in our instance here).

Note here we're not doing any special rendering based upon the error
or any field labels.

There is quite a lot going on in this string example. However it's
easy to produce other field types from this -

```scala
implicit val scalatagsIntField: FormField[Int,Frag] =
  scalatagsStringField.simap(x ⇒
    Either.catchOnly[NumberFormatException](x.toInt)
      .leftMap(_ ⇒ ErrorMsg("bad.value").toTree)
  )(_.toString)
```

An `Int` field would be almost identical to a `String` field - the
presentation should be the same but the `encode` method needs to take
an `Int` and the `decode` method needs to emit an
`Either[ErrorTree,Int]`. These methods should be based upon the
ones used for `scalatagsStringField` - we still want to check the
field is non-empty before attempting to parse it into an `Int` but we
want to append a check and conversion to the `decode` method. The
`encode` method should still place the value in the correct place but
we want to turn the `Int` into a `String` _beforehand_.

The `simap` method (named because the transformation is a split
epimorphism) allows us to prepend a method to `encode` (`Int ⇒ String`
in our case) and append a flatMap transformation to `decode` (
`String ⇒ Either[ErrorTree, Int]`).

## How views are derived

One of the main advantages of using the Uniform Play Interpreter over
manually crafting journeys is that you can take advantage of the form
inference, that is if you have a case class which is composed of
`String`s and `Int`s the interpreter can figure out how to display it
and encode it for you.

```scala
case class Person(forename: String, surname: String, age: Int)

val luke = Person("Luke", "Tebbs", 38)

// normally we won't need this import when working within a
// controller -
object Inferer extends ltbs.uniform.common.web.InferFormField[Frag] {

  def selectionOfFields(
    inner: List[(String, (List[String], Path, Option[Input], ErrorTree, UniformMessages[Frag]) ⇒ Frag)]
  )(
    key: List[String],
    path: Path,
    values: Option[Input],
    errors: ErrorTree,
    messages: UniformMessages[Frag]
  ): Frag = ???

}
import Inferer._

implicitly[FormField[Person, Frag]].encode(luke)
```

## Overriding views for a single page

## Overriding views for a whole journey

## Creating more complex actions
