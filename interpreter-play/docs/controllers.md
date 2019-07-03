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

```tut:silent
import ltbs.uniform._, interpreters.playframework._

import play.api.i18n.{Messages ⇒ _, _}
import play.api.mvc._
import play.twirl.api.{Html, HtmlFormat}

class ExampleController (
  implicit val messagesApi: MessagesApi
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

}
```
