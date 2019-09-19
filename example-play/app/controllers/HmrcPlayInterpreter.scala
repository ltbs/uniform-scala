package controllers

import ltbs.uniform._, interpreters.playframework._
import play.api.mvc.{Results, Request, AnyContent}
import scala.concurrent.ExecutionContext.Implicits.global
import play.twirl.api.{Html, HtmlFormat}
import ltbs.uniform.common.web.InferFormField
import cats.syntax.semigroup._
import ltbs.uniform.common.web.ListingGenerator

case class HmrcPlayInterpreter(
  results: Results,
  messagesApi: play.api.i18n.MessagesApi
) extends PlayTwirlInterpreter(results) with InferFormField[Html] with Widgets with ListingGenerator[Html] {
  
  def genericListingPage(
    rows: List[(Html, Int)]
  ): Html = cats.Monoid[Html].combineAll(rows.map{_._1})

  def messages(
    request: Request[AnyContent]
  ): UniformMessages[Html] =
    this.convertMessages(messagesApi.preferred(request)) |+|
      UniformMessages.bestGuess.map{HtmlFormat.escape}

  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    tell: Html,
    ask: Html,
    breadcrumbs: Breadcrumbs,
    request: Request[AnyContent],
    messages: UniformMessages[Html]
  ): Html =
    views.html.chrome(key, errors, Html(tell.toString + ask.toString), breadcrumbs)(messages, request)

  def selectionOfFields(
    inner: List[(String, (List[String], Breadcrumbs, Input, ErrorTree, UniformMessages[Html]) => Html)]
  )(key: List[String], breadcrumbs: Breadcrumbs, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html = {
    val value: Option[String] = values.valueAtRoot.flatMap{_.headOption}
    views.html.uniform.radios(
      key,
      inner.map{_._1},
      value,
      errors,
      messages,
      inner.map{
        case(subkey,f) => subkey -> f(key :+ subkey, breadcrumbs, {values / subkey}, errors / subkey, messages)
      }.filter(_._2.toString.trim.nonEmpty).toMap
    )
  }

}
