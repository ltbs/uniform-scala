package controllers

import ltbs.uniform._, interpreters.playframework._
import play.api.mvc.{Results, Request, AnyContent}
import scala.concurrent.ExecutionContext.Implicits.global
import play.twirl.api.{Html, HtmlFormat}
import ltbs.uniform.common.web._
import cats.syntax.semigroup._
import ltbs.uniform.examples.{beardtax, LooselyRelatedTC}

case class HmrcPlayInterpreter(
  results: Results,
  messagesApi: play.api.i18n.MessagesApi
) extends PlayTwirlInterpreter(results) with InferFormField[Html] with Widgets with InferListingPages[Html] {
  
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
    messages: UniformMessages[Html],
    stats: FormFieldStats
  ): Html = {
    views.html.chrome(key, errors, Html(
      s"""<div style="border: 1px dotted blue">${stats.isCompound} ${stats.children} ${stats.compoundChildren}</div>""" + 
      tell.toString + ask.toString
    ), breadcrumbs)(messages, request)
  }

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

  /** This tells uniform to wrap or transform anything that is loosely related with the supplied HTML 
    * note however this will be overwritten if you use a custom view
    */
  implicit def genericFormGroupingForLooselyRelated[A: LooselyRelatedTC] = new FormGrouping[A, Html] {
    def wrap(in: Html, key: List[String], messages: UniformMessages[Html]): Html =
      Html("""<div style="border:1px solid red;"> """) |+| in |+| Html("""</div>""")
  }

}
