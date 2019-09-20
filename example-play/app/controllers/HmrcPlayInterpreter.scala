package controllers

import ltbs.uniform._, interpreters.playframework._
import play.api.mvc.{Results, Request, AnyContent}
import scala.concurrent.ExecutionContext.Implicits.global
import play.twirl.api.{Html, HtmlFormat}
import ltbs.uniform.common.web.InferFormField
import cats.syntax.semigroup._

case class HmrcPlayInterpreter(
  results: Results,
  messagesApi: play.api.i18n.MessagesApi
) extends PlayInterpreter[Html](results) with InferFormField[Html] with Widgets {

  def messages(
    request: Request[AnyContent]
  ): UniformMessages[Html] =
    this.convertMessages(messagesApi.preferred(request)) |+|
      UniformMessages.attentionSeeker.map{HtmlFormat.escape}

  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    tell: Html,
    ask: Html,
    breadcrumbs: Path,
    request: Request[AnyContent],
    messages: UniformMessages[Html],
    isCompound: Boolean,
    children: Int,
    compoundChildren: Int
  ): Html = {
    views.html.chrome(key, errors, Html(
      s"""<div style="border: 1px dotted blue">$isCompound $children $compoundChildren</div>""" + 
      tell.toString + ask.toString
    ), breadcrumbs)(messages, request)
  }

  def selectionOfFields(
    inner: List[(String, (List[String], Path, Input, ErrorTree, UniformMessages[Html]) => Html)]
  )(key: List[String], path: Path, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html = {
    val value: Option[String] = values.valueAtRoot.flatMap{_.headOption}
    views.html.uniform.radios(
      key,
      inner.map{_._1},
      value,
      errors,
      messages,
      inner.map{
        case(subkey,f) => subkey -> f(key :+ subkey, path, {values / subkey}, errors / subkey, messages)
      }.filter(_._2.toString.trim.nonEmpty).toMap
    )
  }

}
