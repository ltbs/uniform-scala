package controllers

import cats.implicits._
import ltbs.uniform._, interpreters.playframework._
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import play.twirl.api.{Html, HtmlFormat}
import ltbs.uniform.common.web.InferFormField

case class HmrcPlayInterpreter(results: Results) extends PlayInterpreter[Html](results) with InferFormField[Html] with Widgets {

    def messages(
      request: Request[AnyContent]
    ): UniformMessages[Html] = UniformMessages.attentionSeeker.map{HtmlFormat.escape}

    def pageChrome(
      key: List[String],
      errors: ErrorTree,
      tell: Html,
      ask: Html,
      breadcrumbs: Path,
      request: Request[AnyContent],
      messages: UniformMessages[Html]
    ): Html =
      views.html.chrome(key, errors, Html(tell.toString + ask.toString), breadcrumbs)(messages, request)

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
