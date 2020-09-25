package controllers

import ltbs.uniform._, interpreters.playframework._, validation.Rule
import play.twirl.api.Html
import play.api.mvc.{Request, AnyContent}
import ltbs.uniform.common.web._
import cats.syntax.semigroup._
import ltbs.uniform.examples.Widgets

trait HmrcPlayInterpreter
    extends PlayInterpreter2[Html]
    with InferFormFields[Html]
    with Widgets
    with AutoListingPage[Html]
{

  def renderListPage[A](
    pageKey: List[String],
    breadcrumbs: Breadcrumbs,
    existingEntries: List[ListingRow[Html]],
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    validation: Rule[List[A]]
  ): Html = ???

  def renderAnd(
    pageKey: List[String],
    fieldKey: List[String],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    members: Seq[(String, Html)]
  ): Html = ???

  def renderOr(
    pageKey: List[String],
    fieldKey: List[String],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    alternatives: Seq[(String, Option[Html])],
    selected: Option[String]
  ): Html = Widgets.radios(
    fieldKey,
    alternatives.map(_._1),
    selected,
    errors,
    messages,
    alternatives.collect{case (k, Some(v)) => (k,v)}.toMap
  )

  def messagesApi: play.api.i18n.MessagesApi
  def messagesForRequest[C <: AnyContent](request: Request[C]): UniformMessages[Html] =
    {messagesApi.preferred(request).convertMessages() |+| UniformMessages.bestGuess }.map{Html(_)}

  def unitAsk: WebInteraction[Unit,Html] = unitField

  def unitTell: GenericWebTell[Unit, Html] = autoTell

  implicit def autoTell[A] = new GenericWebTell[A, Html] {
    def render(in: A, key: String, messages: UniformMessages[Html]): Html = Html("")
  }

  implicit def messages(
    implicit request: Request[AnyContent]
  ): UniformMessages[Html] =
    { messagesApi.preferred(request).convertMessages() |+|
      UniformMessages.bestGuess }.map{Html(_)}
    
  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    tell: Option[Html],
    ask: Option[Html],
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messages: UniformMessages[Html]
  ): Html = ??? // TODO: Implement Me
}


