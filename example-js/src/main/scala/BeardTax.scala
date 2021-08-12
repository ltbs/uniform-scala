package ltbs.uniform
package examples
package js

import cats.implicits._
import beardtax._
import interpreters.js._
import common.web._

import org.querki.jquery._
import scala.scalajs._, js.annotation.JSExportTopLevel
import scala.concurrent._, ExecutionContext.Implicits.global
import scalatags.JsDom.all._
import ltbs.uniform.validation.Rule

object BeardTaxApp extends JsInterpreter[Tag]($("#uniform")) with InferWebAsk[Tag] with examples.Widgets {

  def render(in: Option[Tag]): String = in.fold("")(_.toString)

  def renderFrame(
    key: List[String],
    frame: JQuery,
    tell: Tag,
    ask: Tag,
    breadcrumbs: Breadcrumbs,
    errors: ErrorTree,
    messages: UniformMessages[Tag]
  ): Future[Unit] = Future {

    $(".govuk-heading-xl").html(messages(key.mkString(".")).toString)

    if (errors.nonEmpty) {
      $(".govuk-error-summary").replaceWith(errorSummary(key, errors, messages).toString)
      $(".govuk-error-summary").show()
    } else {
      $(".govuk-error-summary").html("")
      $(".govuk-error-summary").hide()
    }

    breadcrumbs.drop(1).headOption match {
      case Some(link) =>
        $(".govuk-back-link").html(messages({link :+ "back"}.mkString(".")).toString)
        $(".govuk-back-link").show()
      case _ =>
        $(".govuk-back-link").html("")
        $(".govuk-back-link").hide()

    }

    if (errors.nonEmpty) {
      $(".govuk-error-summary").replaceWith(errorSummary(key, errors, messages).toString)
      $(".govuk-error-summary").show()
    } else {
      $(".govuk-error-summary").html("")
      $(".govuk-error-summary").hide()
    }
      frame.html(tell.toString + ask.toString)
    ()
  }

  import cats.~>
  implicit def e = new ~>[WebMonad[?,Tag],WebMonad[?,Tag]]{
    def apply[A](in: WebMonad[A,Tag]): WebMonad[A,Tag] = in
  }

  val messages: UniformMessages[Tag] = UniformMessages.echo.map(span(_))

  val jsJourney = interpret(beardProgram(jsHod))
  jsJourney.run()(PageIn(Nil, crumbs, None, state, Nil, JourneyConfig(), messages))

  def jsHod = new Hod[WebMonad[Tag, *]] {
    def costOfBeard(beardStyle: BeardStyle, length: BeardLength): WebMonad[Tag, Int] =
      12.pure[WebMonad[Tag, *]]
    def recordBeardHeight(height: Int): WebMonad[Tag,Unit] =
      ().pure[WebMonad[Tag, *]]
  }

  @JSExportTopLevel("back")
  def backLink(): Future[Unit] =
    jsJourney.run()(PageIn(crumbs.init.last, crumbs, None, state, Nil, JourneyConfig(), messages))

  @JSExportTopLevel("submit")
  def submit(): Future[Unit] =
      jsJourney.run()(PageIn(key, Nil, getData.toOption, state, Nil, JourneyConfig(), messages)).andThen{
        case _ =>
          $("#key").html(key.toString)
          $("#state").html(state.toString)
          $("#crumbs").html(crumbs.toString)
      }


  def main(args: Array[String]): Unit = {
    println("Hello world!")
  }

}
