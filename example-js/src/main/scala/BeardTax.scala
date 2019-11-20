package ltbs.uniform
package examples
package js

import beardtax._
import interpreters.js._
import common.web._

import cats.implicits._
import org.querki.jquery._
import scala.scalajs._, js.annotation.JSExportTopLevel
import scala.concurrent._, ExecutionContext.Implicits.global
import scalatags.JsDom.all._

object BeardTaxApp extends App {

  val interpreter = new JsInterpreter[Tag] with InferFormFieldProduct[Tag] with InferFormFieldCoProduct[Tag] with examples.Widgets {

    implicit val tellTwirlUnit = new WebTell[Unit] {
      def render(in: Unit, key: String, messages: UniformMessages[Tag]): Tag = span("")
    }

    def renderFrame(
      key: List[String],
      frame: JQuery,
      htmlForm: Tag,
      breadcrumbs: Path, 
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
      frame.html(htmlForm.toString)
      ()
    }
  }

  import interpreter._

  val i = interpreter.create[TellTypes, AskTypes](UniformMessages.echo.map{span(_)})

  def jsHod = new Hod[WebMonad[?, Tag]] {
    def costOfBeard(beardStyle: BeardStyle, length: BeardLength): WebMonad[Int, Tag] =
      12.pure[WebMonad[?, Tag]]
  }

  val runner = {
    new interpreter.JsRunner[Int](
    beardProgram[interpreter.WM](i, jsHod),
    $("#uniform"),
      diagnostics = true
    )(output => Future($("#uniform").html(output.toString)))
  }

  @JSExportTopLevel("back")
  def backLink(): Future[Unit] = runner.goBack()

  @JSExportTopLevel("submit")
  def submit(): Future[Unit] = runner.submit()

}
