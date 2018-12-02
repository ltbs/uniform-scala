package ltbs.uniform.prototype

import cats._, implicits._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.querki.jquery._
import ltbs.uniform.sampleprograms.LitreageTest._

import JsInterpreter._
import JsImplementations._
import scala.scalajs.js.annotation.JSExportTopLevel
import cats.Monoid
import ltbs.uniform.datapipeline._
import InferParser._
import InferForm._

object PrototypeApp {

  implicit val messages = new MessagesProvider {
    reload()
  }

  @JSExportTopLevel("enableMessages")
  def enableMessages(): Unit = {
    messages.enabled = true
    messages.updateMessages()
  }

  @JSExportTopLevel("disableMessages")
  def disableMessages(): Unit = {
    messages.enabled = false
    messages.updateMessages()
  }

  @JSExportTopLevel("reloadMessages")
  def reloadMessages(): Unit = {
    println("FFS!")
    messages.reload()
  }

  @JSExportTopLevel("saveAndContinue")
  def saveAndContinue(pageId: String): Unit = {
    journey(pageId)
    println(s"state: $state")
  }

  @JSExportTopLevel("backLink")
  def backLink(): Unit = {
    val (last::others) = breadcrumbs
    breadcrumbs = others
    journey(last)
  }

  def main(args: Array[String]): Unit = {
    journey("")
  }

  var state: DB = implicitly[Monoid[DB]].empty
  var breadcrumbs: List[String] = Nil

  implicit val booleanHtml = new HtmlForm[Boolean] {
    def render(key: String, values: Input, errors: Error, messages: Messages) =
      ltbs.uniform.widgets.govuk.html.radios(
        key,
        Set("TRUE","FALSE"),
        values.value.headOption,
        errors,
        messages
      )
  }

  implicit val longHtml = new HtmlForm[Long] {
    def render(key: String, values: Input, errors: Error, messages: Messages) =
      ltbs.uniform.widgets.govuk.html.string(
        key,
        values,
        errors,
        messages
      )
  }

  def journey(pageId: String) = {
    val output: (Either[Page, String],DB) =
      program[FxAppend[TestProgramStack, JsStack]]
        .useForm(inferJsForm[Boolean])
        .useForm(inferJsForm[(Long,Long)])
        .runEither
        .runEval
        .runState(state)
        .runReader(pageId)
        .run

    state = output._2
    output._1 match {
      case Left(page) => setPage(page)
      case Right(fin) => scala.scalajs.js.Dynamic.global.alert(fin)
    }
  }

  def setPage(page: Page): Unit = {
    page.title.map { title =>
      breadcrumbs = title :: breadcrumbs
      $("#title").html(messages.span(s"heading.$title"))
      $("#continue-button").replaceWith(
        s"""|<button class="button" type="submit" id="continue-button"
            |  onclick="saveAndContinue('$title')">
            |    Save and continue
            |</button>""".stripMargin)
    }

    page.body.map { $("#mainBody").html(_) }

    page.errors.flatTree match {
      case Nil => $("#error-summary-display").css("display", "none")
      case err =>
        $("#error-summary-display").css("display", "block")
        $("#error-summary-list").html {
          err.map{ msg =>
            s"""|<li role="tooltip">
                |  <a href="#packQty.higher" id="packQty.higher-error-summary" data-focuses="packQty.higher">
                |    $msg
                |  </a>
                |</li>""".stripMargin
          }.mkString("")
        }
    }
    ()
  }


}
