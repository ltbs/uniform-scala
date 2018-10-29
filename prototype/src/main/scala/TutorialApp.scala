package ltbs.uniform.prototype

import cats._, implicits._, data.State
import java.io.FileReader
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import org.querki.jquery._
import scala.collection.MapLike
import ltbs.uniform._, UniformTest._
import JsInterpreter._
import JsImplementations._
import scala.scalajs.js.annotation.JSExportTopLevel
import cats.Monoid

import scala.concurrent.ExecutionContext.Implicits.global

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

  def journey(pageId: String) = {
    val output: (Either[Page, String],DB) =
      program[FxAppend[TestProgramStack, JsStack]]
        .useForm(booleanForm)
        .useForm(litresForm)
        .runEither
        .runEval
        .runState(state)
        .runReader(pageId)
        .run

    state = output._2
    output._1 match {
      case Left(page) => setPage(page)
      case Right(fin) => println(s"fin:$fin")
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

    page.errors match {
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
  }


}
