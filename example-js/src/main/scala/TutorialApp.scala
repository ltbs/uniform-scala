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
import ltbs.uniform.widgets.govuk._

import InferParser._

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
    messages.reload()
  }

  @JSExportTopLevel("saveAndContinue")
  def saveAndContinue(pageId: String): Unit = {
    journey(Submit(pageId))
    println(s"state: $state")
  }

  @JSExportTopLevel("backLink")
  def backLink(): Unit = {
    val (last::others) = breadcrumbs
    breadcrumbs = others
    journey(Back(last))
  }

  def main(args: Array[String]): Unit = {
    journey(Back(""))
  }

  var state: DB = implicitly[Monoid[DB]].empty
  var breadcrumbs: List[String] = Nil

  implicit val cmsMessages = CmsMessages.fromText{
    """
# https://www.playframework.com/documentation/latest/ScalaI18N
crown-copyright=Crown Copyright

litresProduced.heading=Litres Produced
litresProduced._1.heading=Litres produced at lower concentration
litresProduced._1.hint=Lower concentration is between 5 and 8 grams of sugar per 100ml
litresProduced.details=What if I''m not sure?|Well then go and measure it
litresProduced.details.2=What if I want more answers?\
  |This is just to test line-wrapping and also list entries

TRUE=Yes
FALSE=No

there.is.a.problem=There is a problem

required={0} is required
  """}

  @JSExportTopLevel("back")
  def back(page: String) = journey(Back(page))


  def journey(action: Action) = {
    val output: ((Either[Page, String], DB), List[String]) =
      program[FxAppend[TestProgramStack, JsStack]]
        .useForm(inferJsForm[Boolean])
        .useForm(inferJsForm[Litres])
        .runReader(action)    
        .runEither
        .runState(state)
        .runState(List.empty[String])    
        .runEval
        .run

    val ((result,newState),newBreadcrumbs) = output
    println(s"breadcrumbs: $breadcrumbs")
    breadcrumbs = newBreadcrumbs
    state = newState
    result match {
      case Left(page) => setPage(page)
      case Right(fin) => scala.scalajs.js.Dynamic.global.alert(fin)
    }
  }

  def setPage(page: Page): Unit = {
    page.title.map { title =>
      breadcrumbs = title :: breadcrumbs
      $("#title").html(messages.span(s"heading.$title"))
      $("#backlink").html(messages.span(s"heading.$title"))      
      $("#continue-button").replaceWith(
        s"""|<button class="govuk-button" type="submit" id="continue-button"
            |  onclick="saveAndContinue('$title')">
            |    Save and continue
            |</button>""".stripMargin)
    }

    val backlink = { page.breadcrumbs.headOption match {
      case Some(back) =>
        s"""<a href="#" onclick="back('$back');" class="govuk-back-link">${messages.getMessage(List(s"back-to-$back","back"))}</a>"""
      case None => ""
    }}

    page.body.map { x => $("#mainBody").html(backlink ++ x) }

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
