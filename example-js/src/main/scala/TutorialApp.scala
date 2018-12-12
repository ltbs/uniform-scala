package ltbs.uniform.prototype

import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.querki.jquery._
import ltbs.uniform.sampleprograms.BeardTax._

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
    ()
  }

  def main(args: Array[String]): Unit = {
    journey(Back(""))
    ()
  }

  var state: DB = implicitly[Monoid[DB]].empty
  var breadcrumbs: List[String] = Nil

  implicit val cmsMessages = CmsMessages.fromText{
    """
# https://www.playframework.com/documentation/latest/ScalaI18N
crown-copyright=Crown Copyright
new.service=This is a new service, your
help.improve=will help us to improve it
all-content-available=All content is available under
ogl3=OGL3
except-where-otherwise-stated= except where otherwise stated.

is-public.heading=Are you a member of the public?
is-public.outer.FALSE=No, I’m King Henry VIII.

is-public.inner.forename.heading=Forenames
is-public.inner.surname.heading=Surname
is-public.inner.age.heading=Age

beard-style.heading=Beard Style

beard-length-mm.heading=Beard Length
beard-length-mm._1.heading=Length at shortest point
beard-length-mm._2.heading=Length at longest point

TRUE=Yes
FALSE=No

there.is.a.problem=There is a problem
required=This field is mandatory
nonnumericformat=Please enter a number
back=back
  """}

  @JSExportTopLevel("back")
  def back(page: String) = journey(Back(page))


  def journey(action: Action) = {
    val output: ((Either[Page, Int], DB), List[String]) =
      program[FxAppend[TestProgramStack, JsStack]]
        .useForm(inferJsForm[Option[MemberOfPublic]])
        .useForm(inferJsForm[BeardStyle])
        .useForm(inferJsForm[BeardLength])    
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
      case Right(fin) => scala.scalajs.js.Dynamic.global.alert(s"You have $fin to pay")
    }
  }

  def updateDataTargets(): Unit = {
    val i = $("""[data-target] > input[type="radio"]""")
    i.change{ e: org.scalajs.dom.Element =>
      
      val radioValue=$(e).value
      val dataTarget=$(e).parent("[data-target]").attr("data-target")
	$(".conditional-" + dataTarget).removeClass("govuk-radios__conditional")
	$(".conditional-" + dataTarget).addClass("govuk-radios__conditional--hidden")
	$("#conditional-" + dataTarget + "-" + radioValue).removeClass("govuk-radios__conditional--hidden")
	$("#conditional-" + dataTarget + "-" + radioValue).addClass("govuk-radios__conditional")	      
    }
    ()
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
    updateDataTargets()
  }


}
