package ltbs.uniform
package interpreters.js

import examples.beardtax._

import cats.implicits._
import org.querki.jquery._
import JsImplementations._
import scala.scalajs.js.annotation.JSExportTopLevel
import cats.Monoid
import ltbs.uniform.web.parser._
import ltbs.uniform.widgets.govuk._
import play.twirl.api.{Html,HtmlFormat}

object BeardTaxApp {

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
    journey(Submit(pageId.split("[.]").toList))
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
    journey(Back(Nil))
    ()
  }

  var state: DB = implicitly[Monoid[DB]].empty
  var breadcrumbs: List[List[String]] = Nil

  implicit val cmsMessages: UniformMessages[Html] = CmsMessages.fromText{
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
is-public.details=What happens if I make a claim to the throne?|Making \
  a false claim to the throne is punishable by hanging and \
  excommunication on a second offense.

is-public.inner.forename.heading=Forenames
is-public.inner.surname.heading=Surname
is-public.inner.age.heading=Age

beard-style.heading=Beard Style

beard-length-mm.heading=Beard Length
beard-length-mm._1.heading=Length at shortest point
beard-length-mm._1.hint=Please give length in mm
beard-length-mm._2.heading=Length at longest point
beard-length-mm._2.hint=Please give length in mm
beard-length-mm.details=Details

TRUE=Yes
FALSE=No

there.is.a.problem=There is a problem
required=This field is mandatory
nonnumericformat=Please enter a number
back=back
  """}.map(Html.apply) |+| UniformMessages.bestGuess.map(HtmlFormat.escape)

  @JSExportTopLevel("back")
  def back(page: String) = journey(Back(page.split("[.]").toList))

  def journey(implicit action: Action) = {
    val (result,UniformCore(newState,newBreadcrumbs,_)) = {
      program[FxAppend[TestProgramStack, JsStack]]
        .useForm(inferJsForm[Option[MemberOfPublic]])
        .useForm(inferJsForm[BeardStyle])
        .useForm(inferJsForm[BeardLength])
        .runEither
        .runState(UniformCore(state))
        .run
    }
    println("BREADCRUMBS UPDATE:" ++ newBreadcrumbs.toString)
    breadcrumbs = newBreadcrumbs
    state = newState
    result match {
      case Left(page) ⇒ setPage(page)
      case Right(fin) ⇒ scala.scalajs.js.Dynamic.global.alert(s"You have £$fin to pay")
    }
  }

  def updateDataTargets(): Unit = {
    val i = $("""[data-target] > input[type="radio"]""")
    i.change{ e: org.scalajs.dom.Element ⇒

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
    page.title.map { title ⇒
//      breadcrumbs = title.split("[.]").toList :: breadcrumbs
      $("#title").html(messages.span(s"heading.$title"))
      $("#backlink").html(messages.span(s"heading.$title"))
      $("#continue-button").replaceWith(
        s"""|<button class="govuk-button" type="submit" id="continue-button"
            |  onclick="saveAndContinue('$title')">
            |    Save and continue
            |</button>""".stripMargin)
    }

    val backlink = { breadcrumbs.lastOption match {
      case Some(back) ⇒
        s"""<a href="#" onclick="back('${back.mkString(".")}');" class="govuk-back-link">${messages.getMessage(List(s"back-to-${back.mkString(".")}","back"))}</a>"""
      case None ⇒ ""
    }}

    page.body.map { x ⇒ $("#mainBody").html(backlink ++ x) }

    page.errors.flatTree match {
      case Nil ⇒ $("#error-summary-display").css("display", "none")
      case err ⇒
        $("#error-summary-display").css("display", "block")
        $("#error-summary-list").html {
          err.map{ msg ⇒
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
