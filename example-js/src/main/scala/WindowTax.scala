package ltbs.uniform.prototype

import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.querki.jquery._
import ltbs.uniform.sampleprograms.WindowTax2._

import JsInterpreter._
import JsImplementations._
import scala.scalajs.js.annotation.JSExportTopLevel
import cats.Monoid
import ltbs.uniform.{DB,UniformCore,Uniform,UniformMessages}
import ltbs.uniform.web._
import ltbs.uniform.web.parser._
import ltbs.uniform.widgets.govuk._
import play.twirl.api.{Html,HtmlFormat}

import InferParser._

object WindowTax {

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

  implicit val cmsMessages = UniformMessages.echo.map(HtmlFormat.escape)

  @JSExportTopLevel("back")
  def back(page: String) = journey(Back(page.split("[.]").toList))

  type STACKZ = Fx.append[
    Fx.fx2[
      Uniform[List[Window],ListControl,?],
      Uniform[Window,Boolean,?]
    ],
    WindowsTaxStack
  ]

  type STACKY = Fx.prepend[
    Uniform[Window,Boolean,?],
    SingleWindowStack
  ]

  val fu3: List[Window] => Html = listingTable(
    "windows",
    {ltbs.uniform.widgets.govuk.html.listing(_,_,_,_,_)}, 
    fr,
    cmsMessages
  )(_)

  def fr(i:Window): Html = Html(i.toString)

  def journey(implicit action: Action) = {
    val (result,UniformCore(newState,newBreadcrumbs,_)) = {
      program[FxAppend[STACKZ, JsStack]]
        .delist{
            (existing: List[Window], default: Option[Window]) =>
            singleWindowProgram[STACKY](existing,default)
        }
        .useForm(fu3, inferJsForm[ListControl])
        .useForm(fr, inferJsForm[Boolean])      
        .useForm(inferJsForm[Int])
        .useForm(inferJsForm[(Int,Int)])
        .useForm(inferJsForm[Orientation])
        .useForm(inferJsForm[Boolean])
        .runEither
        .runState(UniformCore(state))
        .run
    }
    println("BREADCRUMBS UPDATE:" ++ newBreadcrumbs.toString)
    breadcrumbs = newBreadcrumbs
    state = newState
    result match {
      case Left(page) => setPage(page)
      case Right(fin) => scala.scalajs.js.Dynamic.global.alert(s"You have £$fin to pay")
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
      case Some(back) =>
        s"""<a href="#" onclick="back('${back.mkString(".")}');" class="govuk-back-link">${messages.getMessage(List(s"back-to-${back.mkString(".")}","back"))}</a>"""
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
