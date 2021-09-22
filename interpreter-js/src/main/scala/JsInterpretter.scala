package ltbs.uniform
package interpreters.js

import common.web._
import concurrent._
import org.querki.jquery._

abstract class JsInterpreter[Html](selector: JQuery)(implicit ec: ExecutionContext) extends WebInterpreter[Html] {

  var key: List[String] = Nil
  var state: DB = DB.empty
  var crumbs: Breadcrumbs = Nil

  def render(in: Option[Html]): String

  def submit(): Boolean

  def renderFrame(
    key: List[String], 
    html: Option[Html],
    breadcrumbs: Breadcrumbs, 
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Future[Unit] = Future{
    selector.html(render(html))
    $("#errors").html(errors.toString)
    $(".govuk-button").click(() => submit())
    ()
  }

  def getData: Either[ErrorTree,Input] = {
    val dataStringEncoded = selector.serialize()
    Input.fromUrlEncodedString(dataStringEncoded)
  }

  implicit class JsWebMonad[A](wm: WebMonad[Html, A]) {
    def run(
      config: JourneyConfig = JourneyConfig()
    )(
      request: PageIn[Html]
    )(implicit ec: ExecutionContext
    ): Future[Result] = wm(request) flatMap {
      case common.web.PageOut(pathOut, dbOut, pageOut, _, _) =>

        state = dbOut
        crumbs = pathOut

        pageOut match {
          case AskResult.GotoPath(targetPath) =>
            key = targetPath            
            run(config)(request.copy(targetId = targetPath, breadcrumbs = pathOut, request = None, state = dbOut, pathPrefix = Nil))
          case AskResult.Payload(html, errors, messagesOut) =>
            renderFrame(key, html, crumbs, errors, messagesOut)
          case AskResult.Success(result) =>
            Future{selector.html(result.toString); ()}
        }
    }
  }
}

