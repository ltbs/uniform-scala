package ltbs.uniform
package interpreters.js

import common.web._

import concurrent._
import org.querki.jquery._

abstract class JsInterpreter[Html](
  implicit ec: ExecutionContext
) extends GenericWebInterpreter[Html] {

  def renderFrame(
    frame: JQuery, 
    htmlForm: Html,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Future[Unit]

  case class JsRunner[A](
    wm: WebMonad[A, Html],
    selector: JQuery,
    var db: DB = DB.empty,
    var crumbs: Path = Nil,
    purgeStateUponCompletion: Boolean = false,
    diagnostics: Boolean = false
  )(
    f: A => Future[Result]
  ) {

    def goBack(): Future[Unit] = crumbs match {
      case (_::last::_) => 
        run(PageIn(last, Nil, None, db))
      case _ =>
        Future.successful(())
    }

    def submit(): Future[Unit] = {
      val dataStringEncoded = selector.serialize()
      val dataInput = Input.fromUrlEncodedString(dataStringEncoded)
      org.scalajs.dom.window.alert(s"dataSubmitted: $dataInput")
      org.scalajs.dom.window.alert(s"target: ${crumbs.head}")      
      run(PageIn(crumbs.head, Nil, dataInput.toOption, db))
    }

    def run(
      request: PageIn
    ): Future[Result] = wm(request) flatMap {
      case common.web.PageOut(path, dbOut, pageOut) =>

        db = dbOut
        crumbs = path

        if (diagnostics) {
          $("#state").html(db.toString)
          $("#crumbs").html(crumbs.toString)          
        }

        pageOut match {
          case AskResult.GotoPath(targetPath) =>
            run(request.copy(targetId = targetPath, path = Nil, request = None, state = db))
          case AskResult.Payload(html, errors, messagesOut, _) =>
            renderFrame(selector, html, errors, messagesOut)
          case AskResult.Success(result) =>
            f(result) map { _ => 
              if (purgeStateUponCompletion) {db = DB.empty}
              ()
            }
        }        
    }
    run(PageIn(Nil, Nil, None, db))
  }

}

