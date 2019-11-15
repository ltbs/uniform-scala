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
    purgeStateUponCompletion: Boolean = false
  )(
    f: A => Future[Result]
  ) {

    def goBack(): Future[Unit] =
      run(PageIn(crumbs.init.last, crumbs.init, None, db))

    def submit(): Future[Unit] = {
      val dataStringEncoded = selector.serialize()
      val dataInput = Input.fromUrlEncodedString(dataStringEncoded)
      run(PageIn(crumbs.last, crumbs, dataInput.toOption, db))
    }

    def run(
      request: PageIn
    ): Future[Result] = wm(request) flatMap {
      case common.web.PageOut(path, dbOut, pageOut) =>
        db = dbOut
        crumbs = path

        pageOut match {
          case AskResult.GotoPath(targetPath) =>
            run(request.copy(targetId = targetPath))
          case AskResult.Payload(html, errors, messagesOut, _) =>
            renderFrame(selector, html, errors, messagesOut)
          case AskResult.Success(result) =>
            f(result) map { _ => 
              if (purgeStateUponCompletion) {db = DB.empty}
              ()
            }
        }
    }
    run(PageIn(Nil, crumbs, None, db))
  }

}

