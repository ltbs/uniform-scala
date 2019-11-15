package ltbs.uniform
package interpreters.playframework

import play.api._,mvc._,http.Writeable
import concurrent.{ExecutionContext, Future}
import cats.implicits._
import common.web._

abstract class PlayInterpreter[Html: Writeable](controller: Results)(
  implicit ec: ExecutionContext
) extends GenericWebInterpreter[Html] {

  def messages(
    request: Request[AnyContent]
  ): UniformMessages[Html]

  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    ask: Html,
    breadcrumbs: Path,
    request: Request[AnyContent],
    messages: UniformMessages[Html],
    fieldStats: FormFieldStats
  ): Html

  val log: Logger = Logger("uniform")

  implicit class PlayWebMonad[A, Req <: Request[AnyContent]](wm: WebMonad[A, Html]) {

    def runSync(path: String)(
      f: A => Result
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req]
    ): Future[Result] = run(path){f.map{_.pure[Future]}}

    def run(path: String, purgeStateUponCompletion: Boolean = false)(
      f: A => Future[Result]
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req]
    ): Future[Result] = {

      val id = path.split("/").filter(_.nonEmpty).toList

      val data: Option[Input] = request.body.asFormUrlEncoded.map {
        _.map{ case (k,v) => (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
      }

      persistence.apply(request) { db =>
        wm(PageIn(id, Nil, data, db)) flatMap {
          case common.web.PageOut(path, dbOut, pageOut) =>
            pageOut match {
              case AskResult.GotoPath(targetPath) =>
                (dbOut, controller.Redirect(relativePath(id, targetPath))).pure[Future]
              case AskResult.Payload(html, errors, messagesOut, stats) =>
                (db, controller.Ok(pageChrome(id, errors, html, path, request, messagesOut, stats))).pure[Future]
              case AskResult.Success(result) =>
                f(result).map{ (if (purgeStateUponCompletion) DB.empty else dbOut, _) }
            }
        }
      }
    }
  }

}
