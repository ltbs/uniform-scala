package ltbs.uniform
package interpreters.playframework

import play.api._,mvc._,http.Writeable
import concurrent.{ExecutionContext, Future}
import cats.Monoid
import cats.implicits._
import common.web._

abstract class PlayInterpreter[Html: Writeable: Monoid](controller: Results)(
  implicit ec: ExecutionContext
) extends GenericWebInterpreter[Html] {

  def messages(
    request: Request[AnyContent]
  ): UniformMessages[Html]

  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    tell: Html,
    ask: Html,
    breadcrumbs: Path,
    request: Request[AnyContent],
    messages: UniformMessages[Html]
  ): Html

  val log: Logger = Logger("uniform")

  implicit class PlayWebMonad[A](wm: WebMonad[A, Html]) {
    def run(path: String)(
      f: A => Future[Result]
    )(implicit
      request: Request[AnyContent],
      persistence: PersistenceEngine[Request[AnyContent]]
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
              case AskResult.Payload(html, errors) =>
                (db, controller.Ok(pageChrome(id, errors, Monoid[Html].empty, html, path, request, messages(request)))).pure[Future]
              case AskResult.Success(result) =>
                f(result).map{ (db, _) }
            }
        }
      }
    }
  }

}
