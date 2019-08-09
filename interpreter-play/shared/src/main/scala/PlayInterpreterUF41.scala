package ltbs.uniform
package interpreters.playframework

import play.api._,mvc._,http.Writeable
import concurrent.{ExecutionContext, Future}
import cats.Monoid
import cats.implicits._
import common.web._

abstract class PlayInterpreter[Html: Writeable: Monoid](
  implicit ec: ExecutionContext
) extends GenericWebInterpreter[Html] with Compatibility.PlayController {

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
    def run(id: List[String])(
      f: A => Future[Result]
    )(implicit
      request: Request[AnyContent],
      persistence: PersistenceEngine[Request[AnyContent]]
    ): Future[Result] = {

      val data: Option[Input] = request.body.asFormUrlEncoded.map {
        _.map{ case (k,v) => (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
      }

      import common.web.{AskResult => AR}

      persistence.apply(request) { db =>
        wm(PageIn(id, Nil, data, db)) flatMap {
          case common.web.PageOut(path, dbOut, pageOut) =>
            pageOut match {
              case AR.GotoPath(targetPath) =>
                (dbOut, Redirect(relativePath(id, targetPath))).pure[Future]
              case AR.Payload(html, errors) =>
                (db, Ok(pageChrome(id, errors, Monoid[Html].empty, html, path, request, messages(request)))).pure[Future]
              case AR.Success(result) =>
                f(result).map{ (db, _) }
            }
        }
      }
    }
  }

}
