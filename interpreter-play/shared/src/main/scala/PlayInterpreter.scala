package ltbs.uniform
package interpreters.playframework

import play.api._,mvc._,http.Writeable
import concurrent.{ExecutionContext, Future}
import cats.Monoid
import cats.implicits._
import common.web._

abstract class PlayInterpreter[Html: Writeable](controller: Results)(
  implicit ec: ExecutionContext,
  val mon: Monoid[Html]
) extends GenericWebInterpreter[Html] {

  def messages(
    request: Request[AnyContent]
  ): UniformMessages[Html]

  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    tell: Html,
    ask: Html,
    breadcrumbs: Breadcrumbs,
    request: Request[AnyContent],
    messages: UniformMessages[Html]
  ): Html

  val log: Logger = Logger("uniform")

  implicit class PlayWebMonad[A, Req <: Request[AnyContent]](wm: WebMonad[A, Html]) {

    def runSync(path: String)(
      f: A => Result
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req]
    ): Future[Result] = run(path){f.map{_.pure[Future]}}

    def run(path: String)(
      f: A => Future[Result]
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req]
    ): Future[Result] = {

      val id = path.split("/").filter(_.nonEmpty).toList

      val data: Option[Input] = request.body.asFormUrlEncoded.map {
        _.map{ case (k,v) => (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
      }

      persistence(request) { db =>
        wm(PageIn(id, Nil, data, db, Nil)) flatMap {
          case common.web.PageOut(breadcrumbs, dbOut, pageOut, _) =>
            println(pageOut)
            pageOut match {
              case AskResult.GotoPath(targetPath) =>
                (dbOut, controller.Redirect(relativePath(id, targetPath))).pure[Future]
              case AskResult.Payload(html, errors, messagesOut) =>
                (db, controller.Ok(pageChrome(id, errors, mon.empty, html, breadcrumbs, request, messagesOut))).pure[Future]
              case AskResult.Success(result) =>
                f(result).map{ (db, _) }
            }
        }
      }
    }
  }

}
