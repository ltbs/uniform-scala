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

      val id = path.split("/", -1).toList
      
      // //this is a nasty bodge to prevent hitting URL's with a trailing slash
      // //which seem to be caused by the UA handling '..' in the redirection target. 
      // if (id.lastOption == Some("")) {
      //   return (controller.Redirect(
      //     request.path.dropRight(1)
      //   )).pure[Future]
      // }

      val data: Option[Input] = request.body.asFormUrlEncoded.map {
        _.map{ case (k,v) => (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
      }

      persistence(request) { db =>
        wm(PageIn(id, Nil, data, db, Nil)) flatMap {
          case common.web.PageOut(breadcrumbs, dbOut, pageOut, _) =>
            pageOut match {
              case AskResult.GotoPath(targetPath) =>
                val path = relativePath(id, targetPath)
                (dbOut, controller.Redirect(path)).pure[Future]
              case AskResult.Payload(html, errors, messagesOut, stats) =>
                (db, controller.Ok(pageChrome(breadcrumbs.head, errors, mon.empty, html, breadcrumbs, request, messagesOut,stats))).pure[Future]
              case AskResult.Success(result) =>
                f(result).map{ (if (purgeStateUponCompletion) DB.empty else dbOut, _) }
            }
        }
      }
    }
  }

}
