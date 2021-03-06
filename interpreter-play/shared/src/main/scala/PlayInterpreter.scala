package ltbs.uniform
package interpreters.playframework

import scala.concurrent.ExecutionContext
import ltbs.uniform.common.web._
import play.api._,mvc._
import play.api.http.Writeable
import scala.concurrent.Future

trait PlayInterpreter[Html] extends Results with WebInterpreter[Html] {

  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    html: Option[Html],
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messages: UniformMessages[Html]
  ): Html

  implicit class PlayWebMonad[A, Req <: Request[AnyContent]](wm: WebMonad[Html, A]) {
    import cats.implicits._
    def runSync(path: String)(
      f: A => Result
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req],
      ec: ExecutionContext,
      writeable: Writeable[Html],
      messages: UniformMessages[Html]
    ): Future[Result] = run(path){f.map{_.pure[Future]}}

    def run(path: String, purgeStateUponCompletion: Boolean = false, config: JourneyConfig = JourneyConfig())(
      f: A => Future[Result]
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req],
      ec: ExecutionContext,
      writeable: Writeable[Html],
      messages: UniformMessages[Html]
    ): Future[Result] = {
      val baseUrl = request.path.dropRight(path.size)
      val id = path.split("/", -1).toList
      
      val data: Option[Input] = request.body.asFormUrlEncoded.map {
        _.map{ case (k,v) => (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
      }

      persistence(request) { db =>
        wm(PageIn(id, Nil, data, db, Nil, config, messages)) flatMap {
          case common.web.PageOut(breadcrumbs, dbOut, pageOut, _, _) =>
            pageOut match {
              case AskResult.GotoPath(targetPath) =>
                val path = baseUrl + targetPath.mkString("/")
                (dbOut, Redirect(path)).pure[Future]
              case AskResult.Payload(html, errors, messagesOut) =>
                val convertedBreadcrumbs = breadcrumbs.map { c => 
                  baseUrl + c.mkString("/")
                }
                (db, Ok(pageChrome(breadcrumbs.head, errors, html, convertedBreadcrumbs, request, messagesOut))).pure[Future]
              case AskResult.Success(result) =>
                f(result).map{ (if (purgeStateUponCompletion) DB.empty else dbOut, _) }
            }
        }
      }
    }
  }  

}
