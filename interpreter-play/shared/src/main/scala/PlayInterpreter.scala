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
    def runSync(
      path: String,
      purgeStateUponCompletion: Boolean = false,
      config: JourneyConfig = JourneyConfig()
    )(
      f: A => Result
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req],
      ec: ExecutionContext,
      writeable: Writeable[Html],
      messages: UniformMessages[Html]
    ): Future[Result] = run(path, purgeStateUponCompletion, config){f.map{_.pure[Future]}}

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

      val data: Option[Input] = getData(request)

      persistence(request) { db =>
        wm(PageIn(id, Nil, data, db, Nil, config, messages, request.queryString)) flatMap {
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

    def runExecRender[B](
      path: String,
      purgeStateUponCompletion: Boolean = false,
      config: JourneyConfig = JourneyConfig()
    )(
      exec: A => Future[B],
      finalPath: String,
      render: (A,B) => Html
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req],
      ec: ExecutionContext,
      writeable: Writeable[Html],
      messages: UniformMessages[Html],
      codec: ltbs.uniform.common.web.Codec[(A,B)]
    ): Future[Result] = {
      val baseUrl = request.path.dropRight(path.size)
      val id = path.split("/", -1).toList

      val data: Option[Input] = getData(request)

      persistence(request) { db =>
        db.get(List("__final")).map(s => Input.fromUrlEncodedString(s).flatMap(codec.decode)) match {
          case Some(Right((a,b))) if path == finalPath =>
            val newDb = if (purgeStateUponCompletion) DB.empty else db
            (newDb, Ok(pageChrome(id, ErrorTree.empty, Some(render(a,b)), Nil, request, messages))).pure[Future]
          case Some(Right(_)) => (db, Redirect(finalPath)).pure[Future]
          case None =>
            wm(PageIn(id, Nil, data, db, Nil, config, messages, request.queryString)) flatMap {
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
                  case AskResult.Success(a) =>
                    exec(a).map { b =>
                      (db + (List("__final") -> codec.encode((a,b)).toUrlEncodedString), Redirect(finalPath))
                    }
                }
            }
        }
      }
    }

    protected def getData(request: Req): Option[Input] = request.body.asFormUrlEncoded.map {
      _.map{ case (k,v) => (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
    }
  }

}
