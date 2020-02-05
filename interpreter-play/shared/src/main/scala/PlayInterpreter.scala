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
    tell: Html,
    ask: Html,
    breadcrumbs: List[String],
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

    def run(path: String, purgeStateUponCompletion: Boolean = false, config: JourneyConfig = JourneyConfig())(
      f: A => Future[Result]
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req]
    ): Future[Result] = {
      val baseUrl = request.path.dropRight(path.size)
      val id = path.split("/", -1).toList
      
      // //this is a nasty bodge to prevent hitting URL's with a trailing slash
      // //which seem to be caused by the UA handling '..' in the redirection target. 
      // if (id.lastOption == Some("")) {
      //   return (controller.Redirect(
      //     request.path.dropRight(1)
      //   )).pure[Future]
      // }
      println(request.body.asFormUrlEncoded)
      val data: Option[Input] = request.body.asFormUrlEncoded.map {
        _.map{ case (k,v) => (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
      }

      persistence(request) { db =>
        wm(PageIn(id, Nil, data, db, Nil, config)) flatMap {
          case common.web.PageOut(breadcrumbs, dbOut, pageOut, _, _) =>
            pageOut match {
              case AskResult.GotoPath(targetPath) =>
                val path = baseUrl + targetPath.mkString("/")
                (dbOut, controller.Redirect(path)).pure[Future]
              case AskResult.Payload(tell, ask, errors, messagesOut, stats) =>
                val convertedBreadcrumbs = breadcrumbs.map { c => 
                  baseUrl + c.mkString("/")
                }
                (db, controller.Ok(pageChrome(breadcrumbs.head, errors, tell, ask, convertedBreadcrumbs, request, messagesOut,stats))).pure[Future]
              case AskResult.Success(result) =>
                f(result).map{ (if (purgeStateUponCompletion) DB.empty else dbOut, _) }
            }
        }
      }
    }
  }

}
