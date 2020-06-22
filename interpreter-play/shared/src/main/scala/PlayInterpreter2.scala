package ltbs.uniform

package interpreters.playframework

import scala.concurrent.ExecutionContext
import ltbs.uniform.common.web._
import play.api._,mvc._
import play.api.http.Writeable
import scala.concurrent.Future

trait PlayInterpreter2[Html] extends Results {

  def messagesForRequest[C <: AnyContent](
    request: Request[C]
  ): UniformMessages[Html]

  def forRequest[C <: AnyContent](request: Request[C]) = {
    val that = this
    new GenericWebInterpreter2[Html] {
      def empty: Html = that.empty
      def messages: UniformMessages[Html] = messagesForRequest(request)
      def unitAsk: WebMonadConstructor[Unit,Html] =
        that.unitAsk
    }
  }

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

  def empty: Html

  def unitAsk: WebMonadConstructor[Unit, Html]

  implicit class PlayWebMonad[A, Req <: Request[AnyContent]](wm: WebMonad[A, Html]) {
    import cats.implicits._
    def runSync(path: String)(
      f: A => Result
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req],
      ec: ExecutionContext,
      writeable: Writeable[Html]
    ): Future[Result] = run(path){f.map{_.pure[Future]}}

    def run(path: String, purgeStateUponCompletion: Boolean = false, config: JourneyConfig = JourneyConfig())(
      f: A => Future[Result]
    )(implicit
      request: Req,
      persistence: PersistenceEngine[Req],
      ec: ExecutionContext,
      writeable: Writeable[Html]
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

      val data: Option[Input] = request.body.asFormUrlEncoded.map {
        _.map{ case (k,v) => (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
      }

      persistence(request) { db =>
        wm(PageIn(id, Nil, data, db, Nil, config)) flatMap {
          case common.web.PageOut(breadcrumbs, dbOut, pageOut, _, _) =>
            pageOut match {
              case AskResult.GotoPath(targetPath) =>
                val path = baseUrl + targetPath.mkString("/")
                (dbOut, Redirect(path)).pure[Future]
              case AskResult.Payload(tell, ask, errors, messagesOut, stats) =>
                val convertedBreadcrumbs = breadcrumbs.map { c => 
                  baseUrl + c.mkString("/")
                }
                (db, Ok(pageChrome(breadcrumbs.head, errors, tell, ask, convertedBreadcrumbs, request, messagesOut,stats))).pure[Future]
              case AskResult.Success(result) =>
                f(result).map{ (if (purgeStateUponCompletion) DB.empty else dbOut, _) }
            }
        }
      }
    }
  }  

}
