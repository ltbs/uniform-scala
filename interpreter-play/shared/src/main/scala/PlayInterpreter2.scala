package ltbs.uniform
package interpreters.playframework

import ltbs.uniform.common.web._
import play.api._,mvc._,http.Writeable
import scala.concurrent._
import izumi.reflect.Tag

abstract class PlayInterpreter2[Html: Writeable] extends Results {

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

  def empty: Html

  def unitAsk: WebMonadConstructor[Unit, Html]

  val generic = new GenericWebInterpreter2[Html](empty, _, unitAsk)

  def run[Req <: Request[AnyContent], A: Tag](
    journey: Uniform[_, A, _], 
    path: String,
    purgeStateUponCompletion: Boolean = false,
    config: JourneyConfig = JourneyConfig()
  )(
    f: A => Future[Result]
  )(implicit
    persistence: PersistenceEngine[Req],
    ec: ExecutionContext,
    request: Req    
  ): Future[Result] = {
    val baseUrl = request.path.dropRight(path.size)
    val id = path.split("/", -1).toList

    val data: Option[Input] = request.body.asFormUrlEncoded.map {
      _.map{ case (k,v) => (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
    }

    persistence(request) { db =>
      def pageIn = PageIn(id, Nil, data, db, Nil, config)
        generic(messages(request)).execute(journey)(pageIn) flatMap {
        case common.web.PageOut(breadcrumbs, dbOut, pageOut, _, _) =>
          pageOut match {
            case AskResult.GotoPath(targetPath) =>
              val path = baseUrl + targetPath.mkString("/")
                Future((dbOut, Redirect(path)))
            case AskResult.Payload(tell, ask, errors, messagesOut, stats) =>
              val convertedBreadcrumbs = breadcrumbs.map { c =>
                baseUrl + c.mkString("/")
              }
                Future((db, Ok(pageChrome(breadcrumbs.head, errors, tell, ask, convertedBreadcrumbs, request, messagesOut,stats))))
            case AskResult.Success(result) =>
              f(result).map{ (if (purgeStateUponCompletion) DB.empty else dbOut, _) }
          }
      }
    }
  }


}
