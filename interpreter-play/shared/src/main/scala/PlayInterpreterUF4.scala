package ltbs.uniform
package interpreters.playframework

import play.api._,mvc._,http.Writeable
import shapeless.{Path ⇒ _,_}
import concurrent.{ExecutionContext, Future}
import cats.data.{EitherT, RWST}
import cats.implicits._
import cats.Monoid
import common.web._

abstract class PlayInterpreter[Html: Writeable: Monoid](
  implicit ec: ExecutionContext
) extends InferFormField[Html] with Compatibility.PlayController {

  type PlayAsk[A] = GenericWebAsk[A, Html]
  type PlayTell[A] = GenericWebTell[A, Html]

  def messages(
    request: Request[AnyContent],
    customContent: Map[String,(String,List[Any])]
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

  class FuturePlayInterpreter[
    SupportedTell <: HList,
    SupportedAsk  <: HList
  ](
    implicit tellSummoner : TypeclassList[SupportedTell, PlayTell],
    askSummoner  : TypeclassList[SupportedAsk, PlayAsk]
  ) extends Language[WebMonad, SupportedTell, SupportedAsk] {
    override def interact[Tell, Ask](
      id: String,
      t: Tell,
      default: Option[Ask],
      validation: List[List[Rule[Ask]]],
      customContent: Map[String,(String,List[Any])]
    )(
      implicit selectorTell : IndexOf[SupportedTell, Tell],
      selectorAsk : IndexOf[SupportedAsk, Ask]
    ): WebMonad[Ask] = {
      val asker: PlayAsk[Ask] = askSummoner.forType[Ask]
      val teller: PlayTell[Tell] = tellSummoner.forType[Tell]
      EitherT[WebInner, Result, Ask] {
        RWST { case ((config, currentId, request), (path, db)) ⇒
          val input: Option[Input] = request.body.asFormUrlEncoded.map{
            _.map{ case (k,v) ⇒ (k.split("[.]").toList.dropWhile(_.isEmpty), v.toList) }
          }
          import AskResult._

          val localMessages = messages(request, customContent)
          val tellHtml = teller.render(t, id, localMessages)
          asker.page(
            targetId = id.split("/").toList.dropWhile(_.isEmpty),
            currentId,
            default,
            validation,
            config,
            input,
            path,
            db,
            localMessages
          ).map { case PageOut(newPath, newDb, output) ⇒
              val result: Either[Result,Ask] = output match {
                case GotoPath(redirection) ⇒
                  val path = relativePath(currentId, redirection)
                  log.info(s"redirecting to $path")
                  Left(Redirect(path))
                case Payload(askHtml, errors) ⇒ Left(Ok(
                  pageChrome(currentId, errors, tellHtml, askHtml, path, request, localMessages)
                ))
                case Success(out) ⇒ Right(out)
              }

              ((),(newPath,newDb),result)
          }
        }
      }

    }
  }

  def run[A, B <: Request[AnyContent]](
    program: WebMonad[A],
    id: String,
    config: JourneyConfig = ""
  )(
    terminalFold: A ⇒ Future[Result]
  )(
    implicit request: B,
    persistence: PersistenceEngine[B]
  ): Future[Result] = {
    val targetId: List[String] = id.split("/").toList.dropWhile(_.isEmpty)

    persistence(request){ db ⇒
      program.value.run((config,targetId,request),(Monoid[Path].empty,db)) flatMap {
        case (_, (_,newDb), Left(result)) ⇒
          Future.successful((newDb,result))
        case (_, (_,newDb), Right(value)) ⇒
          terminalFold(value).map{result ⇒ (newDb,result)}
      }
    }
  }


}
