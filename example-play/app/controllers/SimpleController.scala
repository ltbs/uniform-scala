package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import validation._

sealed trait Maybe
object Maybe {
  case object Yes extends Maybe
  case object No extends Maybe
}

@Singleton
class SimpleController @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController
    with ControllerHelpers
    with I18nSupport
    with HmrcPlayInterpreter {

  val journey = for {
    x2 <- ask[String]("x2")
  } yield (x2)

  def simpleAction(targetId: String) = Action.async {
    implicit request: Request[AnyContent] =>

    implicit val persistence: PersistenceEngine[Request[AnyContent]] =
      SessionPersistence("simple")

    interpret(journey).runSync(targetId){ onCompletion =>
      Ok(onCompletion.toString)
    }
  }

}
