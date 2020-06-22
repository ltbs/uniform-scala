package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import scalatags.Text.all._
import ltbs.uniform.examples.Widgets._
import ScalatagsSupport._
import validation._

@Singleton
class SimpleController @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController
    with ControllerHelpers
    with I18nSupport
    with HmrcPlayInterpreter {

  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    DebugPersistence(UnsafePersistence())

  val journey = for {
    x <- ask[Boolean]("x")
    x2 <- interact[String]("x-back", x)
    y <- if (x) ask[Int]("y", validation = Rule.min(0)) else end("z")
  } yield (x, y)

  def simpleAction(targetId: String) = Action.async {
    implicit request: Request[AnyContent] =>
    
    interpret(journey).runSync(targetId){ onCompletion =>
      Ok("Done")
    }
  }

}
