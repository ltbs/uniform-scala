package controllers

import scala.language.higherKinds

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.witchcraft.subjourneys._, common.web._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._

@Singleton
class SubjourneyController @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController with ControllerHelpers with I18nSupport with HmrcPlayInterpreter {

  def main(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    implicit val persistence: PersistenceEngine[Request[AnyContent]] =
      SessionPersistence("subjourney")

    interpret(subjourneyProg).run(targetId) {
      _ => Future(Ok("Ta!"))
    }
  }
}
