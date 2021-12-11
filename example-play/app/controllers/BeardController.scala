package controllers

import javax.inject._
import ltbs.uniform._
import interpreters.playframework._
import examples.beardtax._
import ltbs.uniform.common.web.{JourneyConfig, FutureAdapter}
import play.api.i18n.{Messages => _, _}
import play.api.mvc._

import scala.concurrent._
import duration._
import scalatags.Text.all._

@Singleton
class BeardController @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController with ControllerHelpers with I18nSupport with HmrcPlayInterpreter {

  def beardAction(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    implicit val persistence = SessionPersistence("beard")

    val adaptor = FutureAdapter.rerunOnStateChange[Tag](1.minute)
    import adaptor._

    interpret(beardProgram(new HodConnector)).runSync(targetId, config = JourneyConfig(leapAhead = true)){
      i: Int => Ok(s"$i")
    }
  }
}
