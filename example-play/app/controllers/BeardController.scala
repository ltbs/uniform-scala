package controllers

import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.beardtax._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._, duration._
import scalatags.Text.all._

@Singleton
class BeardController @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController with ControllerHelpers with I18nSupport with HmrcPlayInterpreter {

  def beardAction(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    implicit val persistence = SessionPersistence("beard")

    val adaptor = FutureAdaptor.rerunOnStateChange[Tag](1.minute)
    import adaptor._

    interpret(beardProgram(new HodConnector)).runSync(targetId){ 
      i: Int => Ok(s"$i")
    }
  }
}
