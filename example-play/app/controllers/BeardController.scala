package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.beardtax._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import scalatags.Text.all._
import ltbs.uniform.examples.Widgets._
import ltbs.uniform.common.web.FutureAdapter

class HodConnector(implicit ec: ExecutionContext) extends Hod[Future] {
  def costOfBeard(beardStyle: BeardStyle, length: BeardLength): Future[Int] =
    Future{
      Thread.sleep(2000)
      IdDummyHod.costOfBeard(beardStyle, length)
    }
}

@Singleton
class BeardController2 @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController with ControllerHelpers with I18nSupport with HmrcPlayInterpreter {

  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    DebugPersistence(UnsafePersistence())

  def beardAction(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    implicit val persistence: PersistenceEngine[Request[AnyContent]] =
      SessionPersistence("beard")

    implicit val futureAdapter = FutureAdapter[Tag].alwaysRerun

    interpret(beardProgram(new HodConnector)).runSync(targetId){ 
      i: Int => Ok(s"$i")
    }
  }
}

