package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework.{twirlUnitField => _, _}, examples.beardtax._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent.{Future, ExecutionContext}
import scalatags.Text.all._
import ltbs.uniform.common.web._

class HodConnector(implicit ec: ExecutionContext) extends Hod[Future] {

  def recordBeardHeight(height: Int): Future[Unit] = Future{
    println(s"HOD CALL: $height")
  }

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

    import FutureAdaptor.rerunOnPriorStateChange

    interpret(beardProgram(new HodConnector)).runSync(targetId){ 
      i: Int => Ok(s"$i")
    }
  }
}


