package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.beardtax._
import common.web.WebMonad
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import scalatags.Text.all._

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
) extends BaseController with ControllerHelpers with I18nSupport {

  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    DebugPersistence(UnsafePersistence())

  def adaptedHod = new Hod[WebMonad[?, Tag]] {
    val inner = new HodConnector
    def costOfBeard(beardStyle: BeardStyle, length: BeardLength): WebMonad[Int, Tag] =
      common.web.FutureAdapter[Tag].alwaysRerun.apply(inner.costOfBeard(beardStyle, length))
  }


  lazy val interpreter = HmrcPlayInterpreter(this, messagesApi)

  def beardAction(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    import interpreter._

    val playProgram = beardProgram[interpreter.WM](
      create[TellTypes, AskTypes](interpreter.messages(request)),
      adaptedHod
    )

    playProgram.run(targetId, purgeStateUponCompletion = true) {
      i: Int => Ok(s"$i").pure[Future]
    }

  }

}
