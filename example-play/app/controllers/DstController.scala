package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.dst._
import common.web.WebMonad
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import scalatags.Text.all._

@Singleton class DstController @Inject()(
  implicit val messagesApi: MessagesApi,
  ec: ExecutionContext
) extends ControllerHelpers with I18nSupport {


  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    DebugPersistence(UnsafePersistence())

  lazy val interpreter = HmrcPlayInterpreter(this, messagesApi)

  def register(targetId: String) = Action.async { implicit request: Request[AnyContent] =>
    import interpreter._
    val playProgram = registrationJourney[interpreter.WM](
      create[TellTypes, AskTypes](interpreter.messages(request))
    )
    playProgram.run(targetId, purgeStateUponCompletion = true) {
      i: Registration => Ok(s"$i").pure[Future]
    }
  }

  def returnForm(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    import interpreter._
    val playProgram = returnJourney[interpreter.WM](
      create[TellTypes, AskTypesReturn](interpreter.messages(request))
    )
    playProgram.run(targetId, purgeStateUponCompletion = true) {
      i: ReturnForm => Ok(s"$i").pure[Future]
    }
  }


}
