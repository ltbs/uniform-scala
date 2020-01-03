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
  implicit val messagesApi: MessagesApi,
  ec:ExecutionContext
) extends ControllerHelpers with I18nSupport {

  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    UnsafePersistence()

  lazy val interpreter = HmrcPlayInterpreter(this, messagesApi)

  import interpreter._

  def main(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    val playProgram = subjourneyProg[interpreter.WM](
      create[TellTypes, AskTypes](interpreter.messages(request))
    )

    playProgram.run(targetId) {
      _ => Future(Ok("Ta!"))
    }
  }
}
