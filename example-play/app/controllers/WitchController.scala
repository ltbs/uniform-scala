package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.witchcraft._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._

@Singleton
class WitchController @Inject()(
  implicit val messagesApi: MessagesApi,
  ec:ExecutionContext
) extends ControllerHelpers with I18nSupport {

  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    DebugPersistence(UnsafePersistence())

  lazy val interpreter = HmrcPlayInterpreter(this, messagesApi)

  import interpreter._
  // implicit val evidenceListing = interpreter.listingPage[examples.witchcraft.Evidence]
  //   {x => play.twirl.api.Html(x.toString) }

  implicit val familiarListing = interpreter.listingPage[examples.witchcraft.Familiar]
    {x => play.twirl.api.Html(x.toString) }

  def reportWitch(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    val playProgram = witchProgram[interpreter.WM](
      create[TellTypes, AskTypes](interpreter.messages(request))
    )

    playProgram.run(targetId) {
      case WitchReport(acc, _, fam) => Ok(
        s"Thankyou for your report, ${acc.name} and their ${fam.size} familiars will now be put to death."
      ).pure[Future]
    }
  }
}
