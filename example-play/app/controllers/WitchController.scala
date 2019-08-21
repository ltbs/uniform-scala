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
  implicit val evidenceListing = interpreter.listingPage[examples.witchcraft.Evidence]
    {x => play.twirl.api.Html(x.toString) }

  def familiarProgram[F[_]: cats.Monad](
    existing: List[Familiar],
    editIndex: Option[Int],
  )(
    int: Language[F, NilTypes, Boolean :: String :: NilTypes]
  ): F[Familiar] = {
    import int._
    for {
      name <- ask[String]("fam-name")
      isBlack <- ask[Boolean]("fam-isblack")
    } yield (Familiar.Cat(name, isBlack))
  }

  implicit def familiarListing(implicit request: Request[AnyContent]) = interpreter.listingPageWM[Familiar](
    {(x: Familiar, i: Int) => play.twirl.api.Html(x.toString) },
    familiarProgram[interpreter.WM](_,_)(create[NilTypes, Boolean :: String :: NilTypes](interpreter.messages(request)))
  )

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
