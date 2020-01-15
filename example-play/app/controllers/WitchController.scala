package controllers

import scala.language.higherKinds

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.witchcraft._, common.web._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import play.twirl.api.Html
import scalatags.Text.all._

@Singleton
class WitchController @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController with ControllerHelpers with I18nSupport {

  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    UnsafePersistence()

  lazy val interpreter = HmrcPlayInterpreter(this, messagesApi)

  import interpreter._

  def familiarProgram[F[_]: cats.Monad](
    existing: List[Familiar],
    editIndex: Option[Int],
    messages: UniformMessages[Tag],
    rule: validation.Rule[Familiar]
  )(
    int: Language[F, NilTypes, Boolean :: String :: NilTypes]
  ): F[Familiar] = {
    import int._
    val existingCat: Option[Familiar.Cat] = editIndex.map{existing(_)} collect {
      case c: Familiar.Cat => c
    }

    for {
      name <- ask[String]("fam-name", default = existingCat.map{_.name})
      isBlack <- ask[Boolean]("fam-isblack", default = existingCat.map{_.isBlack})
    } yield (Familiar.Cat(name, isBlack))
  }

  implicit def familiarListing(
    implicit request: Request[AnyContent]
  ) = interpreter.listingPageWM[Familiar](
    familiarProgram[interpreter.WM](_,_,_,_)(create[NilTypes, Boolean :: String :: NilTypes](interpreter.messages(request)))
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

