package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.witchcraft._, common.web._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import scalatags.Text.all._
import com.github.ghik.silencer.silent
import scala.language.higherKinds

@Singleton
class WitchController @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController with ControllerHelpers with I18nSupport with HmrcPlayInterpreter {

  @silent("never used")
  def familiarProgram(
    existing: List[Familiar],
    editIndex: Option[Int],
    messages: UniformMessages[Tag],
    rule: validation.Rule[Familiar]
  ) = {

    val existingCat: Option[Familiar.Cat] = editIndex.map{existing(_)} collect {
      case c: Familiar.Cat => c
    }

    for {
      name <- ask[String]("fam-name", default = existingCat.map{_.name})
      isBlack <- ask[Boolean]("fam-isblack", default = existingCat.map{_.isBlack})
    } yield (Familiar.Cat(name, isBlack))

  }

  def reportWitch(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    implicit val persistence: PersistenceEngine[Request[AnyContent]] =
      SessionPersistence("witches")

    interpret(witchProgram).runSync(targetId) {
      case WitchReport(acc, _, fam) => Ok(
        s"Thankyou for your report, ${acc.name} and their ${fam.size} familiars will now be put to death."
      )
    }
  }


}
