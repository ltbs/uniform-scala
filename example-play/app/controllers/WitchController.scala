package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.witchcraft._, common.web._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import play.twirl.api.Html
import scalatags.Text.all._
import ltbs.uniform.examples.Widgets._

@Singleton
class WitchController @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController with ControllerHelpers with I18nSupport with HmrcPlayInterpreter {

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

  // implicit def familiarListing(
  //   implicit request: Request[AnyContent]
  // ) = interpreter.listingPageWM[Familiar](
  //   familiarProgram[interpreter.WM](_,_,_,_)(create[NilTypes, Boolean :: String :: NilTypes](interpreter.messages(request)))
  // )

  def reportWitch(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    implicit val persistence: PersistenceEngine[Request[AnyContent]] =
      SessionPersistence("witches")

//    implicit val e: WebInteraction[List[Evidence], Tag] = singlePageForm[Evidence]
//    implicit def ff: FormField[List[Evidence], Tag] = ???

    interpret(witchProgram).runSync(targetId) {
      case WitchReport(acc, _, fam) => Ok(
        s"Thankyou for your report, ${acc.name} and their ${fam.size} familiars will now be put to death."
      )
    }
  }


}
