package controllers

import scala.language.higherKinds

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.witchcraft._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import play.twirl.api.Html

@Singleton
class WitchController @Inject()(
  implicit val messagesApi: MessagesApi,
  ec:ExecutionContext
) extends ControllerHelpers with I18nSupport {

  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    DebugPersistence(UnsafePersistence())

  lazy val interpreter = HmrcPlayInterpreter(this, messagesApi)

  implicit val familiarRows = new common.web.ListingTell[Html, Familiar] {
    def apply(rows: List[common.web.ListingTellRow[Familiar]], messages: UniformMessages[Html]): Html = {


      val famRow: common.web.ListingTellRow[Familiar] => String = {
        case common.web.ListingTellRow(value, editLink, deleteLink) =>
          val cell = value match {
            case Familiar.Cat(name, true) => s"Black cat called $name"
            case Familiar.Cat(name, false) => s"Cat called $name"
            case _ => "Non-cat pet"
          }

          s"""<tr><td>$cell</td><td><a href="${editLink}">Edit</a></td><td><a href="${deleteLink}">Delete</a></td></tr>"""
      }

      val h =
        s"""|<table>
            |  <tr><th>Familiar</th><th>Edit</th><th>Delete</th></tr>
            |  ${rows.map(famRow).mkString}
            |</table>""".stripMargin
      Html(h)
    }
  }

  implicit val evidenceRows = new common.web.ListingTell[Html, Evidence] {
    def apply(rows: List[common.web.ListingTellRow[Evidence]], messages: UniformMessages[Html]): Html = {
      ???
    }
  }

  import interpreter._
  implicit val evidenceListing = interpreter.listingPage[examples.witchcraft.Evidence]

  def familiarProgram[F[_]: cats.Monad](
    existing: List[Familiar],
    editIndex: Option[Int],
    messages: UniformMessages[Html]
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

  implicit def familiarListing(implicit request: Request[AnyContent]) = interpreter.listingPageWM[Familiar](
    familiarProgram[interpreter.WM](_,_,_)(create[NilTypes, Boolean :: String :: NilTypes](interpreter.messages(request)))
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
