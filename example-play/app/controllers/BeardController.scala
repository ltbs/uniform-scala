package controllers


import cats.implicits._
import cats.kernel.Monoid
import javax.inject._
import ltbs.uniform._
import ltbs.uniform.web._
import ltbs.uniform.web.parser._
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.sampleprograms.BeardTax._
import ltbs.uniform.widgets.govuk._
import org.atnos.eff._
import play.api._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import play.twirl.api.Html

import InferParser._

@Singleton
class BeardController @Inject()(implicit val messagesApi: MessagesApi) extends Controller with PlayInterpreter2 with I18nSupport {

  def messages(request: Request[AnyContent]): Messages =
    BestGuessMessages(convertMessages(messagesApi.preferred(request)))

  def renderForm(
    key: List[String],
    errors: ErrorTree,
    form: Html,
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messagesIn: Messages
  ): Html = {
    views.html.chrome(key.last, errors, form, breadcrumbs)(messagesIn, request)
  }

  def listingPage[A](
    key: List[String],
    errors: ErrorTree,
    elements: List[A],
    messages: Messages
  )(implicit evidence$1: Htmlable[A]): Html = ???

  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty
    def dataGet: Future[DB] = Future.successful(data)
    def dataPut(dataIn: DB): Future[Unit] =
      Future(data = dataIn).map{_ => ()}
  }

  implicit def renderTell: (Unit, String) => Html = {case _ => Html("")}

  def beardAction(key: String) = {
    implicit val keys: List[String] = key.split("/").toList
    Action.async { implicit request =>
      runWeb(
        program = program[FxAppend[TestProgramStack, PlayStack]]
          .useForm(PlayForm.automatic[Unit, Option[MemberOfPublic]])
          .useForm(PlayForm.automatic[Unit, BeardStyle])
          .useForm(PlayForm.automatic[Unit, BeardLength]),
        persistence
      )(
        a => Future.successful(Ok(s"You have £$a to pay"))
      )
    }
  }

}
