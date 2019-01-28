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
class BeardController @Inject()(implicit val messagesApi: MessagesApi) extends Controller with PlayInterpreter with I18nSupport {

  def messages(request: Request[AnyContent]): Messages = convertMessages(messagesApi.preferred(request))

  def renderForm(key: String, errors: ErrorTree, form: Html, breadcrumbs: List[String], request: Request[AnyContent], messagesIn: Messages): Html = {
    views.html.chrome(key, errors, form, breadcrumbs)(messagesIn, request)
  }

  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty
    def dataGet: Future[DB] = Future.successful(data)
    def dataPut(dataIn: DB): Future[Unit] =
      Future(data = dataIn).map{_ => ()}
  }

  def beardAction(implicit key: String) = {

    Action.async { implicit request =>
      runWeb(
        program = program[FxAppend[TestProgramStack, PlayStack]]
          .useForm(PlayForm.automatic[Option[MemberOfPublic]])
          .useForm(PlayForm.automatic[BeardStyle])
          .useForm(PlayForm.automatic[BeardLength]),
        persistence
      )(
        a => Future.successful(Ok(s"You have £$a to pay"))
      )
    }
  }

}
