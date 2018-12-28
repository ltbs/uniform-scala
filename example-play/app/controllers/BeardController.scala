package controllers


import cats.implicits._
import cats.kernel.Monoid
import javax.inject._
import ltbs.uniform.datapipeline.{Messages => _, _}
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.sampleprograms.BeardTax._
import ltbs.uniform.widgets.govuk._
import org.atnos.eff._
import play.api._
import play.api.i18n._
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

import InferParser._

@Singleton
class BeardController @Inject()(implicit val messagesApi: MessagesApi) extends Controller with PlayAnnotatingInterpreter with I18nSupport {


  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty
    def dataGet: Future[DB] = Future.successful(data)
    def dataPut(dataIn: DB): Future[Unit] =
      Future(data = dataIn).map{_ => ()}
  }


  def beardAction(key: String) = {

    def inferForm[A](
      implicit
        parser: DataParser[A],
      html: HtmlForm[A],
      request: Request[AnyContent]
    ) = inferWebMonadForm[A](views.html.chrome.apply)

    Action.async { implicit request =>
      runWeb(
        program = program[FxAppend[TestProgramStack, PlayStack]]
          .useForm(inferForm[Option[MemberOfPublic]])
          .useForm(inferForm[BeardStyle])
          .useForm(inferForm[BeardLength]),
        key,
        request,
        persistence
      )(
        a => Future.successful(Ok(s"You have £$a to pay"))
      )
    }
  }

}
