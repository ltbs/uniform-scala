package controllers

import cats.kernel.Monoid
import javax.inject._
import play.api._
import play.api.data._, Forms._
import play.api.mvc._
import play.twirl.api.Html
import scala.util.Try

import ltbs.uniform.interpreters.playframework._
import org.atnos.eff._, all._, syntax.all._, future._
import cats.data._
import cats.implicits._
import org.atnos.eff.syntax.future._

import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.Validated
import java.time.{LocalDate => Date}
import java.io.File
import play.api.i18n._

import ltbs.uniform.sampleprograms.LitreageTest._
import ltbs.uniform.datapipeline.{Messages => _, _}
import ltbs.uniform.widgets.govuk._

import InferParser._

@Singleton
class OfstedController @Inject()(implicit val messagesApi: MessagesApi) extends Controller with PlayInterpreter with I18nSupport {

  // val stringForm: WebMonadForm[String] = new WebMonadForm[String] {
  //   def decode(out: Encoded): String = out
  //   def encode(in: String): Encoded = in
  //   def playForm(key: String, validation: String => Validated[ValidationError,String]): Form[String] = Form(single(key -> nonEmptyText))
  //   def render(key: String, existing: ValidatedData[String], request: Request[AnyContent]): Html = {
  //     val form = existing match {
  //       case Some(Validated.Invalid(e)) => Form(single(key -> nonEmptyText)).withError("", e)
  //       case _ => Form(single(key -> nonEmptyText))
  //     }

  //     implicit val r: Request[AnyContent] = request
  //     views.html.string(key, form)
  //   }
  // }


  def cs3action(key: String) = {

    def inferForm[A](
      implicit 
      parser: DataParser[A],
      html: HtmlForm[A],
      request: Request[AnyContent]
    ) = inferWebMonadForm[A](views.html.chrome.apply)

    Action.async { implicit request =>
      runWeb(
        program = program[FxAppend[TestProgramStack, PlayStack]]
          .useForm(inferForm[Litres])
          .useForm(inferForm[Boolean]),
        key,
        request,
        persistence
      )(
        a => Future.successful(Ok(a))
      )
    }
  }

  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty
    def dataGet: Future[DB] = Future.successful(data)
    def dataPut(dataIn: DB): Future[Unit] =
      Future(data = dataIn).map{_ => ()}
  }

}
