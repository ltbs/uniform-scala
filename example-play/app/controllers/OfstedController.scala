package controllers

import cats.kernel.Monoid
import javax.inject._
import play.api._
import play.api.data._, Forms._
import play.api.mvc._
import play.twirl.api.Html
import scala.util.Try

import ltbs.uniform.webmonad._
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

import ltbs.uniform.test.LitreageTest._

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

  val booleanForm = new WebMonadForm[Boolean] {
    def decode(out: Encoded): Boolean = out == "true"
    def encode(in: Boolean): Encoded = in.toString
    def playForm(key: String, validation: Boolean => Validated[ValidationError,Boolean]): Form[Boolean] = Form(single(key -> boolean))
    def render(key: String, existing: ValidatedData[Boolean], request: Request[AnyContent]): Html = {
      val form = existing match {
        case Some(Validated.Invalid(e)) => Form(single(key -> boolean)).withError("", e)
        case _ => Form(single(key -> boolean))
      }

      implicit val r: Request[AnyContent] = request
      views.html.boolean(key, form)
    }
  }

  val litresForm = new WebMonadForm[Litres] {

    private def lmapping(key: String): Mapping[Litres] = tuple(
      s"$key.lower" -> longNumber,
      s"$key.upper" -> longNumber
    )

    private def lform(key: String) = Form(lmapping(key))

    def decode(out: Encoded): Litres = {
      val (a::b::_) = out.split(",").toList
      (a.toInt,b.toInt)
    }

    def encode(in: Litres): Encoded = in._1 + "," + in._2
    def playForm(key: String, validation: Litres => Validated[ValidationError,Litres]): Form[Litres] = lform(key)
    def render(key: String, existing: ValidatedData[Litres], request: Request[AnyContent]): Html = {
      val formPop = existing match {
        case Some(Validated.Invalid(e)) => lform(key).withError("", e)
        case _ => lform(key)
      }

      implicit val r: Request[AnyContent] = request
      views.html.litres(key, formPop)
    }
  }

  def cs3action(key: String) = Action.async { implicit request =>
    runWeb(
      program = program[FxAppend[TestProgramStack, PlayStack]]
        .useForm(litresForm)
        .useForm(booleanForm),
      key,
      request,
      persistence
    )(
      a => Future.successful(Ok(a))
    )
  }

  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty
    def dataGet: Future[DB] = Future.successful(data)
    def dataPut(dataIn: DB): Future[Unit] =
      Future(data = dataIn).map{_ => ()}
  }

}
