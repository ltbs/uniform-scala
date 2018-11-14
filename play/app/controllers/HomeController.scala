package controllers

import cats.kernel.Monoid
import javax.inject._
import play.api._
import play.api.data._, Forms._
import play.api.mvc._
import play.twirl.api.Html
import scala.util.Try

import ltbs.uniform.webmonad._
import ltbs.uniform.test.LitreageTest._
import org.atnos.eff._, all._, syntax.all._, future._
import cats.data._
import cats.implicits._
import org.atnos.eff.syntax.future._

import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.Validated

@Singleton
class HomeController @Inject() extends Controller with PlayInterpreter {

  def index(page: Long) = Action { implicit request =>
    Ok(views.html.index(1L,1000L, List(("One", "1"))))
  }

  def user(nino: String, page: Long) = Action { implicit request =>
    Ok(views.html.user("user", nino, page))
  }

  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty

    def dataGet: Future[DB] = Future.successful(data)

    def dataPut(dataIn: DB): Future[Unit] = 
      Future(data = dataIn).map{_ => ()}
  }

  def test(key: String) = Action.async{ request => 
    runWeb(
      program = program[FxAppend[TestProgramStack, PlayStack]]
        .useForm(litreageForm)
        .useForm(booleanForm),
      key,
      request,
      persistence
    )(
      a => Future.successful(Ok(a))
    )
  }

  val litreageForm = new WebMonadForm[Litres] {
    def decode(out: Encoded): Litres = {
      val (l::h::Nil) = out.split(",").map{_.toLong}.toList
      (l,h)
    }

    def encode(in: Litres): Encoded = s"${in._1},${in._2}"

    val litreage: Mapping[Long] = text
      .verifying("error.litreage.required", _.nonEmpty)
      .transform[String](_.replaceAll(",", ""), _.toString)
      .verifying("error.litreage.numeric", l => Try(BigDecimal.apply(l)).isSuccess)
      .transform[BigDecimal](BigDecimal.apply, _.toString)
      .verifying("error.litreage.numeric", _.isWhole)
      .verifying("error.litreage.max", _ <= 9999999999999L)
      .verifying("error.litreage.min", _ >= 0)
      .transform[Long](_.toLong, BigDecimal.apply)

    val litreagePair: Mapping[(Long,Long)] =
      tuple("lower" -> litreage, "higher" -> litreage)

    def playForm(validation: Litres => Validated[ValidationError,Litres]): Form[Litres] = {
      Form(litreagePair)
    }

    def render(key: String, existing: ValidatedData[Litres], request: Request[AnyContent]): Html = {
      views.html.litres(key, Form(litreagePair))
    }

  }

  val booleanForm = new WebMonadForm[Boolean] {
    val bool: Mapping[Boolean] = optional(boolean)
      .verifying("error.radio-form.choose-option", _.isDefined)
      .transform(_.getOrElse(false),{x: Boolean => Some(x)})

    def decode(out: Encoded): Boolean = out == "yes"
    def encode(in: Boolean): Encoded = if (in) "yes" else "no"
    def playForm(validation: Boolean => Validated[ValidationError,Boolean]): Form[Boolean] = Form(bool)
    def render(key: String, existing: ValidatedData[Boolean], request: Request[AnyContent]): Html = {
      val errors: Option[List[String]] = existing.map{_.fold(_.pure[List], _ => List.empty[String])};
      val e1: Option[Boolean] = existing.flatMap{_.toOption}
      val form = existing match {
        case Some(Validated.Invalid(e)) => Form(bool).withError("", e)
        case _ => Form(bool)
      }

      views.html.radios(key, List(true,false).map(_.toString), e1.map{_.toString}, form)(request)
    }
  }

}
