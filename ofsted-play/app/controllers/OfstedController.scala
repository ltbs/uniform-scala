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
import ltbs.uniform.gformsparser._
import play.api.i18n._
import ofsted.Programs.cs3

@Singleton
class OfstedController @Inject()(implicit messages: MessagesApi) extends Controller with PlayInterpreter with I18nSupport {

  implicit val messagesApi: MessagesApi = messages.add(cs3.messages)


    val stringForm: WebMonadForm[String] = new WebMonadForm[String] {
    def decode(out: Encoded): String = out
    def encode(in: String): Encoded = in
    def playForm(key: String, validation: String => Validated[ValidationError,String]): Form[String] = Form(single(key -> nonEmptyText))
    def render(key: String, existing: ValidatedData[String], request: Request[AnyContent]): Html = {
//      val errors: Option[List[String]] = existing.map{_.fold(_.pure[List], _ => List.empty[String])};
//      val e1: Option[String] = existing.flatMap{_.toOption}
      val form = existing match {
        case Some(Validated.Invalid(e)) => Form(single(key -> nonEmptyText)).withError("", e)
        case _ => Form(single(key -> nonEmptyText))
      }

      implicit val r: Request[AnyContent] = request
      views.html.string(key, form)
    }
  }

  val dateForm: WebMonadForm[Date] = new WebMonadForm[Date] {
    def decode(out: Encoded): Date = Date.parse(out)
    def encode(in: Date): Encoded = in.toString

    val mapping: Mapping[Date] = tuple(
      "day" -> number.verifying("error.start-day.invalid", d => d > 0 && d <= 31),
      "month" -> number.verifying("error.start-month.invalid", d => d > 0 && d <= 12),
      "year" -> number.verifying("error.start-year.invalid", d => d >= 1900 && d < 2100)
    ).verifying(
      "error.date.invalid",
      x => x match { case (d, m, y) => Try(Date.of(y, m, d)).isSuccess }
    ).transform({ case (d, m, y) =>
                  Date.of(y, m, d) }, d => (d.getDayOfMonth, d.getMonthValue, d.getYear)
    )

    def playForm(key: String, validation: Date => Validated[ValidationError,Date]): Form[Date] = Form(mapping)
    def render(key: String, existing: ValidatedData[Date], request: Request[AnyContent]): Html = {
      val errors: Option[List[String]] = existing.map{_.fold(_.pure[List], _ => List.empty[String])};
      val e1: Option[Date] = existing.flatMap{_.toOption}
      val form = existing match {
        case Some(Validated.Invalid(e)) => Form(mapping).withError("", e)
        case _ => Form(mapping)
      }
      implicit val r: Request[AnyContent] = request
      views.html.date(key, form)
    }
  }

  val fileForm: WebMonadForm[File] = new WebMonadForm[File] {
    def decode(out: Encoded): File = ???
    def encode(in: File): Encoded = ???

    def mapping: Mapping[File] = ???

    def playForm(key: String, validation: File => Validated[ValidationError,File]): Form[File] = Form(mapping)
    def render(key: String, existing: ValidatedData[File], request: Request[AnyContent]): Html = {
      val errors: Option[List[String]] = existing.map{_.fold(_.pure[List], _ => List.empty[String])};
      val e1: Option[File] = existing.flatMap{_.toOption}
      val form = existing match {
        case Some(Validated.Invalid(e)) => Form(mapping).withError("", e)
        case _ => Form(mapping)
      }
      implicit val r: Request[AnyContent] = request
      views.html.file(key, form)
    }
  }

  val unitForm: WebMonadForm[Unit] = new WebMonadForm[Unit] {
    def decode(out: Encoded): Unit = ()
    def encode(in: Unit): Encoded = "!"

    def mapping: Mapping[Unit] = ???

    def playForm(key: String, validation: Unit => Validated[ValidationError,Unit]): Form[Unit] = Form(mapping)
    def render(key: String, existing: ValidatedData[Unit], request: Request[AnyContent]): Html = {
      val errors: Option[List[String]] = existing.map{_.fold(_.pure[List], _ => List.empty[String])};
      val e1: Option[Unit] = existing.flatMap{_.toOption}
      val form = existing match {
        case Some(Validated.Invalid(e)) => Form(mapping).withError("", e)
        case _ => Form(mapping)
      }

      implicit val r: Request[AnyContent] = request
      views.html.unit(key, form)
    }
  }

  val addressForm: WebMonadForm[Address] = new WebMonadForm[Address] {
    def decode(out: Encoded): Address = (out.head,out.tail) match {
      case ('D', d) =>
        val (l :+ postcode) = d.split(",").toList
        DomesticAddress(l(0),l.get(1),l.get(2),l.get(3),postcode)
      case ('I', i) =>
        val (l :+ country) = i.split(",").toList
        InternationalAddress(l(0),l.get(1),l.get(2),l.get(3),country)
    }
    def encode(in: Address): Encoded = in match {
      case d: DomesticAddress =>
        "D" ++ List(d.line1.some, d.line2, d.line3, d.line4, d.postcode.some).flatten.mkString(",")
      case i: InternationalAddress =>
        "I" ++ List(i.line1.some, i.line2, i.line3, i.line4, i.country.some).flatten.mkString(",")
    }

    implicit class RichA[A](in: A) {
      def emptyUnless(pred: A => Boolean): Option[A] =
        if (pred(in)) in.some else None
    }

    def fuckYou(in: Option[String]): String = in.getOrElse("")

    lazy val mapping: Mapping[Address] = dmapping.transform(
      (x => (x :Address)),
      _.asInstanceOf[DomesticAddress]
    )

    lazy val dmapping: Mapping[DomesticAddress] = Forms.mapping(
    "line1" -> nonEmptyText,
    "line2" -> text.transform((_.emptyUnless(_.trim.nonEmpty)), fuckYou),
    "line3" -> text.transform(_.emptyUnless(_.trim.nonEmpty), fuckYou),
    "line4" -> text.transform(_.emptyUnless(_.trim.nonEmpty), fuckYou),
    "postcode" -> nonEmptyText
  )(DomesticAddress.apply)(DomesticAddress.unapply)

    def playForm(key: String, validation: Address => Validated[ValidationError,Address]): Form[Address] = Form(mapping)
    def render(key: String, existing: ValidatedData[Address], request: Request[AnyContent]): Html = {
      val errors: Option[List[String]] = existing.map{_.fold(_.pure[List], _ => List.empty[String])};
      val e1: Option[Address] = existing.flatMap{_.toOption}
      val form = existing match {
        case Some(Validated.Invalid(e)) => Form(mapping).withError("", e)
        case _ => Form(mapping)
      }

      implicit val r: Request[AnyContent] = request
      views.html.address(key, form)
    }
  }

  val stringRadios: WebMonadSelectPage[String] = new WebMonadSelectPage[String]{
    def decode(out: Encoded): String = out
    def encode(in: String): Encoded = in
    def playForm(key: String,validation: String => Validated[ValidationError,String]): Form[String] = Form(single(key -> nonEmptyText))
    def render(key: String,options: Set[String],existing: ValidatedData[String],request: Request[AnyContent]): Html = {
      implicit val r: Request[AnyContent] = request

      val form = existing match {
        case Some(Validated.Invalid(e)) => Form(single(key -> nonEmptyText)).withError("", e)
        case _ => Form(single(key -> nonEmptyText))
      }

      views.html.radios(key, options, existing.flatMap(_.toOption), form)
    }

    def toHtml(in: String): Html = Html(in)
  }

  def optionally[A](inner: WebMonadForm[A]): WebMonadForm[Option[A]] = new WebMonadForm[Option[A]] {

    def decode(out: Encoded): Option[A] = out.toList match {
      case 'N' :: Nil => None
      case 'Y':: innerEncoded => Some(inner.decode(innerEncoded.mkString))
    }

    def encode(in: Option[A]): Encoded = in match {
      case None => "N"
      case Some(a) => "Y" ++ inner.encode(a)
    }

    def mapping: Mapping[Option[A]] = ???

    def playForm(key: String, validation: Option[A] => Validated[ValidationError,Option[A]]): Form[Option[A]] = ???
    def render(key: String, existing: ValidatedData[Option[A]], request: Request[AnyContent]): Html = ???
  }



  def cs3action(key: String) = Action.async { implicit request =>

    runWeb(
      program = cs3.program[FxAppend[cs3.Stack, PlayStack]]
        .useForm(stringForm)
        .useForm(dateForm)
        .useForm(fileForm)
        .useForm(unitForm)
        .useForm(addressForm)
        .useForm(optionally(stringForm))
        .useSelectPage(stringRadios),
      key,
      request,
      persistence
    )(
      a => Future.successful(Ok(""))
    )
  }

  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty
    def dataGet: Future[DB] = Future.successful(data)
    def dataPut(dataIn: DB): Future[Unit] =
      Future(data = dataIn).map{_ => ()}
  }

}
