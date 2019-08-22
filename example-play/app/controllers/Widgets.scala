package controllers

import cats.implicits._

import ltbs.uniform._, interpreters.playframework._
import ltbs.uniform.common.web.FormField
import play.twirl.api.Html
import java.time.LocalDate
import cats.data.Validated

object Widgets extends Widgets

trait Widgets {

  implicit val twirlBigStringField = new FormField[BigString,Html] {
    import shapeless.tag
    def decode(out: Input): Either[ErrorTree,BigString] = {
      val root: Option[BigString] = {
        val asString = out.valueAtRoot
          .flatMap(_.filter(_.trim.nonEmpty).headOption)

        asString.map{tag[BigStringTag][String]}
      }

      root match {
        case None => Left(ErrorMsg("required").toTree)
        case Some(data) => Right(data)
      }
    }

    def encode(in: BigString): Input = Input.one(List(in))
    def render(
      key: List[String],
      breadcrumbs: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: String = data.valueAtRoot.flatMap{_.headOption}.getOrElse("")
      views.html.uniform.textarea(key, existingValue, errors, messages)
    }
  }

  implicit val twirlBooleanField = new FormField[Boolean,Html] {
    def decode(out: Input): Either[ErrorTree,Boolean] = {
      val root: Option[String] = out.valueAtRoot
        .flatMap(_.filter(_.trim.nonEmpty).headOption)

      root match {
        case None => Left(ErrorMsg("required").toTree)
        case Some("TRUE") => Right(true)
        case Some("FALSE") => Right(false)
        case _ => Left(ErrorMsg("bad.value").toTree)
      }
    }

    def encode(in: Boolean): Input = in match {
      case true => Input.one(List("TRUE"))
      case false => Input.one(List("FALSE"))
    }

    def render(
      key: List[String],
      breadcrumbs: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: Option[String] = data.valueAtRoot.flatMap{_.headOption}
      views.html.uniform.radios(key, List("TRUE","FALSE"), existingValue, errors, messages)
    }
  }


  implicit val twirlStringField = new FormField[String,Html] {
    def decode(out: Input): Either[ErrorTree,String] =
      out.valueAtRoot.flatMap(_.headOption).getOrElse("").asRight

    def encode(in: String): Input = Input.one(List(in))
    def render(
      key: List[String],
      breadcrumbs: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: String = data.valueAtRoot.flatMap{_.headOption}.getOrElse("")
      views.html.uniform.string(key, existingValue, errors, messages)
    }
  }

  implicit val twirlNonEmptyStringField: FormField[NonEmptyString, Html] =
    twirlStringField.simap(x =>
      NonEmptyString.fromString(x) match {
        case Some(x) => Right(x)
        case None    => Left(ErrorMsg("required").toTree)
      }
    )(identity)

  implicit val twirlIntField2: FormField[Int,Html] =
    twirlStringField.simap(x =>
      Either.catchOnly[NumberFormatException](x.toInt)
        .leftMap(_ => ErrorMsg("bad.value").toTree)
    )(_.toString)


  implicit val twirlDateField = new FormField[LocalDate,Html] {

    def decode(out: Input): Either[ErrorTree,LocalDate] = {

      def intAtKey(key: String): Validated[ErrorTree, Int] =
        Validated.fromOption(
          out.valueAt(key).flatMap{_.filter(_.trim.nonEmpty).headOption},
          ErrorTree.oneErr(ErrorMsg("required")).prefixWith(key)
        ).andThen{
            x => Validated.catchOnly[NumberFormatException](x.toInt).leftMap(_ => ErrorMsg("badValue").toTree)
        }

      (
        intAtKey("year"),
        intAtKey("month"),
        intAtKey("day")
      ).tupled.toEither.flatMap{ case (y,m,d) =>
        Either.catchOnly[java.time.DateTimeException]{
          LocalDate.of(y,m,d)
        }.leftMap(_ => ErrorTree.oneErr(ErrorMsg("badDate")))
      }
    }

    def encode(in: LocalDate): Input = Map(
        List("year") -> in.getYear(),
        List("month") -> in.getMonthValue(),
        List("day") -> in.getDayOfMonth()
      ).mapValues(_.toString.pure[List])

    def render(
      key: List[String],
      breadcrumbs: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      views.html.uniform.date(
        key,
        data,
        errors,
        messages
      )
    }
  }

}
