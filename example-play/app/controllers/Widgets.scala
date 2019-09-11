package controllers

import cats.implicits._

import ltbs.uniform._, interpreters.playframework._
import ltbs.uniform.common.web.FormField
import play.twirl.api.Html
import java.time.LocalDate
import cats.data.Validated

object Widgets extends Widgets

trait Widgets extends InputOps {

  implicit val twirlBigStringField = new FormField[BigString,Html] {
    import shapeless.tag
    def decode(out: Input): Either[ErrorTree,BigString] =
      out.toField[BigString](
        x => Validated.Valid(tag[BigStringTag][String](x))
      ).toEither

    def encode(in: BigString): Input = Input.one(List(in))
    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: String = data.valueAtRoot.flatMap{_.headOption}.getOrElse("")
      views.html.uniform.textarea(key, existingValue, errors, messages)
    }
  }

  implicit val twirlBooleanField = new FormField[Boolean,Html] {
    def decode(out: Input): Either[ErrorTree,Boolean] =
      out.toField[Boolean]{x: String =>
        Validated.catchOnly[IllegalArgumentException](x.toBoolean).leftMap(_ => ErrorMsg("invalid").toTree)
      }.toEither

    def encode(in: Boolean): Input = Input.one(List(in.toString))

    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: Option[String] = data.valueAtRoot.flatMap{_.headOption}
      views.html.uniform.radios(key, List(true.toString,false.toString), existingValue, errors, messages)
    }
  }

  implicit val twirlStringField = new FormField[String,Html] {
    def decode(out: Input): Either[ErrorTree,String] = out.toStringField().toEither
    def encode(in: String): Input = Input.one(List(in))

    def render(
      key: List[String],
      path: Path,
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
        out.subField(key, nonEmptyString(_) andThen {x: String =>
          Validated.catchOnly[NumberFormatException](x.toInt).leftMap(_ => ErrorMsg("badValue").toTree)
        } andThen min(0))

      (
        intAtKey("day"),
        intAtKey("month"),
        intAtKey("year")
      ).tupled.toEither.flatMap{ case (d,m,y) =>
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
      path: Path,
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
