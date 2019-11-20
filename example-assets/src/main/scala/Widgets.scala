package ltbs.uniform
package examples

import ltbs.uniform.common.web._

import cats.data.Validated
import cats.implicits._
import java.time.LocalDate
import scalatags._, generic.Bundle
import validation.{Rule, Transformation}

private[examples] trait AbstractWidgets[Builder, Output <: FragT, FragT]{

  val bundle: Bundle[Builder, Output, FragT]
  
  import bundle.all._

  implicit val unitField = new FormField[Unit,Tag] {
    def decode(out: Input): Either[ltbs.uniform.ErrorTree,Unit] = Right(())
    def encode(in: Unit): Input = Input.empty
    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Tag]
    ): Tag = span(cls:="unit")("")

  }

  implicit class RichError(errors: ErrorTree) {
    def cls(className: String): String =
      if (errors.definedAtRoot) { className } else ""
  }

  def fieldSurround(key: List[String], errors: ErrorTree, messages: UniformMessages[Tag])(inner: Tag*): Tag = 
    div(cls := s"govuk-form-group ${errors.cls("govuk-form-group--error")}")(
      label(cls := "govuk-label", attr("for") := key.mkString("_"))(
        messages(key.mkString("."))
      ),
      messages.get({key:+ "hint"}.mkString(".")).map { hintMsg =>
        span( id := key.mkString("_"), cls := "govuk-hint") (hintMsg)
      },
      errors.valueAtRootList.map { error => 
        span ( id := s"${key.mkString("_")}-error", cls := "govuk-error-message" )(
          span( cls := "govuk-visually-hidden")(messages("error"),":"), 
          error.prefixWith(key).render(messages)
        )},
      inner
    )

  implicit val stringField = new FormField[String,Tag] {
    def decode(out: Input): Either[ErrorTree,String] = out.toStringField().toEither
    def encode(in: String): Input = Input.one(List(in))

    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Tag]
    ): Tag = {

      val existingValue: String = data.valueAtRoot.flatMap{_.headOption}.getOrElse("")
      fieldSurround(key, errors, messages) {
        input(
          cls   := s"govuk-input ${errors.cls("govuk-input--error")}",
          id    := key.mkString("_"),
          name  := key.mkString("."),
          value := existingValue
        )
      }
    }
  }

  implicit val intField: FormField[Int,Tag] =
    stringField.simap(x => 
      {
        Rule.nonEmpty[String].apply(x) andThen
        Transformation.catchOnly[NumberFormatException]("not-a-number")(_.toInt)
      }.toEither
    )(_.toString)

  implicit val booleanField = new FormField[Boolean,Tag] {
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
      messages: UniformMessages[Tag]
    ): Tag = {
      val existingValue: Option[String] = data.valueAtRoot.flatMap{_.headOption}
      radios(key, List(true.toString,false.toString), existingValue, errors, messages)
    }
  }

  def radios(
    key: List[String],
    options: Seq[String],
    existing: Option[String],
    errors: ErrorTree,
    messages: UniformMessages[Tag],
    conditional: PartialFunction[String,Tag] = PartialFunction.empty
  ): Tag = {
    val keyNoDots=key.mkString("-")

    fieldSurround(key, errors, messages) (
      div (cls:= "govuk-radios") {
        options.zipWithIndex.map{ case (opt,num) =>
            div(cls:="govuk-radios__item", attr("data-target"):=keyNoDots)(
              input(
                cls:="govuk-radios__input",
                id:=s"$keyNoDots-$num",
                name:= key.mkString("."),
                attr("type"):="radio",
                value:=opt,
                attr("aria-describedby"):=s"$keyNoDots-num-item-hint",
                if(existing.exists(_ == opt)){ checked },
                if(conditional.isDefinedAt(opt)) {attr("aria-expanded"):="true"}
              ),
              label(cls:="govuk-label govuk-radios__label govuk-label--s", attr("for"):=s"$keyNoDots-$num")(
                messages.decompose({key :+ opt}.mkString("."))
              ),
              messages.get({key :+ opt :+ "hint"}.mkString(".")).map { hint =>
                span( id:=s"$keyNoDots-opt-item-hint", cls:="govuk-hint govuk-checkboxes__hint")(
                  hint
                )
              },
              if (conditional.isDefinedAt(opt)) {
                div (id:=s"conditional-$keyNoDots-$opt", cls:=s"conditional conditional-$keyNoDots")(conditional(opt))
              }
            )
        }
      }
    )
  }

  implicit val dateField = new FormField[LocalDate,Tag] {

    override def stats = FormFieldStats(children = 3)

    def decode(out: Input): Either[ErrorTree,LocalDate] = {

      def intAtKey(key: String): Validated[ErrorTree, Int] =
        out.subField(key, Rule.nonEmpty[String].apply(_) andThen {x: String =>
          Validated.catchOnly[NumberFormatException](x.toInt).leftMap(_ => ErrorMsg("badValue").toTree)
        } andThen Rule.min(1))

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
      messages: UniformMessages[Tag]
    ): Tag = {
      fieldSurround(key, errors, messages)(
        Seq("day","month","year") flatMap { field => 
          Seq(
            div(cls:="govuk-date-input__item") (
            div (cls:="govuk-form-group")(
              label (cls:="govuk-label govuk-date-input__label", attr("for"):="@key-@field")(
                messages((key :+ field).mkString(".") ,field)
              ),

            )
          ),
          input(
            cls:=s"govuk-input govuk-date-input__input govuk-input--width-${if(field=="year") 4 else 2} ${if (errors.definedAt(field)) {"govuk-input--error"}}",
            id:=(key :+ field).mkString("_"),
            name:=(key :+ field).mkString("."),
            attr("type"):="number",
            pattern:="[0-9]*",
            value:=data.valueAt(field).headOption.fold("")(_.toString)
          ))
        }:_*
        
      )
    }
  }

}
