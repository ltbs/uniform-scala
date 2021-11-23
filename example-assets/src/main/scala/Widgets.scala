package ltbs.uniform
package examples

import ltbs.uniform.common.web._

import cats.data.Validated
import cats.implicits._
import java.time.LocalDate
import scalatags._, generic.Bundle
import validation._

private[examples] trait AbstractWidgets[Builder, Output <: FragT, FragT] {

  val bundle: Bundle[Builder, Output, FragT]
  
  import bundle.all._

  implicit class RichError(errors: ErrorTree) {
    def cls(className: String): String =
      if (errors.definedAtRoot) { className } else ""
  }

  implicit val tellInt = new WebTell[Tag, Int] {
    def render(in: Int, key: String, messages: UniformMessages[Tag]): Option[Tag] = Some(span(in.toString))
  }

  implicit val tellString = new WebTell[Tag, String] {
    def render(in: String, key: String, messages: UniformMessages[Tag]): Option[Tag] = Some(span(in))
  }

  implicit val tellBoolean = new WebTell[Tag, Boolean] {
    def render(in: Boolean, key: String, messages: UniformMessages[Tag]): Option[Tag] = Some(span(in.toString))
  }

  def formSurround(key: String, tell: Option[Tag], errors: ErrorTree, messages: UniformMessages[Tag])(inner: Tag*): Tag =
    div(cls := s"govuk-form-group ${errors.cls("govuk-form-group--error")}")(
      tell.map{x => div(x, br(), br())},
      errorSummary(List(key), errors, messages),
      errors.valueAtRootList.map { error => 
        span ( id := s"${key}-error", cls := "govuk-error-message" )(
          span( cls := "govuk-visually-hidden")(messages("error"),":"), 
          error.prefixWith(List(key)).render(messages)
        )},
      inner,
      br(), br(),
      button(tpe:="submit", cls:="govuk-button")(messages({key ++ ".save-and-continue"}))
    )

  def subfieldSurround(key: List[String], errors: ErrorTree, messages: UniformMessages[Tag])(inner: Tag*): Tag =
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

  def fieldSurround(key: List[String], tell: Option[Tag], errors: ErrorTree, messages: UniformMessages[Tag])(inner: Tag*): Tag = 
    key match {
      case (topLevel :: Nil) => formSurround(topLevel, tell, errors, messages)(inner:_*)
      case _ => subfieldSurround(key, errors, messages)(inner:_*)        
    }

  def optLabel(key: List[String], tell: Option[Tag], errors: ErrorTree, messages: UniformMessages[Tag])(inner: Tag): Tag = {
    println(key)
    key match {
      case _ :: Nil => inner
      case _ => fieldSurround(key, tell, errors, messages)(inner)
    }
  }

  implicit val stringField = new WebAsk[Tag,String] {
    def decode(out: Input): Either[ErrorTree,String] = out.toStringField().toEither
    def encode(in: String): Input = Input.one(List(in))

    def render(
      pageIn: PageIn[Tag],
      stepDetails: StepDetails[Tag, String]
    ): Option[Tag] = Some{
      import stepDetails._
      val existingValue: Option[String] = data.valueAtRoot.flatMap{_.headOption}

      val restrictedOptions: Option[Seq[String]] = stepDetails.validation.subRules.collectFirst{
        case Rule.In(options, _) => options
      }.filter(_.size < 20)

        restrictedOptions match {
          case Some(options) => radios(stepDetails.fieldKey, stepDetails.tell, options, existingValue, stepDetails.errors, pageIn.messages)
          case None =>
            fieldSurround(fieldKey, tell, errors, pageIn.messages) {
              input(
                cls   := s"govuk-input ${errors.cls("govuk-input--error")}",
                id    := fieldKey.mkString("_"),
                name  := fieldKey.mkString("."),
                value := existingValue.getOrElse("")
              )
            }
      }
    }
  }

  implicit val intField: WebAsk[Tag,Int] =
    stringField.simap(x => 
      {
        Rule.nonEmpty[String].apply(x) andThen
        Transformation.catchOnly[NumberFormatException]("not-a-number")(_.toInt)
      }.toEither
    )(_.toString)

  implicit val booleanField = new WebAsk[Tag,Boolean] {
    def decode(out: Input): Either[ErrorTree,Boolean] =
      out.toField[Boolean]{x: String =>
        Validated.catchOnly[IllegalArgumentException](x.toBoolean).leftMap(_ => ErrorMsg("invalid").toTree)
      }.toEither

    def encode(in: Boolean): Input = Input.one(List(in.toString))

    def render(
      pageIn: PageIn[Tag],
      stepDetails: StepDetails[Tag, Boolean]
    ): Option[Tag] = Some{
      import stepDetails._
      val existingValue: Option[String] = data.valueAtRoot.flatMap{_.headOption}
      radios(fieldKey, tell, List(true.toString,false.toString), existingValue, errors, pageIn.messages)
    }
  }

  def radios(
    fieldKey: List[String],
    tell: Option[Tag],
    options: Seq[String],
    existing: Option[String],
    errors: ErrorTree,
    messages: UniformMessages[Tag],
    conditional: PartialFunction[String,Tag] = PartialFunction.empty
  ): Tag = {
    val keyNoDots=fieldKey.mkString("-")

    fieldSurround(fieldKey, tell, errors, messages) ( 
      div (cls:= "govuk-radios") {
        options.zipWithIndex.map{ case (opt,num) =>
            div(cls:="govuk-radios__item", attr("data-target"):=keyNoDots)(
              input(
                cls:="govuk-radios__input",
                id:=s"$keyNoDots-$num",
                name:= fieldKey.mkString("."),
                attr("type"):="radio",
                value:=opt,
                attr("aria-describedby"):=s"$keyNoDots-num-item-hint",
                if(existing.exists(_ == opt)){ checked },
                if(conditional.isDefinedAt(opt)) {attr("aria-expanded"):="true"}
              ),
              label(cls:="govuk-label govuk-radios__label govuk-label--s", attr("for"):=s"$keyNoDots-$num")(
                messages.decompose({fieldKey :+ opt}.mkString("."))
              ),
              messages.get({fieldKey :+ opt :+ "hint"}.mkString(".")).map { hint =>
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

  implicit val dateField = new WebAsk[Tag,LocalDate] {

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
      pageIn: PageIn[Tag],
      stepDetails: StepDetails[Tag, LocalDate]
    ): Option[Tag] = Some{
      import pageIn.messages
      import stepDetails._
      fieldSurround(fieldKey, tell, errors, messages)(
        Seq("day","month","year") flatMap { field => 
          Seq(
            div(cls:="govuk-date-input__item") (
            div (cls:="govuk-form-group")(
              label (cls:="govuk-label govuk-date-input__label", attr("for"):="@fieldKey-@field")(
                messages((fieldKey :+ field).mkString(".") ,field)
              ),
            )
          ),
          input(
            cls:=s"govuk-input govuk-date-input__input govuk-input--width-${if(field=="year") 4 else 2} ${if (errors.definedAt(field)) {"govuk-input--error"}}",
            id:=(fieldKey :+ field).mkString("_"),
            name:=(fieldKey :+ field).mkString("."),
            attr("type"):="number",
            pattern:="[0-9]*",
            value:={data / field}.valueAtRoot.flatMap{_.headOption}.getOrElse("")
          ))
        }:_*
        
      )
    }
  }

  def errorSummary(
    key: List[String],
    errors: ErrorTree,
    messages: UniformMessages[Tag]
  ): Option[Tag] = errors match {
    case ErrorTree.empty => None
    case _ => Some{

      val errorTags: List[Tag] =
        ErrorTree.simplified(errors).map { case (path, errormsg) =>
          li()(
            a(href:=s"#${(key ++ path).mkString("_")}")(
              errormsg.prefixWith(key ++ path).render(messages)
              )
          )
        }.toList

      div(
        cls:="govuk-error-summary",
        attr("aria-labelledby"):="error-summary-title",
        attr("role"):="alert",
        tabindex:="-1",
          attr("data-module"):="error-summary"
      )(
        h2(cls:="govuk-error-summary__title", id:="error-summary-title")(
          messages({key :+ "there.is.a.problem"}.mkString("."))
        ),
        div(cls:="govuk-error-summary__body")(
          ul(cls:="govuk-list govuk-error-summary__list")(
            errorTags
          )
        )
      )
    }
  }
}
