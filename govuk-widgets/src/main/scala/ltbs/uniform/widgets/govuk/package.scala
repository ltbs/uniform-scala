package ltbs.uniform.widgets

import ltbs.uniform._, web._
import enumeratum._
import play.twirl.api.Html

package object govuk extends InferForm {

  def errorSummary(key: String, values: Input, errors: ErrorTree, messages: Messages): Html =
    html.errorsummary(key, values, errors, messages)

  def compoundField(key: String, values: Input, errors: ErrorTree, messages: Messages)(inner: Html): Html = 
    html.compoundfield(key, errors, messages)(inner)

  def soloField(key: String, values: Input,errors: ErrorTree,messages: Messages)(inner: Html): Html =
    html.standardfield(key, errors, messages)(inner)

  implicit val booleanField = new HtmlField[Boolean] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages) =
      html.radios(
        key,
        Seq("TRUE","FALSE"),
        values.value.headOption,
        errors,
        messages
      )
  }

  implicit val longHtml = new HtmlField[Long] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages) =
        html.string(
          key,
          values,
          errors,
          messages
        )
  }

  implicit val intHtml = new HtmlField[Int] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages) =
        html.string(
          key,
          values,
          errors,
          messages
        )
  }


  implicit val stringHtml = new HtmlField[String] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages) =
        html.string(
          key,
          values,
          errors,
          messages
        )
  }

  implicit val localdateHtml = new HtmlField[java.time.LocalDate] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages) =
        html.date(
          key,
          values,
          errors,
          messages
        )
  }

  implicit def optionHtml[A](implicit inner: HtmlField[A]) = new HtmlField[Option[A]] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages) =
      html.option(key, values, errors, messages, inner.render _)
  }

  implicit def enumeratumHtml[A <: EnumEntry](implicit enum: Enum[A]) = new HtmlField[A] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages) = {
      val options: Seq[A] = enum.values
      val path = key.split("[.]").filter(_.nonEmpty).tail
      val existing: Option[String] = values.atPath(path:_*).flatMap{_.headOption}
      html.radios(key, options.map{_.toString}, existing, errors, messages)
    }
  }

  implicit def enumeratumSetHtml[A <: EnumEntry](implicit enum: Enum[A]) = new HtmlField[Set[A]] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages) = {
      val options: Seq[A] = enum.values
      val path = key.split("[.]").filter(_.nonEmpty).tail
      val existing: List[String] = values.atPath(path:_*).getOrElse(Nil)
      html.checkboxes(key, options.map{_.toString}, existing, errors, messages)
    }
  }

}
