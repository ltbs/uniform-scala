package ltbs.uniform.widgets

import ltbs.uniform.datapipeline._
import enumeratum._
import play.twirl.api.Html

package object govuk extends InferForm {

  def errorSummary(key: String, values: Input, errors: Error, messages: Messages[Html]): Html =
    html.errorsummary(key, values, errors, messages)

  def compoundField(key: String, values: Input, errors: Error, messages: Messages[Html])(inner: Html): Html = 
    html.compoundfield(key, errors, messages)(inner)

  def soloField(key: String, values: Input,errors: Error,messages: Messages[Html])(inner: Html): Html =
    html.standardfield(key, errors, messages)(inner)

  implicit val booleanField = new HtmlField[Boolean] {
    def render(key: String, values: Input, errors: Error, messages: Messages[Html]) =
      html.radios(
        key,
        Set("TRUE","FALSE"),
        values.children.get(key).flatMap(_.value.headOption),
        errors,
        messages
      )
  }

  implicit val longHtml = new HtmlField[Long] {
    def render(key: String, values: Input, errors: Error, messages: Messages[Html]) =
        html.string(
          key,
          values,
          errors,
          messages
        )
  }

  implicit val intHtml = new HtmlField[Int] {
    def render(key: String, values: Input, errors: Error, messages: Messages[Html]) =
        html.string(
          key,
          values,
          errors,
          messages
        )
  }

  implicit val stringHtml = new HtmlField[String] {
    def render(key: String, values: Input, errors: Error, messages: Messages[Html]) =
        html.string(
          key,
          values,
          errors,
          messages
        )
  }

  implicit val localdateHtml = new HtmlField[java.time.LocalDate] {
    def render(key: String, values: Input, errors: Error, messages: Messages[Html]) =
        html.date(
          key,
          values,
          errors,
          messages
        )
  }

  implicit def optionHtml[A](implicit inner: HtmlField[A]) = new HtmlField[Option[A]] {
    def render(key: String, values: Input, errors: Error, messages: Messages[Html]) =
      html.option(key, values, errors, messages, inner.render _)
  }

  implicit def enumeratumHtml[A <: EnumEntry](implicit enum: Enum[A]) = new HtmlField[A] {
    def render(key: String, values: Input, errors: Error, messages: Messages[Html]) = {
      val options: Set[A] = enum.values.toSet
      val path = key.split("[.]").filter(_.nonEmpty)
      val existing: Option[String] = values.atPath(path:_*).flatMap{_.headOption}
      html.radios(key, options.map{_.toString}, existing, errors, messages)
    }
  }

  implicit def enumeratumSetHtml[A <: EnumEntry](implicit enum: Enum[A]) = new HtmlField[Set[A]] {
    def render(key: String, values: Input, errors: Error, messages: Messages[Html]) = {
      val options: Set[A] = enum.values.toSet
      val path = key.split("[.]").filter(_.nonEmpty)
      val existing: Option[List[String]] = values.atPath(path:_*)

      html.checkboxes(key, options.map{_.toString}, existing, errors, messages)
    }
  }

}
