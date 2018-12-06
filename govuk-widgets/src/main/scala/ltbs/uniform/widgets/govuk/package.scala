package ltbs.uniform.widgets

import ltbs.uniform.datapipeline._
import enumeratum._

package object govuk {

  implicit val booleanField = new HtmlForm[Boolean] {
    def render(key: String, values: Input, errors: Error, messages: Messages) =
      html.standardfield(key, errors, messages)(
        html.radios(
          key,
          Set("TRUE","FALSE"),
          values.value.headOption,
          errors,
          messages
        ))
  }

  implicit val longHtml = new HtmlForm[Long] {
    def render(key: String, values: Input, errors: Error, messages: Messages) =
      html.standardfield(key, errors, messages)(
        html.string(
          key,
          values,
          errors,
          messages
        )
      )
  }

  implicit val intHtml = new HtmlForm[Int] {
    def render(key: String, values: Input, errors: Error, messages: Messages) =
      html.standardfield(key, errors, messages)(
        html.string(
          key,
          values,
          errors,
          messages
        )
      )
  }


  implicit val stringHtml = new HtmlForm[String] {
    def render(key: String, values: Input, errors: Error, messages: Messages) =
      html.standardfield(key, errors, messages)(
        html.string(
          key,
          values,
          errors,
          messages
        )
      )
  }

  implicit val localdateHtml = new HtmlForm[java.time.LocalDate] {
    def render(key: String, values: Input, errors: Error, messages: Messages) =
      html.standardfield(key, errors, messages)(
        html.date(
          key,
          values,
          errors,
          messages
        )
      )
  }

  implicit def optionHtml[A](implicit inner: HtmlForm[A]) = new HtmlForm[Option[A]] {
    def render(key: String, values: Input, errors: Error, messages: Messages) =
      html.option(key, values, errors, messages, inner.render _)
  }

  implicit def enumeratumHtml[A <: EnumEntry](implicit enum: Enum[A]) = new HtmlForm[A] {
    def render(key: String, values: Input, errors: Error, messages: Messages) = {
      val options: Set[A] = enum.values.toSet
      val path = key.split("[.]").filter(_.nonEmpty)
      val existing: Option[String] = values.atPath(path:_*).flatMap{_.headOption}

      html.standardfield(key, errors, messages)(
        html.radios(key, options.map{_.toString}, existing, errors, messages)
      )
    }
  }

  implicit def enumeratumSetHtml[A <: EnumEntry](implicit enum: Enum[A]) = new HtmlForm[Set[A]] {
    def render(key: String, values: Input, errors: Error, messages: Messages) = {
      val options: Set[A] = enum.values.toSet
      val path = key.split("[.]").filter(_.nonEmpty)
      val existing: Option[List[String]] = values.atPath(path:_*)

      html.standardfield(key, errors, messages)(
        html.checkboxes(key, options.map{_.toString}, existing, errors, messages)
      )
    }
  }

}
