package ltbs.uniform.widgets

import ltbs.uniform._, web._
import enumeratum._
import play.twirl.api.Html
import cats.implicits._

package object govuk extends InferForm {

  def errorSummary(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html =
    html.errorsummary(key, values, errors, messages)

  def compoundField(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html])(inner: Html): Html =
    html.compoundfield(key, errors, messages)(inner)

  def soloField(key: String, values: Input,errors: ErrorTree,messages: UniformMessages[Html])(ask: Html)(tell: Html): Html =
    html.standardfield(key, errors, messages)(ask)(tell)

  def selectionOfFields(
    inner: List[(String, (String, Input, ErrorTree, UniformMessages[Html]) => Html)]
  )(
    key: String,
    values: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html = html.radios(
    key,
    inner.map{_._1},
    values.value.headOption,
    errors,
    messages,
    inner.map{
      case(subkey,f) => subkey -> f(s"$key.$subkey", values, errors, messages)
    }.filter(_._2.toString.trim.nonEmpty).toMap
  )

  implicit val booleanField = new HtmlField[Boolean] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) =
      html.radios(
        key,
        Seq("TRUE","FALSE"),
        values.value.headOption,
        errors,
        messages
      )
  }

  implicit val longHtml = new HtmlField[Long] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) =
        html.string(
          key,
          values,
          errors,
          messages
        )
  }

  implicit val intHtml = new HtmlField[Int] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) =
        html.string(
          key,
          values,
          errors,
          messages
        )
  }

  implicit val stringHtml = new HtmlField[String] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) =
        html.string(
          key,
          values,
          errors,
          messages
        )
  }

  implicit val localdateHtml = new HtmlField[java.time.LocalDate] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) =
        html.date(
          key,
          values,
          errors,
          messages
        )
  }

  implicit def optionHtml[A](implicit inner: HtmlField[A]) = new HtmlField[Option[A]] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) =
      html.option(key, values, errors, messages, inner.render _)
  }

  implicit def enumeratumHtml[A <: EnumEntry](implicit enum: Enum[A]) = new HtmlField[A] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) = {
      val options: Seq[A] = enum.values
      val path = key.split("[.]").filter(_.nonEmpty).tail
      val existing: Option[String] = values.atPath(path:_*).flatMap{_.headOption}
      html.radios(key, options.map{_.toString}, existing, errors, messages)
    }
  }

  implicit def enumeratumSetHtml[A <: EnumEntry](implicit enum: Enum[A]) = new HtmlField[Set[A]] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) = {
      val options: Seq[A] = enum.values
      val path = key.split("[.]").filter(_.nonEmpty).tail
      val existing: List[String] = values.atPath(path:_*).getOrElse(Nil)
      html.checkboxes(key, options.map{_.toString}, existing, errors, messages)
    }
  }

  val jsListControlHtmlField = new ltbs.uniform.web.HtmlField[ListControl] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) = {
      val path = key.split("[.]").filter(_.nonEmpty).tail
      val existing: Option[String] = values.atPath(path:_*).flatMap{_.headOption}

      val visibleRadios: Html = {
        val options: Seq[ListControl] = Seq(AddAnother, Continue)
        html.radios(key, options.map{_.toString}, existing, errors, messages)
      }

      val hiddenFields = Html {
        s"""|<input name="$key" value="Delete" type="radio" style="display:none;" />
            |<input type="hidden" name="$key.Delete.ordinal" value="" />
            |<input name="$key" value="Edit" type="radio" style="display:none;" />
            |<input type="hidden" name="$key.Edit.ordinal" value="" />
            |""".stripMargin
      }

      visibleRadios |+| hiddenFields
    }
  }

  val jdkListControlHtmlField = new ltbs.uniform.web.HtmlField[ListControl] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]) = {
      val path = key.split("[.]").filter(_.nonEmpty).tail
      val existing: Option[String] = values.atPath(path:_*).flatMap{_.headOption}

      val options: Seq[ListControl] = Seq(AddAnother, Continue)
      html.radios(key, options.map{_.toString}, existing, errors, messages)
    }
  }

}
