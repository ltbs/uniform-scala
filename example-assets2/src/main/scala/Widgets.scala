package ltbs.uniform
package examples

import ltbs.uniform.common.web._
import play.twirl.api.Html
import java.time.{LocalDate => Date}
import validation.{Rule, Transformation}
import cats.data.NonEmptyList

object Widgets extends Widgets

trait Widgets {

  // do we even need this anymore???
  implicit def unitField: FormField[Unit,Html] = new FormField[Unit, Html] {
    def decode(out: Input): Either[ErrorTree,Unit] = Right(())
    def encode(in: Unit): Input = Input.empty
    def render(pageKey: List[String],fieldKey: List[String],breadcrumbs: Breadcrumbs,data: Input,errors: ErrorTree,messages: UniformMessages[Html]): Option[Html] = Some(Html(""))
  }

  implicit val stringField = new FormField[String,Html] {

    def decode(out: Input): Either[ErrorTree,String] = out.toStringField().toEither
    def encode(in: String): Input = Input.one(List(in))

    def render(pageKey: List[String],fieldKey: List[String],breadcrumbs: Breadcrumbs,data: Input,errors: ErrorTree,messages: UniformMessages[Html]): Option[Html] = {
      import uk.gov.hmrc.govukfrontend.views.html.components._
      Some((new govukInput(new govukErrorMessage(), new govukHint(), new govukLabel())).apply(new Input(
        id = pageKey.mkString("_"),
        name = pageKey.mkString(".")
      )))
    }
  }
  implicit val intField: FormField[Int,Html] =
    stringField.simap(x => 
      {
        Rule.nonEmpty[String].apply(x) andThen
        Transformation.catchOnly[NumberFormatException]("not-a-number")(_.toInt)
      }.toEither
    )(_.toString)
    
  implicit def booleanField: FormField[Boolean,Html] = ???
  implicit def dateField: FormField[Date,Html] = ???  

  def radios(
    fieldKey: List[String],
    options: Seq[String],
    existing: Option[String],
    errors: ErrorTree,
    messages: UniformMessages[Html],
    conditional: PartialFunction[String,Html] = PartialFunction.empty
  ): Html = ???

  def errorSummary(
    key: List[String],
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html = {
    import uk.gov.hmrc.govukfrontend.views.html.layouts._    
    import uk.gov.hmrc.govukfrontend.views.html.components._

    (new govukErrorSummary)(
      ErrorSummary(
        errorList = errors.toSeq.collect{ case (NonEmptyList(primary,_), NonEmptyList(msg,_)) => 
          ErrorLink(
            href = Some(s"#${primary}"), 
            content = Text(msg.render(messages).toString),
            attributes = Map("class" ->"govuk-link")
          )
        },
        title = Text("There is a problem")
      )
    )
  }


}
