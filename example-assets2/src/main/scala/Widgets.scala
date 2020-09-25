package ltbs.uniform
package examples

import ltbs.uniform.common.web._
import play.twirl.api.Html
import java.time.{LocalDate => Date}
import validation.{Rule, Transformation}

object Widgets extends Widgets

trait Widgets {

  // do we even need this anymore???
  implicit def unitField: FormField[Unit,Html] = ???

  def fieldSurround(key: List[String], errors: ErrorTree, messages: UniformMessages[Html])(inner: Html*): Html = ???

  implicit val stringField = new FormField[String,Html] {

    def decode(out: Input): Either[ErrorTree,String] = out.toStringField().toEither
    def encode(in: String): Input = Input.one(List(in))

    def render(pageKey: List[String],fieldKey: List[String],breadcrumbs: Breadcrumbs,data: Input,errors: ErrorTree,messages: UniformMessages[Html]): Option[Html] = ??? // TODO: Implement Me

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
  ): Html = ???


}
