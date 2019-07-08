package ltbs.uniform
package common.web

import org.scalatest._
import cats.implicits._
import cats.data.NonEmptyList
object SampleFormFields extends SampleFormFields
trait SampleFormFields extends SampleFormFieldEncoders with SampleFormFieldRenderers {
  implicit val stringField: FormField[String, String] =
    InferFormField.combine(implicitly, implicitly)

  implicit val intField: FormFieldEncoding[Int] =
    stringField.simap(x =>
      Either.catchOnly[NumberFormatException](x.toInt)
        .leftMap(_ => ErrorMsg("bad.value").toTree)
    )(_.toString)
}

class InferFormFieldSpec extends FlatSpec with Matchers with InferFormField[String] with SampleFormFields {

  "FormField" should "infer products and coproducts" in {
    "implicitly[FormField[String]]" should compile
    "implicitly[FormField[Int]]" should compile
    "implicitly[FormField[Boolean]]" shouldNot compile // no instance for boolean
    "implicitly[FormField[(Int, String)]]" should compile // deriving a product
    "implicitly[FormField[Either[Int, String]]]" should compile // deriving a coproduct    
  }

}
