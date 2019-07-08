package ltbs.uniform
package common.web

import org.scalatest._
import cats.implicits._
import cats.data.NonEmptyList
object SampleFormFields extends SampleFormFields
trait SampleFormFields extends SampleFormFieldEncoders with SampleFormFieldRenderers {
}

class InferFormFieldSpec extends FlatSpec with Matchers  {

  object Presenter extends InferFormField[String] with SampleFormFields {

  }
  import Presenter._

  type Field[A] = FormField[A, String]

  "FormField" should "infer products and coproducts" in {
    // "implicitly[Field[String]]" should compile
    // "implicitly[Field[Int]]" should compile
    // "implicitly[Field[Boolean]]" shouldNot compile // no instance for boolean
    // "implicitly[Field[(Int, String)]]" should compile // deriving a product
    // "implicitly[Field[Either[Int, String]]]" should compile // deriving a coproduct
  }

}
