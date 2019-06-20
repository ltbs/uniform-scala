package ltbs.uniform
package common.web

import org.scalatest._
import cats.implicits._
import cats.data.NonEmptyList
object SampleFormFieldEncoders extends SampleFormFieldEncoders
trait SampleFormFieldEncoders {
  implicit val stringField = new FormFieldEncoding[String] {
    def decode(out: Input): Either[ErrorTree,String] = {
      val root: Option[String] = out.valueAtRoot
        .flatMap(_.filter(_.trim.nonEmpty).headOption)

      root match {
        case None => Left(ErrorMsg("required").toTree)
        case Some(data) => Right(data)
      }
    }

    def encode(in: String): Input = Input.one(List(in))
  }

  implicit val intField: FormFieldEncoding[Int] =
    stringField.simap(x =>
      Either.catchOnly[NumberFormatException](x.toInt)
        .leftMap(_ => ErrorMsg("bad.value").toTree)
    )(_.toString)
}

final case class TestCaseClass(a: Int, b: String, c: (Int, Int))

class InferFormFieldEncodingSpec extends FlatSpec with Matchers with InferFormFieldEncoding with SampleFormFieldEncoders {

  // the names of the fields are taken from the structure of the data
  // however in different versions of scala the names for the values inside 
  // Left and Right differ. To test them correctly we use runtime reflection
  // to get the appropriate values to check against.
  val (leftValueName, rightValueName) = {
    import scala.reflect.runtime.universe._
    def getConstructorParamName[T: TypeTag]: List[String] = {
      typeOf[T].members.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.name.toString
      }.toList
    }
    (
      getConstructorParamName[Left[String, Int]].head,
      getConstructorParamName[Right[String, Int]].head
    )
  }

  def testEncoding[A](in: A)(implicit codec: FormFieldEncoding[A]): org.scalatest.Assertion = {
    import codec._
    decode(encode(in)) should be ((in).asRight[ErrorTree])
  }

  "FormFieldEncoding" should "infer products and coproducts" in {
    "implicitly[FormFieldEncoding[String]]" should compile
    "implicitly[FormFieldEncoding[Int]]" should compile
    "implicitly[FormFieldEncoding[Boolean]]" shouldNot compile // no instance for boolean
    "implicitly[FormFieldEncoding[(Int, String)]]" should compile // deriving a product
    "implicitly[FormFieldEncoding[Either[Int, String]]]" should compile // deriving a coproduct    
  }

  it should "correctly encode a case class" in {
    val codec = implicitly[FormFieldEncoding[TestCaseClass]]
    val testRecord = TestCaseClass(1,"test2", (12, 23))
    codec.encode(testRecord) should be (
      Map(
        List("c", "_1") -> List(testRecord.c._1.toString),
        List("c", "_2") -> List(testRecord.c._2.toString),
        List("b") -> List(testRecord.b.toString),
        List("a") -> List(testRecord.a.toString)
      )
    )
  }

  it should "correctly encode an either (coproduct)" in {
    type TestType = Either[String, Int]
    val codec = implicitly[FormFieldEncoding[TestType]]
    val testRecord1: TestType = Left("test")
    val testRecord2: TestType = Right(12)

    codec.encode(testRecord1) should be (
      Map(
        Nil -> List("Left"),
        List("Left", leftValueName) -> List("test")
      )
    )

    codec.encode(testRecord2) should be (
      Map(
        Nil -> List("Right"),
        List("Right", rightValueName) -> List("12")
      )
    )    
  }

  it should "have the property decode(encode(x)) = Right(x)" in {
    testEncoding((1,1))
    testEncoding(1)
    "testEncoding(false)" shouldNot compile // no instance for boolean    
    testEncoding("test")
    testEncoding(TestCaseClass(1,"test2", (12, 23)))    
    testEncoding("test".asLeft[Int])    
  }

  it should "propagate errors from a product" in {
    case class Inner[A](lower: A, higher: A)
    case class Middle(a: Inner[Int])
    case class Outer(in: Middle, stringField: String)
    val codec = implicitly[FormFieldEncoding[Outer]]

    codec.decode(
      Map(
        List("stringField") -> List("nonempty"),
        List("in", "a", "lower") -> List("not an int"),
        List("in", "a", "higher") -> List("12")
      )
    ) should be (
      Left(Map(NonEmptyList.one(List("in", "a", "lower")) -> NonEmptyList.one(ErrorMsg("bad.value"))))
    )
  }

  it should "propagate errors from a coproduct" in {
    type TestType = Either[String, Int]
    val codec = implicitly[FormFieldEncoding[TestType]]

    codec.decode(
      Map(
        Nil -> List("Left")
      )
    ) should be (
      Left(Map(NonEmptyList.one(List("Left", leftValueName)) -> NonEmptyList.one(ErrorMsg("required"))))
    )

  }

  it should "propagate errors from a nested coproduct" in {
    type TestType = Either[String, Either[String, Int]]
    val codec = implicitly[FormFieldEncoding[TestType]]

    codec.decode(
      Map(
        Nil -> List("Right"),
        List("Right",rightValueName) -> List("Right")        
      )
    ) should be (
      Left(Map(NonEmptyList.one(List("Right", rightValueName, "Right", rightValueName)) -> NonEmptyList.one(ErrorMsg("required"))))
    )

  }



}
