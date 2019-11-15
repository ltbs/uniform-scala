package ltbs.uniform
package common.web

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers

import cats.implicits._

object Presenter extends InferFormFieldProduct[String] with SampleFormFields {

  def renderProduct[A](
    key: List[String],
    path: Path,
    values: Input,
    errors: ErrorTree,
    messages: UniformMessages[String],
    pfl: ProductFieldList[A]
  ): String =
    pfl.inner.map { case (subFieldId, f) =>
      f(key:+ subFieldId, path, values, errors, messages)
    }.mkString

}

final case class TestCaseClass(a: Int, b: String, c: (Int, Int))

class InferFormFieldSpec extends AnyFlatSpec with Matchers {

  import Presenter._

  val renderer = implicitly[FormField[TestCaseClass, String]]

  def testEncoding[A](in: A)(implicit codec: Codec[A]): org.scalatest.Assertion = {
    import codec._
    decode(encode(in)) should be ((in).asRight[ErrorTree])
  }

  "An inductively inferred FormField for a case class " should "encode correctly" in {
    testEncoding(TestCaseClass(1,"test2", (12, 23)))

  }

  it should "render correctly" in {
    renderer.render(List("testRecord"), Nil, Input.empty, ErrorTree.empty, UniformMessages.noop) should be (
      "INT[testRecord.a]STRING[testRecord.b]INT[testRecord.c._1]INT[testRecord.c._2]"
    )
  }

  // it should "instances should be inductively inferable for an either (coproduct)" in {
  //   type TestType = Either[String, Int]
  //   val presentation = implicitly[FormField[TestType, String]]

  //   presentation.render(List("testRecord"), Nil, Input.empty, ErrorTree.empty, UniformMessages.noop) should be (
  //     "testRecord:Left,Right"
  //   )

  //   testEncoding("test".asLeft[Int])
  //   testEncoding(12.asRight[Int])
  // }
}
