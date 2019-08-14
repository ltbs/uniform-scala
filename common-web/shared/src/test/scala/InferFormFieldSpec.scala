package ltbs.uniform
package common.web

import org.scalatest._
import cats.implicits._
import com.github.ghik.silencer.silent

object Presenter extends InferFormField[String] with SampleFormFields {

  val mon: cats.Monoid[String] = implicitly

  def selectionOfFields(
    inner: List[(String, (List[String], Path, Input, ErrorTree, UniformMessages[String]) => String)]
  )(
    key: List[String],
    @silent("never used") path: Path,
    @silent("never used") values: Input,
    @silent("never used") errors: ErrorTree,
    @silent("never used") messages: UniformMessages[String]
  ):String = key.mkString(".") ++ ":" ++ inner.map(_._1).mkString(",")

}

class InferFormFieldSpec extends FlatSpec with Matchers {

  import Presenter._

  val mon: cats.Monoid[String] = implicitly

  def testEncoding[A](in: A)(implicit codec: Codec[A]): org.scalatest.Assertion = {
    import codec._
    decode(encode(in)) should be ((in).asRight[ErrorTree])
  }

  "FormField" should "instances should be inductively inferable for a case class" in {

    final case class TestCaseClass(a: Int, b: String, c: (Int, Int))

    val renderer = implicitly[FormField[TestCaseClass, String]]

    renderer.render(List("testRecord"), Nil, Input.empty, ErrorTree.empty, UniformMessages.noop) should be (
      "INT[testRecord.a]STRING[testRecord.b]INT[testRecord.c._1]INT[testRecord.c._2]"
    )

    testEncoding(TestCaseClass(1,"test2", (12, 23)))
  }

  it should "instances should be inductively inferable for an either (coproduct)" in {
    type TestType = Either[String, Int]
    val presentation = implicitly[FormField[TestType, String]]

    presentation.render(List("testRecord"), Nil, Input.empty, ErrorTree.empty, UniformMessages.noop) should be (
      "testRecord:Left,Right"
    )

    testEncoding("test".asLeft[Int])
    testEncoding(12.asRight[Int])
  }
}
