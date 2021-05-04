package ltbs.uniform
package common.web

import scala.language.higherKinds

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import cats.implicits._

object Presenter extends InferFormFields[String] with SampleFormFields {

  def renderAnd(
    pageKey: List[String],
    fieldKey: List[String],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[String],
    members: Seq[(String, String)]
  ): String =
    members.map(_._2).mkString("∧")

  def renderOr(
    pageKey: List[String],
    fieldKey: List[String],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[String],
    alternatives: Seq[(String, Option[String])],
    selected: Option[String]): String =
    alternatives.flatMap(_._2).mkString("∨")

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
    renderer.render(Nil, List("testRecord"), Nil, Input.empty, ErrorTree.empty, UniformMessages.noop) should be (
      Some("INT[testRecord.a]∧STRING[testRecord.b]∧INT[testRecord.c._1]∧INT[testRecord.c._2]")
    )
  }

  it should "instances should be inductively inferable for an either (coproduct)" in {
    val presentation = implicitly[FormField[Either[String, Int], String]]

    presentation.render(Nil, List("testRecord"), Nil, Input.empty, ErrorTree.empty, UniformMessages.noop) should be (
      Some("STRING[testRecord.Left.value]∨INT[testRecord.Right.value]")
    )

    testEncoding("test".asLeft[Int])
    testEncoding(12.asRight[Int])
  }

}
