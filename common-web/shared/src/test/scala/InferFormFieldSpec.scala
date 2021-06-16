package ltbs.uniform
package common.web

import scala.language.higherKinds
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

class InferFormFieldSpec extends munit.FunSuite {

  import Presenter._

  val renderer = implicitly[FormField[String, TestCaseClass]]

  def testEncoding[A](in: A)(implicit codec: Codec[A]) = {
    import codec._
    assertEquals(decode(encode(in)), (in).asRight[ErrorTree])
  }

  test("An inductively inferred FormField for a case class ") {
    test ("should encode correctly") {
      testEncoding(TestCaseClass(1,"test2", (12, 23)))
    }

    test ("should render correctly") {
      assertEquals(
        renderer.render(Nil, List("testRecord"), Nil, Input.empty, ErrorTree.empty, UniformMessages.noop),
        Some("INT[testRecord.a]∧STRING[testRecord.b]∧INT[testRecord.c._1]∧INT[testRecord.c._2]")
      )
    }

    test ("should instances should be inductively inferable for an either (coproduct)") {
      val presentation = implicitly[FormField[String, Either[String, Int]]]

      assertEquals(
        presentation.render(Nil, List("testRecord"), Nil, Input.empty, ErrorTree.empty, UniformMessages.noop),
        Some("STRING[testRecord.Left.value]∨INT[testRecord.Right.value]")
      )

      testEncoding("test".asLeft[Int])
      testEncoding(12.asRight[Int])
    }
  }

}
