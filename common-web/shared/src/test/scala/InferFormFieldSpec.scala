package ltbs.uniform
package common.web

import scala.language.higherKinds
import cats.implicits._
import validation.Rule

object Presenter extends SampleFormFields with InferWebAsk[String] {

  // def renderAnd(pageKey: List[String],fieldKey: List[String],tell: Option[String],breadcrumbs: ltbs.uniform.common.web.Breadcrumbs,data: ltbs.uniform.Input,errors: ltbs.uniform.ErrorTree,messages: ltbs.uniform.UniformMessages[String],members: Seq[(String, String)]): String = ???

  // def renderAnd(
  //   pageKey: List[String],
  //   fieldKey: List[String],
  //   tell: Option[String],
  //   breadcrumbs: Breadcrumbs,
  //   data: Input,
  //   errors: ErrorTree,
  //   messages: UniformMessages[String],
  //   members: Seq[(String, String)]
  // ): String =
  //   members.map(_._2).mkString("∧")

  def renderAnd[T](
    pageIn: PageIn[String],
    stepDetails: StepDetails[String,T],
    members: Seq[(String, String)]
  ): String = members.map(_._2).mkString("∧")

  // def renderOr(
  //   pageKey: List[String],
  //   fieldKey: List[String],
  //   tell: Option[String],
  //   breadcrumbs: Breadcrumbs,
  //   data: Input,
  //   errors: ErrorTree,
  //   messages: UniformMessages[String],
  //   alternatives: Seq[(String, Option[String])],
  //   selected: Option[String]): String =
  //   alternatives.flatMap(_._2).mkString("∨")

  def renderOr[T](
    pageIn: PageIn[String],
    stepDetails: StepDetails[String,T],
    alternatives: Seq[(String, Option[String])],
    selected: Option[String]
  ): String = alternatives.flatMap(_._2).mkString("∨")
  
}

final case class TestCaseClass(a: Int, b: String, c: (Int, Int))

class InferFormFieldSpec extends munit.FunSuite {

  import Presenter._
  val renderer = implicitly[WebAsk[String, TestCaseClass]]

  def testEncoding[A](in: A)(implicit codec: Codec[A]) = {
    import codec._
    assertEquals(decode(encode(in)), (in).asRight[ErrorTree])
  }

  test("An inductively inferred WebAsk for a case class ") {

    val pageIn = PageIn[String](
      targetId = Nil,
      breadcrumbs = Nil,
      request = None,
      state = DB.empty,
      pathPrefix = Nil,
      JourneyConfig(),
      UniformMessages.noop,
      Map.empty
    )

    test ("should encode correctly") {
      testEncoding(TestCaseClass(1,"test2", (12, 23)))
    }

    test ("should render correctly") {
      assertEquals(
        renderer.render(
          pageIn,
          StepDetails[String, TestCaseClass](
            stepKey = "testRecord" :: Nil,
            fieldKey = "testRecord" :: Nil,
            tell = None,
            data = Map.empty,
            errors = ErrorTree.empty,
            validation = Rule.alwaysPass[TestCaseClass]
          )
        ),
        Some("INT[testRecord.a]∧STRING[testRecord.b]∧INT[testRecord.c._1]∧INT[testRecord.c._2]")
      )
    }

    test ("should instances should be inductively inferable for an either (coproduct)") {
      val presentation = implicitly[WebAsk[String, Either[String, Int]]]

      assertEquals(
        presentation.render(
          pageIn,
          StepDetails[String, Either[String, Int]](
            stepKey = "testRecord" :: Nil,
            fieldKey = "testRecord" :: Nil,
            tell = None,
            data = Map.empty,
            errors = ErrorTree.empty,
            validation = Rule.alwaysPass
          )
        ),
        Some("STRING[testRecord.Left.value]∨INT[testRecord.Right.value]")
      )

      testEncoding("test".asLeft[Int])
      testEncoding(12.asRight[Int])
    }
  }

}
