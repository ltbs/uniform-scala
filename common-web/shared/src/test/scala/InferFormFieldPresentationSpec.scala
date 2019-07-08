package ltbs.uniform
package common.web

import org.scalatest._
import cats.implicits._

object SampleFormFieldRenderers extends SampleFormFieldRenderers
trait SampleFormFieldRenderers {

  def selectionOfFields(inner: List[(String, (List[String], Path, Option[Input], ErrorTree, UniformMessages[String]) ⇒ String)])(key: List[String], path: Path, values: Option[Input], errors: ErrorTree, messages: UniformMessages[String]):String = key.mkString(".") ++ ":" ++ inner.map(_._1).mkString(",")

  implicit val stringFieldR = new FormFieldPresentation[String, String] {
    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[String]
    ): String = {
      val k = key.mkString(".")
      s"STRING[$k]"
    }
  }

  implicit val intFieldR = new FormFieldPresentation[Int, String] {
    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[String]
    ): String = {
      val k = key.mkString(".")
      s"INT[$k]"
    }
  }

}

class InferFormFieldPresentationSpec extends FlatSpec with Matchers {

  object Presenter extends InferFormFieldPresentation[String] with SampleFormFieldRenderers {}
  import Presenter._

  val mon: cats.Monoid[String] = implicitly
  def selectionOfFields(inner: List[(String, (List[String], Path, Option[Input], ErrorTree, UniformMessages[String]) ⇒ String)])(key: List[String],path: Path, values: Option[Input],errors: ErrorTree,messages: UniformMessages[String]): String = {
    key.mkString(".") ++ ":" ++ inner.map{_._1}.mkString(",")
  }

  def testEncoding[A](in: A)(implicit codec: FormFieldEncoding[A]): org.scalatest.Assertion = {
    import codec._
    decode(encode(in)) should be ((in).asRight[ErrorTree])
  }

  "FormFieldPresentation" should "infer a renderer for a case class" in {
    val renderer = implicitly[FormFieldPresentation[TestCaseClass, String]]
    renderer.render(List("testRecord"), Nil, None, ErrorTree.empty, UniformMessages.noop) should be (
      "INT[testRecord.a]STRING[testRecord.b]INT[testRecord.c._1]INT[testRecord.c._2]"
    )
  }

  it should "infer a renderer for an either (coproduct)" in {
    type TestType = Either[String, Int]
    val presentation = implicitly[FormFieldPresentation[TestType, String]]

    presentation.render(List("testRecord"), Nil, None, ErrorTree.empty, UniformMessages.noop) should be (
      "testRecord:Left,Right"
    )
  }
}
