package ltbs.uniform
package examples
package js

import beardtax._
import interpreters.js._
import common.web._

import cats.implicits._
import org.querki.jquery._
import scala.scalajs._, js.annotation.JSExportTopLevel
import scala.concurrent._, ExecutionContext.Implicits.global
import scalatags.JsDom.all._

object BeardTaxApp extends App {

  val interpreter = new JsInterpreter[Tag] with InferFormFieldProduct[Tag] with InferFormFieldCoProduct[Tag] with examples.Widgets {

    def renderProduct[A](
      key: List[String],
      path: Path,
      values: Input,
      errors: ErrorTree,
      messages: UniformMessages[Tag],
      pfl: ProductFieldList[A]
    ): Tag = div(
      pfl.inner map { case (subFieldId, f) =>
        f(key:+ subFieldId, path, values / subFieldId, errors / subFieldId, messages)
      }
    )

    def renderCoproduct[A](
      key: List[String],
      path: Path,
      values: Input,
      errors: ErrorTree,
      messages: UniformMessages[Tag],
      cfl: CoproductFieldList[A]
    ): Tag = {
      val value: Option[String] = values.valueAtRoot.flatMap{_.headOption}
      radios(
        key,
        cfl.inner.map{_._1},
        value,
        errors,
        messages,
        cfl.inner.map{
          case(subkey,f) => subkey -> f(key :+ subkey, path, {values / subkey}, errors / subkey, messages)
        }.filter(_._2.toString.trim.nonEmpty).toMap
      )
    }

    implicit val tellTwirlUnit = new WebTell[Unit] {
      def render(in: Unit, key: String, messages: UniformMessages[Tag]): Tag = span("")
    }

    def renderFrame(
      frame: JQuery,
      htmlForm: Tag,
      errors: ErrorTree,
      messages: UniformMessages[Tag]
    ): Future[Unit] = Future {
      frame.html(htmlForm.toString)
      ()
    }
  }

  import interpreter._

  val i = interpreter.create[TellTypes, AskTypes](UniformMessages.echo.map{span(_)})

  def jsHod = new Hod[WebMonad[?, Tag]] {
    def costOfBeard(beardStyle: BeardStyle, length: BeardLength): WebMonad[Int, Tag] =
      12.pure[WebMonad[?, Tag]]
  }

  val runner = {
    new interpreter.JsRunner[Int](
    beardProgram[interpreter.WM](i, jsHod),
    $("#uniform"),
      diagnostics = true
    )(output => Future($("#uniform").html(output.toString)))
  }

  @JSExportTopLevel("back")
  def backLink(): Future[Unit] = runner.goBack()

  @JSExportTopLevel("submit")
  def submit(): Future[Unit] = runner.submit()

}
