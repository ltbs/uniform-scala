package examples.js

import ltbs.uniform._
import interpreters.js._
import common.web._
import examples.beardtax._
import cats.implicits._

import org.querki.jquery._
import scala.scalajs._, js.annotation.JSExportTopLevel
import scala.concurrent._, ExecutionContext.Implicits.global
import scalatags.JsDom.all._
import ltbs.uniform.validation.Rule

object BeardTaxApp extends JsInterpreter[Tag]($("#uniform")) with InferWebAsk[Tag] with examples.Widgets {

  def render(in: Option[Tag]): String = in.fold("")(_.toString)

  def renderAnd(
    pageKey: List[String],
    fieldKey: List[String],
    tell: Option[Tag],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Tag],
    members: Seq[(String, Tag)]
  ): Tag = fieldSurround(fieldKey, tell, errors, messages) {
    table(
      members.map { case (label, html) =>
        tr(th(label), td(html))
      }
    )
  }

  def renderOr(
    pageKey: List[String],
    fieldKey: List[String],
    tell: Option[Tag],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Tag],
    alternatives: Seq[(String, Option[Tag])],
    selected: Option[String]
  ): Tag = radios(
    fieldKey,
    tell,
    alternatives.map(_._1),
    selected,
    errors,
    messages,
    alternatives.collect{case (k, Some(v)) => (k,v)}.toMap
  )

  import cats.~>
  implicit def e = new ~>[WebMonad[?,Tag],WebMonad[?,Tag]]{
    def apply[A](in: WebMonad[A,Tag]): WebMonad[A,Tag] = in
  }

  val messages: UniformMessages[Tag] = UniformMessages.echo.map(span(_))

  val jsJourney = interpret(beardProgram(jsHod))
  jsJourney.run()(PageIn(Nil, crumbs, None, state, Nil, JourneyConfig(), messages))

  def jsHod = new Hod[WebMonad[Tag, *]] {
    def costOfBeard(beardStyle: BeardStyle, length: BeardLength): WebMonad[Tag, Int] =
      12.pure[WebMonad[Tag, ?]]

    def recordBeardHeight(height: Int): WebMonad[Tag, Unit] =
      ().pure[WebMonad[Tag, ?]]
  }

  @JSExportTopLevel("back")
  def backLink(): Future[Unit] =
    jsJourney.run()(PageIn(crumbs.init.last, crumbs, None, state, Nil, JourneyConfig(), messages))

  @JSExportTopLevel("submit")
  def submit(): Boolean = {
    jsJourney.run()(PageIn(key, Nil, getData.toOption, state, Nil, JourneyConfig(), messages))
    false;
  }

  def main(args: Array[String]): Unit = {
    println("Hello world!")
  }

}
