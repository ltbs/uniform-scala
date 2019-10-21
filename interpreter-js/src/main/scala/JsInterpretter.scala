package ltbs.uniform
package interpreters.js

import common.web._
import cats.implicits._
import org.querki.jquery._
import concurrent._

abstract class JsInterpreter[Html](
  domSelector: String
)( implicit
  ec: ExecutionContext
) extends GenericWebInterpreter[Html]{

  var state: DB = DB.empty
  var breadcrumbs: Path = List.empty
  var currentId: List[String] = Nil

  val dom = $(domSelector)

  def pageChrome(
    html: Html,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    isCompoundField: Boolean    
  ): Html

  def messages: UniformMessages[Html]

  def readData: Input = {
    val fields = dom.serialize()
    Input.fromUrlEncodedString(fields) match {
      case Left(e) => throw new IllegalStateException(e.toString)
      case Right(r) => r
    }
  }

  implicit class JsWebMonad[A](wm: WebMonad[A, Html]) {

    case class withFinalAction(
      f: A => Html
    ) {

      def submit = run(currentId, Some(readData))
      def initial = run(currentId, None)
      def goto(targetId: List[String]) = run(targetId, None)
      def back = run(breadcrumbs.lastOption.getOrElse(Nil), None)

      def run(
        targetId: List[String],
        data: Option[Input]
      ): Future[Unit] = {

        wm(PageIn(targetId, Nil, data, state)) flatMap {
          case common.web.PageOut(pathOut, dbOut, pageOut) =>
            state = dbOut
            breadcrumbs = pathOut
            pageOut match {
              case AskResult.GotoPath(targetPath) =>
                run(targetPath, None)
              case AskResult.Payload(html, errors, msg, isCompound) =>
                dom.html(pageChrome(html, errors, msg, isCompound).toString)
                ().pure[Future]
              case AskResult.Success(result) =>
                dom.html(f(result).toString)
                ().pure[Future]
            }
        }
      }
    }
  }
}
