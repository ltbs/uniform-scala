package ltbs.uniform
package common.web

import scala.concurrent._
import cats.data.Ior
import cats.syntax.eq._

/** Represents rendering a type for a `tell` interaction used in a web
  * interpreter 
  */
trait GenericWebTell[A,Html] {
  def render(in: A, key: String, messages: UniformMessages[Html]): Html

  def end(in: A, key: String) = new WebMonad[Nothing, Html] {
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Nothing, Html]] =
      Future.successful{
        import pageIn._
        val targetIdP = targetId.reverse.dropWhile(_ == "").reverse
        val currentId = pageIn.pathPrefix :+ key

        if (targetIdP === currentId) {
          pageIn.toPageOut(AskResult.Payload(
            Ior.left(render(in, key, pageIn.messages)),
            ErrorTree.empty, pageIn.messages)
          )
        } else {
          // unlike in PostAndGetPage we don't care if they are trying
          // to access a page after this one, because there are no
          // pages after this one!
          pageIn.toPageOut(AskResult.GotoPath[Nothing,Html](currentId))
        }
      }
  }

  def pureHtml(in: A, key: String) = new WebMonad[Html, Html] {
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html, Html]] =
      Future.successful{
        pageIn.toPageOut(AskResult.Success(render(in, key, pageIn.messages)))
      }
  }
}
