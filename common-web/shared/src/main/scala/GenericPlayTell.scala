package ltbs.uniform
package common.web

// import scala.concurrent._
// import cats.syntax.eq._

/** Represents rendering a type for a `tell` interaction used in a web
  * interpreter 
  */
trait GenericWebTell[Html,A] {
  def render(in: A, key: String, messages: UniformMessages[Html]): Option[Html]

  // def end(
  //   in: A,
  //   key: String,
  //   customContent: Map[String,(String,List[Any])]
  // ) = new WebMonad[Html, Nothing] {
  //   def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html, Nothing]] =
  //     Future.successful{
  //       import pageIn._
  //       val messages = pageIn.messages.withCustomContent(customContent)
  //       val targetIdP = targetId.reverse.dropWhile(_ == "").reverse
  //       val currentId = pageIn.pathPrefix :+ key

  //       if (targetIdP === currentId) {
  //         pageIn.toPageOut(AskResult.Payload(
  //           render(in, key, messages),
  //           ErrorTree.empty, messages)
  //         )
  //       } else {
  //         // unlike in PostAndGetPage we don't care if they are trying
  //         // to access a page after this one, because there are no
  //         // pages after this one!
  //         pageIn.toPageOut(AskResult.GotoPath[Html,Nothing](currentId))
  //       }
  //     }
  // }

  // def pureHtml(
  //   in: A,
  //   key: String,
  //   customContent: Map[String,(String,List[Any])]
  // ) = new WebMonad[Html, Html] {
  //   def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html, Html]] =
  //     Future.successful{
  //       val messages = pageIn.messages.withCustomContent(customContent)
  //       pageIn.toPageOut(AskResult.Success(render(in, key, messages)))
  //     }
  // }
}
