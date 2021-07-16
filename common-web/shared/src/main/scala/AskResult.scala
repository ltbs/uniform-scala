package ltbs.uniform
package common.web

/** An abstracted result from asking the user for a value via a web
  * interface. Each web interpreter will want to convert this into
  * it's own representation 
  */
sealed trait AskResult[Html,+A]
object AskResult {
  final case class GotoPath[Html,A](path: List[String]) extends AskResult[Html,A] {
    def map[B] = GotoPath[Html,B](path)
  }

  final case class Payload[Html, A](
    html: Option[Html],
    errors: ErrorTree = ErrorTree.empty,
    messages: UniformMessages[Html]
  ) extends AskResult[Html,A] {
    def map[B] = Payload[Html,B](html, errors, messages)
  }

  final case class Success[Html,A](objectOut: A) extends AskResult[Html,A]
}
