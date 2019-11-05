package ltbs.uniform
package common.web

/** An abstracted result from asking the user for a value via a web
  * interface. Each web interpreter will want to convert this into
  * it's own representation 
  */
sealed trait AskResult[+A,Html]
object AskResult {
  final case class GotoPath[A,Html](path: List[String]) extends AskResult[A,Html] {
    def map[B] = GotoPath[B,Html](path)
  }

  final case class Payload[A,Html](
    html: Html,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    stats: FormFieldStats
  ) extends AskResult[A,Html] {
    def map[B] = Payload[B,Html](html, errors, messages, stats)
  }

  final case class Success[A,Html](objectOut: A) extends AskResult[A,Html]
}
