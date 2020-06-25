package ltbs.uniform
package common.web

import cats.data.Ior

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
    tell: Option[Html],
    ask: Option[Html], 
    errors: ErrorTree = ErrorTree.empty,
    messages: UniformMessages[Html]
  ) extends AskResult[A,Html] {
    def map[B] = Payload[B,Html](tell, ask, errors, messages)
  }

  final case class Success[A,Html](objectOut: A) extends AskResult[A,Html]
}
