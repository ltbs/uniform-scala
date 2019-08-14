package ltbs.uniform
package common.web

sealed trait AskResult[+A,Html]
object AskResult {
  final case class GotoPath[A,Html](path: List[String]) extends AskResult[A,Html] {
    def map[B] = GotoPath[B,Html](path)
  }
  final case class Payload[A,Html](html: Html, errors: ErrorTree) extends AskResult[A,Html] {
    def map[B] = Payload[B,Html](html, errors)
  }
  final case class Success[A,Html](objectOut: A) extends AskResult[A,Html]
}
