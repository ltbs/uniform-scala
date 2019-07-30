package ltbs.uniform
package common.web

trait GenericWebTell[A,Html] {
  def render(in: A, key: String, messages: UniformMessages[Html]): Html
}
