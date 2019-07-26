package ltbs.uniform
package common.web

/** Represents rendering a type for a `tell` interaction used in a web
  * interpreter 
  */
trait GenericWebTell[A,Html] {
  def render(in: A, key: String, messages: UniformMessages[Html]): Html
}
