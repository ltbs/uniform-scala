package ltbs.uniform
package examples

import scalatags._
import org.scalajs.dom

trait Widgets extends AbstractWidgets[dom.Element, dom.Element, dom.Node] {
  val bundle = JsDom
}
