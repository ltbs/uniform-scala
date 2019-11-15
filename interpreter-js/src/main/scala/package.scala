package ltbs.uniform
package interpreters

import cats.data._
import scala.concurrent.Future
import org.querki.jquery._

package object js extends common.web.webcommon {

  type Request = Option[Input]
  type Result = Unit

  def fromNode[A](key: String, fieldSet: JQuery): Either[ErrorTree,A] = {
    val fields = $("fieldset.uniform").serialize()
    println(s"raw data: $fields")
//    val decoded=FormUrlEncoded.readString(fields)
//    val input = decoded.toInputTree
//    parser.bind(input.children.getOrElse(key,Tree(Nil): Input))
    ???
  }
  
}
