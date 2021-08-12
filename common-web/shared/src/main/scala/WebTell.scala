package ltbs.uniform
package common.web

/** Represents rendering a type for a `tell` interaction used in a web
  * interpreter 
  */
trait WebTell[Html,A] {
  def render(in: A, key: String, messages: UniformMessages[Html]): Option[Html]
}

object WebTell {

  def fromFunction[Html, A](f: A => Html): WebTell[Html, A] = new WebTell[Html, A] {
    def render(in: A, key: String, messages: UniformMessages[Html]): Option[Html] = Some(f(in))
  }

  implicit def htmlTell[Html]: WebTell[Html, Html] = fromFunction(identity)
}
