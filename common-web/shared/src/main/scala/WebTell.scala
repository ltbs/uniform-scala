package ltbs.uniform
package common.web

/** Represents rendering a type for a `tell` interaction used in a web
  * interpreter 
  */
trait WebTell[Html,A] {
  def render(in: A, key: List[String], pageIn: PageIn[Html]): Option[Html]
}

object WebTell {

  def fromFunction[Html, A](f: A => Html): WebTell[Html, A] = new WebTell[Html, A] {
    def render(in: A, key: List[String], pageIn: PageIn[Html]): Option[Html] = Some(f(in))
  }

  implicit def htmlTell[Html]: WebTell[Html, Html] = fromFunction(identity)

  implicit def tellUnit[Html]: WebTell[Html, Unit] = new WebTell[Html, Unit] {
    def render(unit: Unit, key: List[String], pageIn: PageIn[Html]): Option[Html] = None
  }
}
