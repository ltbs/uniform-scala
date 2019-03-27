package ltbs.uniform.web

import play.twirl.api.Html
import simulacrum.typeclass
import scala.language.implicitConversions

@typeclass trait Htmlable[A] {
  def toHtml(a: A): Html
}
