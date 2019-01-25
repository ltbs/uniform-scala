package ltbs.uniform.web

import play.twirl.api.Html
import simulacrum.typeclass

@typeclass trait Htmlable[A] {
  def toHtml(a: A): Html
}
