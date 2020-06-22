package ltbs.uniform
package common.web
import validation.Rule

trait WebInteraction[A, Html] {
  def apply(
    id: String,
    tell: Html,
    defaultIn: Option[A],
    validationIn: Rule[A]
  ): WebMonad[A, Html]
}
