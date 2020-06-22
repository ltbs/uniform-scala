package ltbs.uniform
package common.web
import validation.Rule

trait WebInteraction[A, Html] {
  def apply(
    id: String,
    tell: Option[Html],
    defaultIn: Option[A],
    validationIn: Rule[A]
  ): WebMonad[A, Html]
}
