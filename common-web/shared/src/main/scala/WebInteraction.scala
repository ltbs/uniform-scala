package ltbs.uniform
package common.web
import validation.Rule

trait WebInteraction[Html, A] {
  def apply(
    id: String,
    tell: Option[Html],
    defaultIn: Option[A],
    validationIn: Rule[A],
    customContent: Map[String,(String,List[Any])]
  ): WebMonad[Html, A]
}
