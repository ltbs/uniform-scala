package ltbs.uniform
package common.web
import validation.Rule

trait WebMonadConstructor[A, Html] {
  def apply(
    id: String,
    tell: Html,
    defaultIn: Option[A],
    validationIn: List[Rule[A]],
    messages: UniformMessages[Html]
  ): WebMonad[A, Html]
}
