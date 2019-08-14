package ltbs.uniform
package common.web

trait WebMonadConstructor[A, Html] {
  def apply(
    id: String,
    tell: Html,
    defaultIn: Option[A],
    validationIn: List[List[Rule[A]]],
    messages: UniformMessages[Html]
  ): WebMonad[A, Html]
}
