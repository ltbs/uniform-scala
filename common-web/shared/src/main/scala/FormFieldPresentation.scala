package ltbs.uniform
package common.web

/** Controls the rendering of a field in a web form */
trait FormFieldPresentation[A, Html]{
  def render(
    key: List[String],
    path: Path,
    data: Option[Input],
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html

  def mapToType[B] = {
    val orig = this
    new FormFieldPresentation[B, Html] {
      def render(
        key: List[String],
        path: Path,
        data: Option[Input],
        errors: ErrorTree,
        messages: UniformMessages[Html]
      ): Html = orig.render(key, path, data, errors, messages)
    }
  }
}
