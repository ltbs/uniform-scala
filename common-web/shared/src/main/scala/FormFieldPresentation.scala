package ltbs.uniform
package common.web

/** Controls the rendering of a field in a web form */
trait FormFieldPresentation[Html, A]{
  def render(
    key: List[String],
    path: Breadcrumbs,
    data: Option[Input],
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html

  def mapToType[B] = {
    val orig = this
    new FormFieldPresentation[Html, B] {
      def render(
        key: List[String],
        path: Breadcrumbs,
        data: Option[Input],
        errors: ErrorTree,
        messages: UniformMessages[Html]
      ): Html = orig.render(key, path, data, errors, messages)
    }
  }
}
