package ltbs.uniform
package common.web

import validation.Rule

case class StepDetails[HTML, A](
  stepKey: List[String],
  fieldKey: List[String],
  tell: Option[HTML],
  data: Input,
  errors: ErrorTree,
  validation: Rule[A]
) {
  def /[B](key: String) = this.copy[HTML, B](
    fieldKey = this.fieldKey :+ key,
    tell = None,
    data = this.data / key,
    errors = this.errors / key,
    validation = Rule.alwaysPass[B]
  )
}
