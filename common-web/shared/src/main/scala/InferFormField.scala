package ltbs.uniform
package common.web

trait InferFormField[Html] extends InferFormFieldPresentation[Html] with InferFormFieldEncoding {

}

object InferFormField {

  def combine[A,Html](
    codec: FormFieldEncoding[A],
    renderer: FormFieldPresentation[A, Html]
  ): FormField[A, Html] = new FormField[A, Html] {
  // Members declared in ltbs.uniform.common.web.FormFieldEncoding
  def decode(out: Input): Either[ErrorTree,A] = codec.decode(out)
  def encode(in: A): Input = codec.encode(in)
   
  // Members declared in ltbs.uniform.common.web.FormFieldPresentation
    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = renderer.render(key, path, data, errors, messages)
  }
}
