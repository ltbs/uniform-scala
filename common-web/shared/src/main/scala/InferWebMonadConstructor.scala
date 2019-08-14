package ltbs.uniform
package common.web

import concurrent._

trait InferWebMonadConstructor[Html]
    extends InferFormFieldPresentation[Html]
    with InferFormFieldEncoding

object InferWebMonadConstructor {

  def combine[A,Html](
    codecIn: FormFieldEncoding[A],
    renderer: FormFieldPresentation[A, Html]
  ): WebMonadConstructor[A, Html] = new PostAndGetPage[A, Html] {

    def codec: FormFieldEncoding[A] = codecIn

    def getPage(
      key: List[String],
      state: DB,
      existing: Input,
      path: Path,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Html =
      renderer.render(key, path, existing, ErrorTree.empty, messages)

    def postPage(
      key: List[String],
      state: DB,
      request: Input,
      errors: ErrorTree,
      path: Path,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Html =
      renderer.render(key, path, request, errors, messages)

  }
}
