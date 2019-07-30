package ltbs.uniform
package common.web
import cats.implicits._

/** Defines both the rendering and the encoding for a given datatype */
trait FormField[A, Html] extends FormFieldEncoding[A] with FormFieldPresentation[A, Html] {

  /** Produce a new `FormField` from this one by mapping the types */
  override def imap[B](f: A => B)(g: B => A): FormField[B, Html] =
    simap[B](f(_).asRight)(g)

  /** Produce a new `FormField` from this one, with the
    * possibility of extending the validation
    */
  override def simap[B](f: A => Either[ErrorTree,B])(g: B => A): FormField[B, Html] = {
    val orig = this

    new FormField[B, Html] {
      def encode(in: B): Input = orig.encode(g(in))
      def decode(out: Input): Either[ErrorTree,B] = orig.decode(out) >>= f
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

object FormField {

  /** Create a new [[FormField]] from a [[FormFieldPresentation]] and a [[FormFieldEncoding]] */
  def build[A,Html](
    codec: FormFieldEncoding[A],
    renderer: FormFieldPresentation[A, Html]
  ) = new FormField[A, Html] {
    def encode(in: A) = codec.encode(in)
    def decode(out: Input) = codec.decode(out)
    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ) = renderer.render(key, path, data, errors, messages)
  }
}
