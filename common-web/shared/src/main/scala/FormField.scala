package ltbs.uniform
package common.web
import cats.implicits._

trait FormFieldEncoding[A]{
  def encode(in: A): Input
  def decode(out: Input): Either[ErrorTree,A]

  def imap[B](f: A => B)(g: B => A): FormFieldEncoding[B] = 
    simap[B](f(_).asRight)(g)

  def simap[B](f: A => Either[ErrorTree,B])(g: B => A): FormFieldEncoding[B] = {
    val orig = this

    new FormFieldEncoding[B] {
      def encode(in: B): Input = orig.encode(g(in))
      def decode(out: Input): Either[ErrorTree,B] = orig.decode(out) >>= f
    }
  }

}

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

trait FormField[A, Html] extends FormFieldEncoding[A] with FormFieldPresentation[A, Html] {
  override def imap[B](f: A => B)(g: B => A): FormField[B, Html] =
    simap[B](f(_).asRight)(g)

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
