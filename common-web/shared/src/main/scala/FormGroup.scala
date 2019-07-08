package ltbs.uniform
package common.web
import cats.implicits._

trait FormGroup[A, Html]{
  def encode(in: A): Input
  def decode(out: Input): Either[ErrorTree,A]
  def render(
    key: List[String],
    path: Path,
    data: Option[Input],
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html

  def imap[B](f: A => B)(g: B => A): FormGroup[B, Html] = 
    simap[B](f(_).asRight)(g)

  def simap[B](f: A => Either[ErrorTree,B])(g: B => A): FormGroup[B, Html] = {
    val orig = this

    new FormGroup[B, Html] {
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
