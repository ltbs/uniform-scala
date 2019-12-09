package ltbs.uniform
package common.web
import cats.implicits._

case class FormFieldStats(
  children: Int = 0,
  compoundChildren: Int = 0
) {
  final def isCompound: Boolean = children >= 2
}

trait FormField[A, Html] extends Codec[A] {

  def stats: FormFieldStats = FormFieldStats()

  def render(
    key: List[String],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html

  override def imap[B](f: A => B)(g: B => A): FormField[B, Html] =
    simap[B](f(_).asRight)(g)

  override def simap[B](f: A => Either[ErrorTree,B])(g: B => A): FormField[B, Html] = {
    val orig = this

    new FormField[B, Html] {
      def encode(in: B): Input = orig.encode(g(in))
      def decode(out: Input): Either[ErrorTree,B] = orig.decode(out) >>= f
      def render(
        key: List[String],
        breadcrumbs: Breadcrumbs,
        data: Input,
        errors: ErrorTree,
        messages: UniformMessages[Html]
      ): Html = orig.render(key, breadcrumbs, data, errors, messages)

      override def stats = orig.stats
    }
  }
}
