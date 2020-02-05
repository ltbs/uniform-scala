package ltbs.uniform
package common.web
import cats.implicits._

case class FormFieldStats(
  children: Int = 0,
  compoundChildren: Int = 0
) {
  final def isCompound: Boolean = children >= 2
}

/** Defines both the rendering and the encoding for a given datatype */
trait FormField[A, Html] extends Codec[A] {

  def stats: FormFieldStats = FormFieldStats()

  def render(
    pageKey: List[String],
    fieldKey: List[String],    
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html

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
        pageKey: List[String],
        fieldKey: List[String],        
        breadcrumbs: Breadcrumbs,
        data: Input,
        errors: ErrorTree,
        messages: UniformMessages[Html]
      ): Html = orig.render(pageKey, fieldKey, breadcrumbs, data, errors, messages)

      override def stats = orig.stats
    }
  }
}
