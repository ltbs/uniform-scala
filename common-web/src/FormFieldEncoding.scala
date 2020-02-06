package ltbs.uniform
package common.web
import cats.implicits._

/** A codec for a datatype to make it usable as a form field inside a
  * uniform web interpreter. Technically this is a split epimorphism between 
  * `Input => Either[ErrorTree,A]` and `A => Input`.
  */
trait FormFieldEncoding[A]{
  def encode(in: A): Input
  def decode(out: Input): Either[ErrorTree,A]

  /** Produce a new `FormFieldEncoding` from this one by mapping the types */  
  def imap[B](f: A => B)(g: B => A): FormFieldEncoding[B] = 
    simap[B](f(_).asRight)(g)

  /** Produce a new `FormFieldEncoding` from this one, with the
    * possibility of extending the validation 
    */
  def simap[B](f: A => Either[ErrorTree,B])(g: B => A): FormFieldEncoding[B] = {
    val orig = this

    new FormFieldEncoding[B] {
      def encode(in: B): Input = orig.encode(g(in))
      def decode(out: Input): Either[ErrorTree,B] = orig.decode(out) >>= f
    }
  }

}
