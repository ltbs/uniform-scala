package ltbs.uniform
package validation

import language.implicitConversions

/** A type that can be checked to see if it is 'empty' (in the sense
  * of a Monoid being empty). Note that unlike with Monoid it does not
  * require a definition for empty, only the ability to check if an
  * element is empty */
@simulacrum.typeclass trait Empty[A] {
  def isEmpty(in: A): Boolean
}

trait EmptyInstances {

  /** Convenience method to create an Empty instance from a supplied value 
    * {{{ 
    * implicit val emptyString: Empty[String] = instance("")
    * }}}
    */
  def instance[A](e: A) = new Empty[A] {
    def empty = e
    def isEmpty(in: A): Boolean = in == empty
  }

  implicit val emptyString = instance("")
  implicit val emptyInt = instance(0)
  implicit def emptyOpt[A] = {
    import cats.syntax.option._
    instance(none[A])
  }

  implicit def emptyNumeric[A](
    implicit num: Numeric[A]
  ) = instance(num.zero)

  implicit def emptyMonoid[A](
    implicit mon: cats.Monoid[A]
  ) = instance(mon.empty)

  implicit def emptyQuantifiable[A](
    implicit qty: Quantifiable[A]
  ) = new Empty[A] {
    def isEmpty(in: A): Boolean = qty.qty(in) == 0
  }

}
