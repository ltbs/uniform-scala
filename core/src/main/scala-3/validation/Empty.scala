package ltbs.uniform
package validation

import scala.language.implicitConversions

/** A type that can be checked to see if it is 'empty' (in the sense
  * of a Monoid being empty). Note that unlike with Monoid it does not
  * require a definition for empty, only the ability to check if an
  * element is empty */
trait Empty[A] {
  extension(in: A) {
    def isEmpty: Boolean
  }
}

object Empty {
  trait ToEmptyOps {
    // for backwards compat with scala 2/simulacrum
  }
}

trait EmptyInstances {

  /** Convenience method to create an Empty instance from a supplied value 
    * {{{ 
    * implicit val emptyString: Empty[String] = instance("")
    * }}}
    */
  def instance[A](e: A) = new Empty[A] {
    extension(in: A) {
      def isEmpty: Boolean = in == empty
    }

    def empty = e
  }

  implicit val emptyString: Empty[String] = instance("")
  implicit val emptyInt: Empty[Int] = instance(0)
  implicit def emptyOpt[A]: Empty[Option[A]] = {
    import cats.syntax.option._
    instance(none[A])
  }

  implicit def emptyNumeric[A](
    implicit num: Numeric[A]
  ): Empty[A] = instance(num.zero)

  implicit def emptyMonoid[A](
    implicit mon: cats.Monoid[A]
  ): Empty[A] = instance(mon.empty)

  implicit def emptyQuantifiable[A](
    implicit qty: Quantifiable[A]
  ): Empty[A] = new Empty[A] {
    extension(in: A) {
      def isEmpty: Boolean = qty.qty(in) == 0
    }
  }

}
