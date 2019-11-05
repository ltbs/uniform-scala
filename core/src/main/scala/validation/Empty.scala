package ltbs.uniform
package validation

import language.implicitConversions

@simulacrum.typeclass trait Empty[A] {
  def isEmpty(in: A): Boolean
}

trait EmptyInstances {

  def newEmpty[A](e: A) = new Empty[A] {
    def empty = e
    def isEmpty(in: A): Boolean = in == empty
  }

  implicit val emptyString = newEmpty("")
  implicit val emptyInt = newEmpty(0)
  implicit def emptyOpt[A] = {
    import cats.syntax.option._
    newEmpty(none[A])
  }

  implicit def emptyNumeric[A](
    implicit num: Numeric[A]
  ) = newEmpty(num.zero)

  implicit def emptyMonoid[A](
    implicit mon: cats.Monoid[A]
  ) = newEmpty(mon.empty)

  implicit def emptyQuantifiable[A](
    implicit qty: Quantifiable[A]
  ) = new Empty[A] {
    def isEmpty(in: A): Boolean = qty.qty(in) == 0
  }

}
