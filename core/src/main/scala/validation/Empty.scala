package ltbs.uniform
package validation

import scala.language.implicitConversions

/** A type that can be checked to see if it is 'empty' (in the sense
  * of a Monoid being empty). Note that unlike with Monoid it does not
  * require a definition for empty, only the ability to check if an
  * element is empty */
trait Empty[A] {
  def isEmpty(in: A): Boolean
}

object Empty {
 def apply[A](implicit instance: Empty[A]): Empty[A] = instance

  trait Ops[A] {
    def typeClassInstance: Empty[A]
    def self: A
    def isEmpty: Boolean = typeClassInstance.isEmpty(self)
  }

  trait ToEmptyOps {
    implicit def toEmptyOps[A](target: A)(implicit tc: Empty[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }

  object nonInheritedOps extends ToEmptyOps

  trait AllOps[A] extends Ops[A] {
    def typeClassInstance: Empty[A]
  }

  object ops {
    implicit def toAllEmptyOps[A](target: A)(implicit tc: Empty[A]): AllOps[A] = new AllOps[A] {
      val self = target
      val typeClassInstance = tc
    }
  }
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
