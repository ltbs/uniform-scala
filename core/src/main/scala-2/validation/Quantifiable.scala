package ltbs.uniform
package validation

import scala.language.implicitConversions

/** A type that has elements that can be counted 
  * 
  * {{{
  * Set(1,2,3).qty
  * "test".qty
  * }}}
  */
@simulacrum.typeclass trait Quantifiable[A] {

  /** Returns the cardinality (number of elements) */
  def qty(in: A): Int
}

trait QuantifiableInstances {
  def instance[A](f: A => Int): Quantifiable[A] = new Quantifiable[A] {
    def qty(in: A): Int = f(in)
  }

  implicit val quantString: Quantifiable[String] = instance[String](x => x.length)
}
