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
trait Quantifiable[A] {
  /** Returns the cardinality (number of elements) */
  def qty(in: A): Int
}

object Quantifiable:
  def instance[A](f: A => Int) = new Quantifiable[A] {
    def qty(in: A): Int = f(in)
  }

given Quantifiable[String] with
  def qty(in: String): Int = in.length
