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

  extension(a: A) {

    /** Returns the cardinality (number of elements) */
    def qty: Int

  }
}

object Quantifiable {
  trait ToQuantifiableOps {
    // for backwards compat with scala 2/simulacrum
  }
}

trait QuantifiableInstances {
  def instance[A](f: A => Int): Quantifiable[A] = new Quantifiable[A] {
    extension(a: A) {

      /** Returns the cardinality (number of elements) */
      def qty: Int = f(a)

    }
  }

  inline val quantString: Quantifiable[String] = instance[String](x => x.length)
}
