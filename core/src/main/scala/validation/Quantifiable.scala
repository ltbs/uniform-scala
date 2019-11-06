package ltbs.uniform
package validation

import scala.language.implicitConversions

@simulacrum.typeclass trait Quantifiable[A] {
  def qty(in: A): Int
}

trait QuantifiableInstances {
  def instance[A](f: A => Int) = new Quantifiable[A] {
    def qty(in: A): Int = f(in)
  }

  implicit val quantString = instance[String](x => x.length)  
}
