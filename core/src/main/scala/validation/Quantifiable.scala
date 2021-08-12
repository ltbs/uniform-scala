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

object Quantifiable {
 def apply[A](implicit instance: Quantifiable[A]): Quantifiable[A] = instance

  trait Ops[A] {
    def typeClassInstance: Quantifiable[A]
    def self: A
    def qty: Int = typeClassInstance.qty(self)
  }

  trait ToQuantifiableOps {
    implicit def toQuantifiableOps[A](target: A)(implicit tc: Quantifiable[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }

  object nonInheritedOps extends ToQuantifiableOps

  trait AllOps[A] extends Ops[A] {
    def typeClassInstance: Quantifiable[A]
  }

  object ops {
    implicit def toAllQuantifiableOps[A](target: A)(implicit tc: Quantifiable[A]): AllOps[A] = new AllOps[A] {
      val self = target
      val typeClassInstance = tc
    }
  }
}

trait QuantifiableInstances {
  def instance[A](f: A => Int) = new Quantifiable[A] {
    def qty(in: A): Int = f(in)
  }

  implicit val quantString = instance[String](x => x.length)  
}
