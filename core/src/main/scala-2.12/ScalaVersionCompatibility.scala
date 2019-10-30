package ltbs.uniform

trait ScalaVersionCompatibility {

}

package validation {

  trait Compat extends QuantifiableInstances {
    implicit def quantGenTranversable[A <: collection.GenTraversableOnce[_]] =
      instance[A](x => x.size)
  }

}
