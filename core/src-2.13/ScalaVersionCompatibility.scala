package ltbs.uniform

trait ScalaVersionCompatibility {

}

package validation {

  trait Compat extends QuantifiableInstances {
    implicit def quantGenTranversable[A <: collection.IterableOnce[_]] =
      instance[A](x => x.iterator.size)
  }

}
