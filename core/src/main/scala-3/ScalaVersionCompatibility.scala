package ltbs.uniform

trait ScalaVersionCompatibility {

}

package validation {

  trait Compat {
    implicit def quantGenTranversable[A <: collection.IterableOnce[_]]: Quantifiable[A] =
      Quantifiable.instance[A](x => x.iterator.size)
  }

}
