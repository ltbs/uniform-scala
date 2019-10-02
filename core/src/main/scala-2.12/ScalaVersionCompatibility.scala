package ltbs.uniform

trait ScalaVersionCompatibility {

}

package validation {

  trait Compat {
    implicit def quantGenTranversable[A <: collection.GenTraversableOnce[_]] =
      Quantifiable.instance[A](x => x.size)
  }

}
