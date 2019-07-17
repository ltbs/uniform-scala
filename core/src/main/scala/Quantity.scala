package ltbs.uniform

import simulacrum._
import scala.language.implicitConversions

@typeclass trait Quantity[A] {
  def quantity(in: A): Int
}

trait QuantityInstances {
  implicit def integralQuantity[I: Integral] = new Quantity[I] {
    def quantity(in: I): Int = implicitly[Integral[I]].toInt(in)
  }

  implicit def sequenceQuantity[S](implicit ev: S ⇒ Seq[_]) = new Quantity[S] {
    def quantity(in: S): Int = in.size
  }

}

case class QuantityRule[A: Quantity] private[uniform](min: Int, max: Int) extends Rule[A] {
  require(min <= max, "minimum quantity is greater than max")
  def apply(in: A): ErrorTree = {
    val size: Int = in.quantity
    if (size < min) {
      ErrorTree.oneErr(ErrorMsg("too-small", min, size))
    } else if (size > max) {
      ErrorTree.oneErr(ErrorMsg("too-big", max, size))
    } else {
      ErrorTree.empty
    }
  }
}
