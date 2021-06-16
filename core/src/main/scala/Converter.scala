package ltbs.uniform

import scala.language.higherKinds
import cats.~>

@annotation.implicitNotFound("Could not find an implicit Converter[${E}, ${F}, ${A}], consider implementing an implicit ${E} ~> ${F} or ${E}[${A}] => ${F}[${A}] unless you need fine control.")
trait Converter[E[_], F[_], A] {
  def apply(key: String, in: () => E[A]): F[A]
}

object Converter {
  implicit def natTransformToConverter[E[_], F[_], A](implicit natConv: E ~> F): Converter[E, F, A] =
    new Converter[E, F, A] {
      override def apply(key: String, in: () => E[A]): F[A] = natConv(in())
    }

  implicit def functionToConverter[E[_], F[_], A](implicit func: E[A] => F[A]): Converter[E, F, A] =
    new Converter[E, F, A] {
      override def apply(key: String, in: () => E[A]): F[A] = func(in())
    }
}
