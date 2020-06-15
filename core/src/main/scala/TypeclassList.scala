package ltbs.uniform

import scala.language.higherKinds
import scala.language.experimental.macros
import shapeless._

/** A KList
  * 
  * For any `F[_]` and Hlist `L` of `A :: B :: ... :: Z :: HNil` 
  * provides a `Repr` of  `F[A] :: F[B] :: ... :: F[Z] :: HNil`
  * by looking for an implicit instance of each member in turn.
  * 
  * {{{
  * import cats.Monoid, cats.implicits._
  * import shapeless._
  * val klist = TypeclassList[Int :: String :: HNil, Monoid]
  * val stringMonoid = klist.forType[String].empty
  * }}}
  */
trait TypeclassList[L <: HList, F[_]] {

  /** KList containing an `F[A]` for every `A` in `L` */
  type Repr <: HList
  protected val list: Repr

  /** retrieve an `F[T]` for `T` from the KList */
  def forType[T](implicit sel: Cached[IndexOf[L, T]]): F[T] =
    HList.unsafeGet(list, sel.value.index).asInstanceOf[F[T]]

}

object TypeclassList {

  type Aux[L <: HList, F[_], Repr0] = TypeclassList[L, F] { type Repr = Repr0 }

  implicit def apply[L <: HList, TC[_]]: TypeclassList[L, TC] =
    macro TypeclassListMacros.fromImplicits[L,TC]

}
