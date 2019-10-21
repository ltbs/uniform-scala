package ltbs.uniform

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
@annotation.implicitNotFound("Cannot find an implicit ${F}[_] for each type in ${L}")
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

  implicit def tclHNil[F[_]] = new TypeclassList[HNil, F] {
    type Repr = HNil
    val list: Repr = HNil
  }

  implicit def tclHCons[F[_], H, T <: HList, TailRepr <: HList](
    implicit fa: F[H],
    tailTypeclassList: TypeclassList.Aux[T, F, TailRepr]
  ) = new TypeclassList[H :: T, F] {
    type Repr = F[H] :: TailRepr
    val list: Repr = fa :: tailTypeclassList.list
  }

  def apply[L <: HList, TC[_]](implicit tcl: TypeclassList[L, TC]) = tcl
}
