package ltbs.uniform

import scala.language.higherKinds
import shapeless._

@annotation.implicitNotFound("Cannot find an implicit ${F}[_] for each type in ${L}")
trait TypeclassList[L <: HList, F[_]] {
  type Repr <: HList
  protected val list: Repr
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
