package ltbs.uniform

import shapeless._

/** Find the index of the first instance of a given type in a HList */
@annotation.implicitNotFound("Cannot find ${U} in ${L}")
trait IndexOf[L <: HList, U] extends DepFn1[L] with Serializable {

  /** The index of the type as a value */
  val index: Int

  /** The index of the type as a Nat */
  type Out = Nat
}

object IndexOf {
  type Aux[L <: HList, U,N <: Nat] = IndexOf[L,U] { type Out = N }

  implicit def select[H, T <: HList]: IndexOf[H :: T, H] =
    new IndexOf[H :: T, H] {
      val index = 0
      def apply(l : H :: T) = Nat._0
    }

  implicit def recurse[H, T <: HList, U, N <: Nat]
    (implicit st : IndexOf.Aux[T, U, N]): IndexOf[H :: T, U] =
    new IndexOf[H :: T, U] {
      val index = st.index + 1
      def apply(l : H :: T) = Succ[N]
    }

}
