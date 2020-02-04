package ltbs.uniform

import shapeless._

/** Allows the extraction of the position of the first occurance of an
  * element in a `HList`. 
  * 
  * Part of the internal wiring of uniform's
  * implementation of KLists, you are not likely to need this for your
  * own journeys.
  * 
  * {{{
  * import shapeless._
  * import ltbs.uniform._
  * 
  * the[IndexOf[Int :: String :: Boolean :: HNil, String]].index // 1
  * the[IndexOf[String :: HNil, Boolean]] // compile error
  * }}}
  */
@annotation.implicitNotFound("Cannot find ${U} in ${L}")
trait IndexOf[L <: HList, U] extends DepFn1[L] with Serializable {

  /** Runtime value for the index of U in L */  
  val index: Int

  /** Type-level value for the index of U in L */
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
