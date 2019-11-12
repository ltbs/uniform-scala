package ltbs

import language.higherKinds

import cats.implicits._
import cats.{Monoid, Applicative, Monad, Eq, Semigroup}
import cats.data.{NonEmptyList, Validated}
import shapeless.tag, tag.{@@}
import collection.immutable.ListMap
import uniform.validation.{Rule, Transformation}

package object uniform
    extends TreeLike.ToTreeLikeOps
    with TreeLikeInstances
    with ScalaVersionCompatibility
{

  /** Used to represent multi-line input.
    *
    * Behaves identically to, and can be freely cast to, a
    * String. However interpreters may decide to treat it
    * differently - for example a web interpreter will usually render
    * this a textarea or a cli interpreter may prompt for several
    * lines.
    */
  type BigString = String @@ BigStringTag

  type NonEmptyString = String @@ NonEmptyStringTag

  type InputPath = List[String]
  type Input = Map[InputPath, List[String]]
  type ErrorTree = ListMap[NonEmptyList[InputPath], NonEmptyList[ErrorMsg]]
  type ::[H,T <: shapeless.HList] = shapeless.::[H,T]
  type NilTypes = Unit :: shapeless.HNil

  implicit object Input extends MapTree[String, List[String]] {

    /** Extract an [[Input]] from a UTF-8 URL encoded String. */
    def fromUrlEncodedString(in: String): Either[ErrorTree,Input] = {
      val ungrouped: List[(String, String)] =
        in.split("&").toList
          .map{_.split("=").toList}
          .collect { case (k::v::Nil) => k ->
            java.net.URLDecoder.decode(v, "UTF-8")
          }

      ungrouped.groupBy(_._1).map{ case (k, comb) =>
        k.split("[.]").toList.dropWhile(_.isEmpty) -> comb.map {_._2}
      }.asRight
    }
  }

  implicit class RichInput(input: Input) {

    /** Generate a UTF-8 URL encoded String. */    
    def toUrlEncodedString: String = {
      input
        .flatMap { case (k, vs) =>
          vs.map { v =>
            s"""${k.mkString(".")}=${java.net.URLEncoder.encode(v, "UTF-8")}"""
          }
        }
        .mkString("&")
    }

    /** Take the string at the root of the input tree and pass it
      * through the transformation pipeline provided. This is usually
      * done when constructing a codec for a datatype. 
      * 
      * For example - 
      * {{{
      * val someBool: Validated[ErrorTree, Boolean] = 
      *   someInput.toField[Boolean]{x: String =>
      *     Validated.catchOnly[IllegalArgumentException](
      *       x.toBoolean
      *     ).leftMap(_ => ErrorMsg("invalid").toTree)
      *   }
      * }}}
      * */
    def toField[A](
      pipeline: Transformation[String, A]
    ): Validated[ErrorTree, A] =
      pipeline(
        input.valueAtRoot.flatMap(_.headOption.map(_.trim)).getOrElse("")
      )

    /** Extract a string from the root of the Input tree. A more
      * specialised version of [[toField]] that only handles
      * strings. */
    def toStringField(
      pipeline: Rule[String] = {Validated.Valid(_)}
    ): Validated[ErrorTree, String] = toField[String](pipeline)

    /** Extract a string from a child element, then transform it into the desired datatype */
    def subField[A](
      key: String,
      pipeline: Transformation[String, A]
    ): Validated[ErrorTree, A] =
      pipeline(
        input.valueAt(key).flatMap(_.headOption.map(_.trim)).getOrElse("")
      ).leftMap(_.prefixWith(key))

    /** Extract a string from a child element */    
    def stringSubField(
      key: String,
      pipeline: Rule[String] = {Validated.Valid(_)}
    ): Validated[ErrorTree, String] = subField(key, pipeline)


  }

  implicit class RichErrorTree(a: ErrorTree) {
    def valueAtRootList: List[ErrorMsg] = a.valueAtRoot match {
      case None      => Nil
      case Some(nel) => nel.toList
    }
  }

  implicit class RichAppOps[F[_]: Applicative, A](e: F[A]) {

    /** Returns empty unless the predicate given is true, will short
      * circuit if possible.
      * 
      * {{{
      * ask[Salary]("salary") emptyUnless user.isEmployed
      * 
      * Future[Int]{"illegal".toInt} emptyUnless (false)
      * }}}
      */
    def emptyUnless(b: => Boolean)(implicit mon: Monoid[A]): F[A] =
      if(b) e else Monoid[A].empty.pure[F]

    /** Returns empty unless the predicate given is true, will short
      * circuit if possible.
      * 
      * {{{
      * ask[Salary]("salary") emptyUnless ask[Boolean]("employed")
      * 
      * Future[Int]{"illegal".toInt} emptyUnless Future{false}
      * }}}
      */    
    def emptyUnless(eb: F[Boolean])(
      implicit mon: Monoid[A],
      monad: Monad[F]
    ): F[A] = for {
      opt <- eb
      ret <- if (opt) e else mon.empty.pure[F]
    } yield ret

    /** Returns None unless the predicate given is true, will short
      * circuit if possible.
      * 
      * {{{
      * ask[Salary]("salary") when user.isEmployed
      * 
      * Future[Int]{"illegal".toInt} when (false)
      * }}}
      */    
    def when(b: => Boolean): F[Option[A]] =
      if(b) e.map{_.some} else none[A].pure[F]

    /** Returns None unless the predicate given is true, will short
      * circuit if possible.
      * 
      * {{{
      * ask[Salary]("salary") when ask[Boolean]("employed")
      * 
      * Future[Int]{"illegal".toInt} when Future{false}
      * }}}
      */        
    def when(wmb: F[Boolean])(implicit monad: Monad[F]): F[Option[A]] = for {
      opt <- wmb
      ret <- if (opt) e map {_.some} else none[A].pure[F]
    } yield ret

  }

  implicit def monListMap[K,V: Semigroup] = new Monoid[ListMap[K,V]] {
    def empty = ListMap.empty

    def combine(xs: ListMap[K, V], ys: ListMap[K, V]): ListMap[K, V] =
      ys.foldLeft(xs) {
        case (mx, (k, y)) =>
          mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
  }

  def taggedEqInstance[A, Tag](eqBase: Eq[A]) = new Eq[A @@ Tag]{
    def eqv(x: A @@ Tag, y: A @@ Tag): Boolean = eqBase.eqv(x,y)
  }

}
