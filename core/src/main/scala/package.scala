package ltbs

import cats.implicits._
import cats.{Monoid, Applicative, Monad}
import cats.data.{NonEmptyList, Validated}
import shapeless.tag.{@@}
import uniform.Quantity.ToQuantityOps
import collection.immutable.ListMap

package object uniform extends TreeLike.ToTreeLikeOps
    with TreeLikeInstances
    with ScalaVersionCompatibility
    with ToQuantityOps
    with QuantityInstances
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
    def toUrlEncodedString: String = {
      input
        .flatMap { case (k, vs) =>
          vs.map { v =>
            s"""${k.mkString(".")}=${java.net.URLEncoder.encode(v, "UTF-8")}"""
          }
        }
        .mkString("&")
    }

    def toField[A](
      pipeline: String => Validated[ErrorTree, A]
    ): Validated[ErrorTree, A] =
      pipeline(
        input.valueAtRoot.flatMap(_.headOption.map(_.trim)).getOrElse("")
      )

    def toStringField(
      pipeline: String => Validated[ErrorTree, String] = {Validated.Valid(_)}
    ): Validated[ErrorTree, String] = toField[String](pipeline)

    def subField[A](
      key: String,
      pipeline: String => Validated[ErrorTree, A]
    ): Validated[ErrorTree, A] =
      pipeline(
        input.valueAt(key).flatMap(_.headOption.map(_.trim)).getOrElse("")
      ).leftMap(_.prefixWith(key))

    def stringSubField(
      key: String,
      pipeline: String => Validated[ErrorTree, String] = {Validated.Valid(_)}
    ): Validated[ErrorTree, String] = subField(key, pipeline)


  }

  implicit class RichErrorTree(a: ErrorTree) {

    def valueAtRootList: List[ErrorMsg] =
      a.valueAtRoot.fold(List.empty[ErrorMsg])(_.toList)

    def errorsAtRoot = valueAtRootList

    def errorsAt(key: String): List[ErrorMsg] =
      a.valueAt(key).fold(List.empty[ErrorMsg])(_.toList)

    def errorsAtPath(key: List[String]): List[ErrorMsg] =
      a.valueAtPath(key).fold(List.empty[ErrorMsg])(_.toList)

  }

  implicit class RichAppOps[F[_]: Applicative, A](e: F[A]) {
    def emptyUnless(b: => Boolean)(implicit mon: Monoid[A]): F[A] =
      if(b) e else Monoid[A].empty.pure[F]

    def emptyUnless(eb: F[Boolean])(
      implicit mon: Monoid[A],
      monad: Monad[F]
    ): F[A] = for {
      opt <- eb
      ret <- if (opt) e else mon.empty.pure[F]
    } yield ret

    def when(b: => Boolean): F[Option[A]] =
      if(b) e.map{_.some} else none[A].pure[F]

    def when(wmb: F[Boolean])(implicit monad: Monad[F]): F[Option[A]] = for {
      opt <- wmb
      ret <- if (opt) e map {_.some} else none[A].pure[F]
    } yield ret

  }

  implicit class RichRuleListList[A](inner: List[List[Rule[A]]]) {
    def combined: Rule[A] = {
      inner match {
        case Nil => Rule.noop
        case x   => x.map{_.combineAll}.reduce(_ andThen _)
      }
    }
  }

  implicit def soloRuleToListList[A](in: Rule[A]): List[List[Rule[A]]] = in.pure[List].pure[List]
  implicit def listOfRulesToListList[A](in: List[Rule[A]]): List[List[Rule[A]]] = in.pure[List]

  implicit def monListMap[K,V] = new Monoid[ListMap[K,V]] {
    def empty = ListMap.empty
    def combine(x: ListMap[K,V], y: ListMap[K,V]): ListMap[K,V] = x ++ y
  }

}
