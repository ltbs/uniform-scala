package ltbs

import language.{higherKinds, implicitConversions}

import cats.implicits._
import cats.{Monoid, Applicative, Monad}
import cats.data.NonEmptyList
import shapeless.tag.{@@}
import uniform.Quantity.ToQuantityOps
package object uniform extends TreeLike.ToTreeLikeOps
    with TreeLikeInstances
    with ScalaVersionCompatibility
    with OptTC
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
  type ErrorTree = Map[NonEmptyList[InputPath], NonEmptyList[ErrorMsg]]
  type ::[H,T <: shapeless.HList] = shapeless.::[H,T]
  type NilTypes = Unit :: shapeless.HNil

  implicit object Input extends MapTree[String, List[String]] {
    def fromUrlEncodedString(in: String): Either[ErrorTree,Input] = {
      val ungrouped: List[(String, String)] =
        in.split("&").toList
          .map{_.split("=").toList}
          .collect { case (k::v::Nil) ⇒ k →
            java.net.URLDecoder.decode(v, "UTF-8")
          }

      ungrouped.groupBy(_._1).map{ case (k, comb) ⇒
        k.split("[.]").toList.dropWhile(_.isEmpty) → comb.map {_._2}
      }.asRight
    }
  }

  implicit class RichInput(input: Input) {
    def toUrlEncodedString: String = {
      input
        .flatMap { case (k, vs) ⇒
          vs.map { v ⇒
            s"""${k.mkString(".")}=${java.net.URLEncoder.encode(v, "UTF-8")}"""
          }
        }
        .mkString("&")
    }
  }

  implicit class RichErrorTree(a: ErrorTree) {
    def valueAtRootList: List[ErrorMsg] = a.valueAtRoot match {
      case None      ⇒ Nil
      case Some(nel) ⇒ nel.toList
    }
  }

  implicit class RichAppOps[F[_]: Applicative, A](e: F[A]) {
    def emptyUnless(b: ⇒ Boolean)(implicit mon: Monoid[A]): F[A] =
      if(b) e else Monoid[A].empty.pure[F]

    def emptyUnless(eb: F[Boolean])(
      implicit mon: Monoid[A],
      monad: Monad[F]
    ): F[A] = for {
      opt ← eb
      ret ← if (opt) e else mon.empty.pure[F]
    } yield ret

    def when(b: ⇒ Boolean): F[Option[A]] =
      if(b) e.map{_.some} else none[A].pure[F]

    def when(wmb: F[Boolean])(implicit monad: Monad[F]): F[Option[A]] = for {
      opt ← wmb
      ret ← if (opt) e map {_.some} else none[A].pure[F]
    } yield ret

  }

  implicit class RichRuleListList[A](inner: List[List[Rule[A]]]) {
    def combined: Rule[A] = {
      inner match {
        case Nil ⇒ Rule.noop
        case x   ⇒ x.map{_.combineAll}.reduce(_ andThen _)
      }
    }
  }

  implicit def soloRuleToListList[A](in: Rule[A]): List[List[Rule[A]]] = in.pure[List].pure[List]
  implicit def listOfRulesToListList[A](in: List[Rule[A]]): List[List[Rule[A]]] = in.pure[List]

}
