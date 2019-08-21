package ltbs.uniform
package common

import cats.syntax.eq._

package web {
  trait webcommon {

    type Path = List[List[String]]
    type JourneyConfig = String
    type DB = Map[List[String],String]

    object DB {
      def empty: DB = Map()
    }

    def relativePath(from: List[String], to: List[String]): String = {
      import cats.instances.string._
      val (rem, add) = removeCommon(from, to)
        (rem.drop(1).map{_ => ".."} ++ add).mkString("/")
    }

    @annotation.tailrec
    final def removeCommon[B: cats.Eq](
      x: List[B],
      y: List[B]
    ): (List[B], List[B]) = (x,y) match {
      case (x::xs, y::ys) if x === y => removeCommon(xs, ys)
      case a => a
    }

    implicit def formToWebMonad[A, Html: cats.Monoid](
      implicit ff: FormField[A, Html]
    ): WebMonadConstructor[A, Html] = PostAndGetPage(ff)

  }

}

package object web extends webcommon {
  implicit class RichList[A](value: List[A]) {
    def deleteAtIndex(i: Int): List[A] =
      value.take(i) ++ value.drop(i + 1)
    def replaceAtIndex(i: Int, a: A): List[A] =
      value.take(i) ++ {a :: value.drop(i + 1)}
  }
}
