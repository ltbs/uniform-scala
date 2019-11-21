package ltbs.uniform
package common

import cats.syntax.eq._

/** Contains common abstractions used for web based
  * interpreters. Responsible for serialisation and deserialisation,
  * and interfaces for rendering of forms.
  * 
  * This package is also concerned with inductive inference (shapeless
  * typeclass derivation) of codecs and renderers for Product and
  * CoProduct datatypes. 
  */
package web {
  trait webcommon {

    type Breadcrumbs = List[List[String]]
    type DB = Map[List[String],String]

    object DB {
      def empty: DB = Map()
    }

    /** Returns a relative path as used in a URI or *nix directory -
      * with '..' used to denote navigating up one element in the tree. 
      * 
      * {{{
      * scala> relativePath(List("1","2"),{1 to 5}.map{_.toString}.toList)
      * res0: String = 3/4/5
      * 
      * scala> relativePath(List("1","other","thing"),{1 to 5}.map{_.toString}.toList)
      * res1: String = ../2/3/4/5
      * }}}
      */
    def relativePath(from: List[String], to: List[String]): String = {
      import cats.instances.string._
      val frags = removeCommon(from, to) match {
        case (Nil, Nil) => "." :: Nil
        case (back, Nil) => back.map(_ => "..")
        case (Nil, up) => (from.takeRight(1) ++ up)
        case (back, up) => (back.drop(1).map(_ => "..") ++ up)
      }
      frags.mkString("/")
    }

    /** Returns the lists given as arguments with any sequence
      * common to the two removed from the start 
      * 
      * {{{
      * scala> removeCommon({1 to 5}.toList, List(1,2))
      * res0: (List[Int], List[Int]) = (List(3, 4, 5),List())
      * }}}
      */
    @annotation.tailrec
    final def removeCommon[B: cats.Eq](
      x: List[B],
      y: List[B]
    ): (List[B], List[B]) = (x,y) match {
      case (x::xs, y::ys) if x === y => removeCommon(xs, ys)
      case a => a
    }

    implicit def formToWebMonad[A, Html](
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
