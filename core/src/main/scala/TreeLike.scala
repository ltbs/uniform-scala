package ltbs.uniform

import simulacrum._
import scala.language.implicitConversions
import cats.data.{NonEmptyList => NEL}

@typeclass trait TreeLike[T] {

  type Key
  type Value

  def appendWith(a: T, key: Key): T
  def prefixWith(a: T, key: Key): T  

  def subTree(a: T, key: Key): T
  def /(a: T, key: Key): T = subTree(a,key)

  def valueAt(a: T, key: Key): Option[Value] =
    valueAtRoot(subTree(a, key))

  def valueAtPath(a: T, key: List[Key]): Option[Value] = {
    valueAtRoot(atPath(a, key))
  }

  def valueAtRoot(a: T): Option[Value]

  def definedAt(a: T, key: Key): Boolean =
    valueAt(a, key).isDefined

  def definedAtPath(a: T, key: List[Key]): Boolean =
    valueAtPath(a, key).isDefined

  def definedAtRoot(a: T): Boolean =
    valueAtRoot(a).isDefined

  def isEmpty(a: T): Boolean = a == empty
  def isNonEmpty(a: T): Boolean = !isEmpty(a)

  def empty: T

  def one(in: Value): T

  def atPath(a: T, path: List[Key]): T = {

    @annotation.tailrec
    def inner(a1: T, path1: List[Key]): T = {
      path1 match {
        case Nil => a1
        case (x::xs) => inner(subTree(a1,x), xs)
      }
    }
    inner(a, path)
  }

}

trait TreeLikeInstances {

  class MapTree[K,V] extends TreeLike[Map[List[K],V]] {
    type Key = K
    type Value = V
    type T = Map[List[K],V]

    def subTree(a: T, key: Key): T =
      a.collect { case (`key`::rem, v) =>
        (rem, v)
      }

    val empty: Map[List[K],V] = Map.empty
    def one(in: Value): T = Map(List.empty[Key] -> in)
    def valueAtRoot(a: T): Option[Value] = a.get(List.empty[Key])

    def appendWith(a: T, key: Key): T = a.map{ case (k,v) =>
      (k :+ key) -> v
    }

    def prefixWith(a: T, key: Key): T = a.map{ case (k,v) =>
      (key :: k) -> v
    }
    
  }

  implicit object ErrorTree extends TreeLike[ErrorTree] {

    type Key = String
    type Value = NEL[ErrorMsg]

    def subTree(a: ErrorTree, keyPath: String): ErrorTree = {
      a.flatMap { case (allPaths, errs) =>
        val refinedPaths = allPaths.collect {
          case (`keyPath`::rem) => rem
        }
        NEL.fromList(refinedPaths) match {
          case None => Nil
          case Some(p) => List(p -> errs)
        }
      }
    }

    val empty: ErrorTree = Map.empty
    def one(in: NEL[ErrorMsg]): ErrorTree = Map (
      NEL.one(Nil) -> in
    )

    def oneErr(in: ErrorMsg): ErrorTree = Map (
      NEL.one(Nil) -> NEL.one(in)
    )

    def valueAtRoot(a: ErrorTree): Option[NEL[ErrorMsg]] = a.get(NEL.one(Nil))

    def simplified(a: ErrorTree): Map[InputPath, ErrorMsg] = a flatMap {
      case (paths, errors) =>
        errors.toList.map{ error =>
          (paths.head, error)
        }
    }

    def appendWith(a: ErrorTree, key: String): ErrorTree = 
      a.map{ case (k,v) =>
        (k.map{_ :+ key}) -> v
      }

    def prefixWith(a: ErrorTree, key: String): ErrorTree = 
      a.map{ case (k,v) =>
        (k.map{key :: _}) -> v
      }

  }
}
