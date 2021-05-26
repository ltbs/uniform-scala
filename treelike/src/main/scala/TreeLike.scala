package ltbs.treelike

import scala.language.implicitConversions

import cats.data.{NonEmptyList => NEL}
import collection.immutable.ListMap

/** Can be navigated like a tree. Has a `Value` at branches and
  * leaves, and edges are labelled with `Key`.
  */
trait TreeLike[T] {

  type Key
  type Value

  def listSubtrees(a: T): List[Key]
  def root(a: T): Option[Value]

  /** a null-graph (no verticies/K0) */
  def empty: T

  /** create a tree with a single vertex (a singleton graph) */  
  def one(in: Value): T

  extension(a: T)
    def keys: List[Key] = listSubtrees(a)

    def definedAt(key: Key): Boolean =
      valueAt(a, key).isDefined

    def atPath(path: List[Key]): T = {
      @annotation.tailrec
      def inner(a1: T, path1: List[Key]): T = {
        path1 match {
          case Nil => a1
          case (x::xs) => inner(subTree(a1,x), xs)
        }
      }
      inner(a, path)
    }

    def valueAtPath(key: List[Key]): Option[Value] = {
      atPath(key).valueAtRoot
    }

    def definedAtPath(key: List[Key]): Boolean =
      valueAtPath(key).isDefined

    def definedAtRoot: Boolean =
      valueAtRoot.isDefined

    def isEmpty: Boolean = a == empty
    def isNonEmpty: Boolean = !isEmpty

    def valueAtRoot: Option[Value] = root(a)

}

object TreeLike:
    def apply[T](using tl: TreeLike[T]): TreeLike[T] = tl

type MapTree[K,V] = Map[List[K],V]

given [K,V]: TreeLike[MapTree[K,V]] with
    type Key = K
    type Value = V
    type T = Map[List[K],V]

    def listSubtrees(a: T): List[Key] = a.keys.collect{
      case (h::_) => h
    }.toList.distinct

    def subTree(a: T, key: Key): T =
      a.collect { case (`key`::rem, v) =>
        (rem, v)
      }

    def subTreeOpt(a: T, key: Key): Option[T] =
      Some(subTree(a, key)).filter(_.nonEmpty)

    val empty: Map[List[K],V] = Map.empty[List[K], V]
    def one(in: Value): T = Map(List.empty[Key] -> in)
    def root(a: T): Option[Value] = a.get(List.empty[Key])

    def appendWith(a: T, key: Key): T = a.map{ case (k,v) =>
      (k :+ key) -> v
    }

    def prefixWith(a: T, key: Key): T = a.map{ case (k,v) =>
      (key :: k) -> v
    }



