package ltbs.uniform

import scala.language.implicitConversions

import simulacrum._
import cats.data.{NonEmptyList => NEL}
import collection.immutable.ListMap

/** Can be navigated like a tree. Has a `Value` at branches and
  * leaves, and edges are labelled with `Key`.
  */
trait TreeLike[T] {

  type Key
  type Value

  def listSubtrees(a: T): List[Key]

  def appendWith(a: T, key: Key): T

  /** Add an key to the start of each existing key, effectively
    * creating a new tree with the existing tree as a single
    * subtree */
  def prefixWith(a: T, key: Key): T

  def prefixWithMany(value: T, key: List[Key]): T = {
    @annotation.tailrec
    def inner(x: T, innerKey: List[Key]): T = innerKey match {
      case Nil => x
      case (k::ks) => inner(prefixWith(x, k), ks)
    }
    inner(value, key.reverse)
  }

  /** gives the subtree at a given key */
  def subTree(a: T, key: Key): T
  def subTreeOpt(a: T, key: Key): Option[T]  

  /** gives the subtree at a given key */  
  def /(a: T, key: Key): T = subTree(a,key)
  def /?(a: T, key: Key): Option[T] = subTreeOpt(a,key)  

  def valueAt(a: T, key: Key): Option[Value] =
    valueAtRoot(subTree(a, key))

  def root(a: T): Option[Value]

  /** returns 'true' if there is a subtree at the given key */
  // def definedAt(a: T, key: Key): Boolean =
  //   valueAt(a, key).isDefined

  /** returns 'true' if there is a subtree at the given path */  

  /** returns 'true' if there is a subtree at the given path */    


  /** a null-graph (no verticies/K0) */
  def empty: T

  /** create a tree with a single vertex (a singleton graph) */  
  def one(in: Value): T

  /** gives the subtree at a given path */    

  extension(a: T)
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

trait MapTree[K,V] extends Map[List[K],V]

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

    val empty: Map[List[K],V] = Map.empty
    def one(in: Value): T = Map(List.empty[Key] -> in)
    def valueAtRoot(a: T): Option[Value] = a.get(List.empty[Key])

    def appendWith(a: T, key: Key): T = a.map{ case (k,v) =>
      (k :+ key) -> v
    }

    def prefixWith(a: T, key: Key): T = a.map{ case (k,v) =>
      (key :: k) -> v
    }

given TreeLike[ErrorTree] with
    type Key = String
    type Value = NEL[ErrorMsg]

    def listSubtrees(a: ErrorTree): List[Key] =
      a.keys.toList.flatMap{_.head}.distinct

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

    def subTreeOpt(a: ErrorTree, keyPath: String): Option[ErrorTree] = 
      if (a.definedAt(keyPath)) Some(subTree(a, keyPath)) else None

    val empty: ErrorTree = ListMap.empty

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


