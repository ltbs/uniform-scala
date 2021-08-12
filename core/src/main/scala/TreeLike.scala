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

  def valueAtPath(a: T, key: List[Key]): Option[Value] = {
    valueAtRoot(atPath(a, key))
  }

  def valueAtRoot(a: T): Option[Value]

  /** returns 'true' if there is a subtree at the given key */
  def definedAt(a: T, key: Key): Boolean =
    valueAt(a, key).isDefined

  /** returns 'true' if there is a subtree at the given path */  
  def definedAtPath(a: T, key: List[Key]): Boolean =
    valueAtPath(a, key).isDefined

  /** returns 'true' if there is a subtree at the given path */    
  def definedAtRoot(a: T): Boolean =
    valueAtRoot(a).isDefined

  def isEmpty(a: T): Boolean = a == empty
  def isNonEmpty(a: T): Boolean = !isEmpty(a)

  /** a null-graph (no verticies/K0) */
  def empty: T

  /** create a tree with a single vertex (a singleton graph) */  
  def one(in: Value): T

  /** gives the subtree at a given path */    
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

object TreeLike extends scala.AnyRef {
    @scala.inline() def apply[T](implicit instance: TreeLike[T]): TreeLike[T] {
      type Key = instance.Key;
      type Value = instance.Value
    } = instance;
    abstract trait Ops[T] extends scala.AnyRef {
      type TypeClassType <: TreeLike[T]; 
      val typeClassInstance: TypeClassType;
      import typeClassInstance._;
      def self: T;
      def listSubtrees: List[Key] = typeClassInstance.listSubtrees(self);
      def appendWith(key: Key): T = typeClassInstance.appendWith(self, key);
      def prefixWith(key: Key): T = typeClassInstance.prefixWith(self, key);
      def prefixWithMany(key: List[Key]): T = typeClassInstance.prefixWithMany(self, key);
      def subTree(key: Key): T = typeClassInstance.subTree(self, key);
      def subTreeOpt(key: Key): Option[T] = typeClassInstance.subTreeOpt(self, key);
      def $div(key: Key): T = typeClassInstance.$div(self, key);
      def $div$qmark(key: Key): Option[T] = typeClassInstance.$div$qmark(self, key);
      def valueAt(key: Key): Option[Value] = typeClassInstance.valueAt(self, key);
      def valueAtPath(key: List[Key]): Option[Value] = typeClassInstance.valueAtPath(self, key);
      def valueAtRoot: Option[Value] = typeClassInstance.valueAtRoot(self);
      def definedAt(key: Key): Boolean = typeClassInstance.definedAt(self, key);
      def definedAtPath(key: List[Key]): Boolean = typeClassInstance.definedAtPath(self, key);
      def definedAtRoot: Boolean = typeClassInstance.definedAtRoot(self);
      def isEmpty: Boolean = typeClassInstance.isEmpty(self);
      def isNonEmpty: Boolean = typeClassInstance.isNonEmpty(self);
      def atPath(path: List[Key]): T = typeClassInstance.atPath(self, path)
    };
    abstract trait ToTreeLikeOps extends scala.AnyRef {
      @java.lang.SuppressWarnings(scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")) implicit def toTreeLikeOps[T](target: T)(implicit tc: TreeLike[T]): Ops[T] {
        type TypeClassType = TreeLike[T] {
          type Key = tc.Key;
          type Value = tc.Value
        }
      } = {
        final class $anon extends Ops[T] {
          type TypeClassType = TreeLike[T] {
            type Key = tc.Key;
            type Value = tc.Value
          };
          val self = target;
          val typeClassInstance: TypeClassType = tc
        };
        new $anon()
      }
    };
    object nonInheritedOps extends ToTreeLikeOps {
    };
    abstract trait AllOps[T] extends Ops[T] {
      type TypeClassType <: TreeLike[T];
      val typeClassInstance: TypeClassType
    };
    object ops extends scala.AnyRef {
      @java.lang.SuppressWarnings(scala.Array("org.wartremover.warts.ExplicitImplicitTypes", "org.wartremover.warts.ImplicitConversion")) implicit def toAllTreeLikeOps[T](target: T)(implicit tc: TreeLike[T]): AllOps[T] {
        type TypeClassType = TreeLike[T] {
          type Key = tc.Key;
          type Value = tc.Value
        }
      } = {
        final class $anon extends AllOps[T] {
          type TypeClassType = TreeLike[T] {
            type Key = tc.Key;
            type Value = tc.Value
          };
          val self = target;
          val typeClassInstance: TypeClassType = tc
        };
        new $anon()
      }
    }
  }; 

trait TreeLikeInstances {

  class MapTree[K,V] extends TreeLike[Map[List[K],V]] {
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
  }

  implicit object ErrorTree extends TreeLike[ErrorTree] {

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
    def one(in: NEL[ErrorMsg]): ErrorTree = ListMap (
      NEL.one(Nil) -> in
    )

    def oneErr(in: ErrorMsg): ErrorTree = ListMap (
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
