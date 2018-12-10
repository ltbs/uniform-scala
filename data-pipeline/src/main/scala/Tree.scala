package ltbs.uniform.datapipeline

import cats.implicits._
import cats.Monoid

case class Tree[K,V](
  value: V,
  children: Map[K,Tree[K,V]] = Map.empty[K,Tree[K,V]]
) {
  def get(key: K): Either[Error,Tree[K,V]] = children.get(key) match {
    case Some(x) => x.asRight[Error]
    case None    => Tree("", Map(key.toString -> Tree[String,String]("required"))).asLeft[Tree[K,V]]
  }

  def add(key: K, newValue: Tree[K,V]): Tree[K,V] = Tree(value, children + (key -> newValue))

  def forestAtPath(path: K*): Option[Tree[K,V]] =
    path.foldLeft(this.some){
      case (tree, p) => tree.flatMap(_.children.get(p))
    }

  def atPath(path: K*): Option[V] = forestAtPath(path:_*).map(_.value)

  def definedAtPath(path: K*)(implicit monoid: Monoid[V]): Boolean =
    forestAtPath(path:_*) match {
      case Some(Tree(e,m)) if e != monoid.empty || m.nonEmpty => true
      case _ => false
    }

  def flatTree(implicit monoid: Monoid[V]): List[(List[K],V)] = {
    def inner(path: List[K], subForest: Tree[K,V]): List[(List[K],V)] = {
      val chi = subForest.children.toList.flatMap {
        case (k, v) => inner(k :: path, v)
      }
      subForest.value match {
        case e if e == monoid.empty => chi
        case v => (path, v) :: chi
      }
    }
    inner(Nil, this)
  }

  def isEmpty(implicit monoid: Monoid[V]): Boolean =
    this == Tree(monoid.empty)

  def nonEmpty(implicit monoid: Monoid[V]): Boolean = !isEmpty
}

object Tree {

  def empty[K,V](implicit monoid: Monoid[V]): Tree[K,V] =
    Tree(monoid.empty)
  
}
