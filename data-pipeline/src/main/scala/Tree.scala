package ltbs.uniform.datapipeline

import cats.implicits._
import cats.data.Validated
import cats.{Invariant, Monoid}

case class Tree[K,V](
  value: V,
  children: Map[K,Tree[K,V]] = Map.empty[K,Tree[K,V]]
) {
  def get(key: K): Either[Error,Tree[K,V]] = children.get(key) match {
    case Some(x) => x.asRight[Error]
    case None    => Tree("", Map(key.toString -> Tree[String,String]("required"))).asLeft[Tree[K,V]]
  }

  def add(key: K, newValue: Tree[K,V]): Tree[K,V] = Tree(value, children + (key -> newValue))

  def atPath(path: K*): Option[V] =
    path.foldLeft(this.some){
      case (tree, p) => tree.flatMap(_.children.get(p))
    }.map(_.value)

  def flatTree(implicit monoid: Monoid[V]): List[(List[K],V)] = {
    def inner(path: List[K], subForest: Tree[K,V]): List[(List[K],V)] = {
      val chi = subForest.children.toList.flatMap{
        case (k, v) => inner(k :: path, v)
      }
      subForest.value match {
        case e if e == monoid.empty => chi
        case v => (path, v) :: chi
      }
    }
    inner(Nil, this)
  }

}

object Tree {

  def empty[K,V](implicit monoid: Monoid[V]): Tree[K,V] =
    Tree(monoid.empty)
}
