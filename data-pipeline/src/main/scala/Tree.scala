package ltbs.uniform.datapipeline

import cats.implicits._
import cats.data.Validated
import cats.Invariant

case class Tree[K,V](
  value: V,
  children: Map[K,Tree[K,V]] = Map.empty[K,Tree[K,V]]
) {
  def get(key: K): Either[Error,Tree[K,V]] = children.get(key) match {
    case Some(x) => x.asRight[Error]
    case None    => Tree("", Map(key.toString -> Tree[String,String]("required"))).asLeft[Tree[K,V]]
  }

  def add(key: K, newValue: Tree[K,V]): Tree[K,V] = Tree(value, children + (key -> newValue))
}
