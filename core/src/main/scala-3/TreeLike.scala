trait TreeLike[T] {

  type Key
  type Value

  /** a null-graph (no verticies/K0) */
  def empty: T

  /** create a tree with a single vertex (a singleton graph) */  
  def one(in: Value): T

  extension (a: T) {
    def listSubtrees: List[Key]

    def prefixWith(key: Key): T

    def prefixWithMany(key: List[Key]): T = {
      @annotation.tailrec
      def inner(x: T, innerKey: List[Key]): T = innerKey match {
        case Nil => x
        case (k::ks) => inner(x.prefixWith(k), ks)
      }
      inner(a, key.reverse)
    }

    /** gives the subtree at a given key */
    def subTree(key: Key): T
    def subTreeOpt(key: Key): Option[T]

    /** gives the subtree at a given key */
    def /(key: Key): T = a.subTree(key)
    def /?(key: Key): Option[T] = a.subTreeOpt(key)

    def valueAt(key: Key): Option[Value] =
      a.subTree(key).valueAtRoot

    def valueAtPath(key: List[Key]): Option[Value] = {
      a.atPath(key).valueAtRoot
    }

    def valueAtRoot: Option[Value]

    /** returns 'true' if there is a subtree at the given key */
    def definedAt(key: Key): Boolean =
      a.valueAt(key).isDefined

    /** returns 'true' if there is a subtree at the given path */
    def definedAtPath(key: List[Key]): Boolean =
      a.valueAtPath(key).isDefined

    /** returns 'true' if there is a subtree at the given path */
    def definedAtRoot: Boolean =
      a.valueAtRoot.isDefined

    def isEmpty: Boolean = a == empty
    def isNonEmpty: Boolean = !a.isEmpty


    /** gives the subtree at a given path */
    def atPath(path: List[Key]): T = {

      @annotation.tailrec
      def inner(a1: T, path1: List[Key]): T = {
        path1 match {
          case Nil => a1
          case (x::xs) => inner(a1.subTree(x), xs)
        }
      }
      inner(a, path)
    }
  }
}

given mapTreeLike[K,V]: TreeLike[Map[List[K], V]] with {

    type Key = K
    type Value = V

    def empty: Map[List[Key], V] = Map.empty
    def one(in: Value): Map[List[Key], V] = Map(Nil -> in)

    extension (a: Map[List[Key], V]) {

      def listSubtrees: List[Key] = a.keys.collect{
        case (h::_) => h
      }.toList.distinct

      def prefixWith(key: Key): Map[List[Key],V] = a.map{ case (k,v) =>
        (key :: k) -> v
      }
      def subTree(key: Key): Map[List[Key],V] =
      a.collect { case (`key`::rem, v) =>
        (rem, v)
      }

      def subTreeOpt(key: Key): Option[Map[List[Key],V]] =
        Some(a.subTree(key)).filter(_.nonEmpty)
      def valueAtRoot: Option[Value] = a.get(List.empty[Key])
    }
  }

