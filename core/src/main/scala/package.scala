package ltbs

import org.atnos.eff._
import cats.data.Validated
import org.atnos.eff.all.{none => _, _}
import cats.implicits._
import cats.Monoid

package object uniform {

  type Encoded = String
  type ErrorTree = Tree[String,String]  

  type _uniform[V,R] = UniformAsk[V,?] |= R
  type _uniformList[V,R] = UniformAskList[V,?] |= R  
  type _uniformSelect[V,R] = UniformSelect[V,?] |= R

  def uask[R, T](key: String, validation: T => Validated[String,T] = {v:T => v.valid})(implicit member: UniformAsk[T, ?] |= R): Eff[R, T] =
    send[UniformAsk[T, ?], R, T](UniformAsk(key, validation))

  def uaskList[R, T](
    key: String,
    min: Int = 0,
    max: Int = Int.MaxValue
  )(implicit member: UniformAskList[T, ?] |= R): Eff[R, List[T]] =
    send[UniformAskList[T, ?], R, List[T]](
      UniformAskList(key, min, max)
    )

  def uaskOneOf[R, T](key: String, options: Set[T], validation: T => Validated[String,T] = {v:T => v.valid})(implicit member: UniformSelect[T, ?] |= R): Eff[R, T] =
    send[UniformSelect[T, ?], R, T](UniformSelectOne(key, options, validation))

  def uaskNOf[R, T](
    key: String,
    options: Set[T],
    min: Int = 0,
    max: Int = Int.MaxValue,
    validation: Set[T] => Validated[String,Set[T]] = {v:Set[T] => v.valid}
  )(implicit member: UniformSelect[T, ?] |= R): Eff[R, Set[T]] =
    send[UniformSelect[T, ?], R, Set[T]](
      UniformSelectMany(key, options, min, Math.min(max, options.size), validation)
    )

  implicit class RichMonoidOps[R, A](e: Eff[R, A])(implicit monoid: Monoid[A]) {
    
    def emptyUnless(b: => Boolean): Eff[R, A] =
      if(b) e else Eff.pure[R,A](monoid.empty)

    def emptyUnless(eb: Eff[R,Boolean]): Eff[R,A] = for {
      opt <- eb
      ret <- if (opt) e else Eff.pure[R,A](monoid.empty)
    } yield ret

  }

  implicit class RichOps[R, A](wm: Eff[R, A]) {
    def when(b: => Boolean): Eff[R,Option[A]] =
      if(b) wm.map{_.some} else Eff.pure[R,Option[A]](none[A])

    def when(wmb: Eff[R,Boolean]): Eff[R,Option[A]] = for {
      opt <- wmb
      ret <- if (opt) wm map {_.some} else Eff.pure[R,Option[A]](none[A])
    } yield ret
  }

  def when[R, A](b: => Boolean)(wm: Eff[R, A]): Eff[R,Option[A]] =
    if(b) wm.map{_.some} else Eff.pure[R,Option[A]](none[A])

  implicit def treeMonoid[K, V: Monoid] = new Monoid[Tree[K,V]] {
    def empty: Tree[K,V] = Tree(Monoid[V].empty)

    def combine(x: Tree[K,V], y: Tree[K,V]): Tree[K,V] = {
      val value = x.value |+| y.value

      // crude 'unionwith'
      val xkeys = x.children.keys.toList
      val ykeys = y.children.keys.toList
      val shared = xkeys.intersect(ykeys)
      val xonly = xkeys.map{v => v -> x.children(v)}
      val yonly = ykeys.map{v => v -> y.children(v)}
      val merged = shared.map{v => v -> combine(x.children(v), y.children(v))}

      Tree(value, Map({xonly ++ yonly ++ merged}:_*))
    }
  }
}
