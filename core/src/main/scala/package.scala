package ltbs

import org.atnos.eff._
import cats.data.Validated
import org.atnos.eff.all.{none => _, _}
import org.atnos.eff.syntax.all._
import cats.implicits._
import cats.Monoid
import scala.language.implicitConversions

package object uniform {

  type _uniformCore[Q]  = cats.data.State[UniformCore,?] |= Q

  type UniformAsk[OUT,A] = Uniform[Unit,OUT,A]
  type UniformTell[IN,A] = Uniform[IN,Unit,A]

  type _uniform[IN, OUT, R] = Uniform[IN,OUT,?] |= R
  type _uniformAsk[OUT, R] = UniformAsk[OUT,?] |= R
  type _uniformTell[IN, R] = UniformTell[IN,?] |= R

  type Encoded = String
  type ErrorTree = Tree[String,String]  

  type _uniformList[STACK,SUB] = UniformAskList[SUB,?] |= STACK
  type _uniformSelect[STACK,SUB] = UniformSelect[SUB,?] |= STACK

  type DB = Map[List[String],Encoded]

  implicit def uniformToStack[IN,OUT,R :_uniform[IN, OUT, ?]](inner: Uniform[IN,OUT,R]): Eff[R,OUT] =
    inner match {
      case Uniform(key,tell,default,validation) =>
            send[Uniform[IN,OUT,?], R, OUT](Uniform(key,tell,default,validation))
    }

  implicit def uniformBToStack[IN,OUT,R :_uniform[IN, OUT, ?] : _uniformCore](
    inner: UniformB[IN,OUT]
  ): Eff[R,OUT] =
    inner match {
      case UniformB(key,tell,default,validation) => for { 
        predKeys <- core.map{_.path}
        x <- send[Uniform[IN,OUT,?], R, OUT](Uniform(predKeys :+ key,tell,default,validation))
      } yield (x)
    }

  def core[STACK : _uniformCore] = get[STACK,UniformCore]
  def coreMod[STACK : _uniformCore](f: UniformCore => UniformCore) =
    get[STACK,UniformCore] >>= {x => put[STACK,UniformCore](f(x))}

  def breadcrumbs[STACK : _uniformCore] = get[STACK,UniformCore].map{_.breadcrumbs}
  def path[STACK : _uniformCore] = get[STACK,UniformCore].map{_.path}
  def pathPush[STACK : _uniformCore](key: String) = coreMod{ old =>
    old.copy(path = old.path :+ key)
  }

  def pathPop[STACK : _uniformCore] = coreMod{ old =>
    old.copy(path = old.path.init)
  }  

  def crumbPush[STACK : _uniformCore](crumb: List[String]) = coreMod{ old =>
    old.copy(breadcrumbs = old.breadcrumbs :+ crumb)
  }

  object db {

    def remove[STACK : _uniformCore](
      key: List[String]
    ): Eff[STACK,Unit] = encoded.remove(key)

    def removeRecursive[STACK : _uniformCore](
      key: List[String]
    ): Eff[STACK,Unit] = encoded.removeRecursive(key)

    object encoded {
      def get[STACK : _uniformCore](key: List[String]): Eff[STACK,Option[String]] = core.map(_.state.get(key))
      def update[STACK : _uniformCore](key: List[String], newval: String): Eff[STACK,Unit] = coreMod{ old =>
        old.copy(state = old.state + (key -> newval))
      }

      def remove[STACK : _uniformCore](key: List[String]): Eff[STACK,Unit] = coreMod{ old =>
        old.copy(state = old.state - key)
      }

      def removeRecursive[STACK : _uniformCore](key: List[String]): Eff[STACK,Unit] = coreMod{ old =>
        old.copy(state = old.state.filterNot(_._1.startsWith(key)))
      }
      
    }
  }

  def subjourney[A,STACK : _uniformCore](
    path: String
  )(
    inner: Eff[STACK,A]
  ): Eff[STACK,A] = for {
    a <- pathPush(path) >> inner
    _ <- pathPop
  } yield (a)

  def uniformP[IN,OUT,R :_uniform[IN, OUT, ?]](
    key: List[String],
    tell: IN,
    default: Option[OUT] = None,
    validation: OUT => Validated[String,OUT] = {v:OUT => v.valid}
  ): Eff[R, OUT] =
    send[Uniform[IN,OUT,?], R, OUT](Uniform(key,tell,default,validation))

  def uniform[IN,OUT,R :_uniform[IN, OUT, ?]](
    key: String,
    tell: IN,
    default: Option[OUT] = None,
    validation: OUT => Validated[String,OUT] = {v:OUT => v.valid}
  ): Eff[R, OUT] =
    uniformP(List(key),tell,default,validation)

  def uaskP[OUT,R :_uniformAsk[OUT, ?]](
    key: List[String],
    default: Option[OUT] = None,
    validation: OUT => Validated[String,OUT] = {v:OUT => v.valid}
  ): Eff[R, OUT] =
    send[UniformAsk[OUT,?], R, OUT](Uniform(key,(),default,validation))

  def ask[OUT](key: String) =
    UniformB[Unit,OUT](key, (), None, {v:OUT => v.valid})

  def tell[IN](key: String)(value: IN) =
    UniformB[IN,Unit](key, value, None, {v:Unit => v.valid})

  def dialogue[IN,OUT](key: String)(value: IN) =
    UniformB[IN,OUT](key, value, None, {v:OUT => v.valid})

  def end[IN](key: String)(value: IN) =
    UniformB[IN,Unit](key, value, None, {_:Unit => "journey.end".invalid})

  def uask[OUT,R :_uniformAsk[OUT, ?]](
    key: String,
    default: Option[OUT] = None,
    validation: OUT => Validated[String,OUT] = {v:OUT => v.valid}
  ): Eff[R, OUT] =
    send[UniformAsk[OUT,?], R, OUT](Uniform(List(key),(),default,validation))

  def utell[IN,R :_uniformTell[IN, ?]](
    key: String,
    tell: IN
  ): Eff[R, Unit] =
    send[UniformTell[IN,?], R, Unit](Uniform(List(key),tell, None, {v:Unit => v.valid}))

  def uaskList[R, T](
    key: String,
    min: Int = 0,
    max: Int = Int.MaxValue
  )(implicit member: UniformAskList[T, ?] |= R): Eff[R, List[T]] =
    send[UniformAskList[T, ?], R, List[T]](
      UniformAskList(List(key), min, max)
    )

  def uaskOneOf[R, T](key: String, options: Set[T], validation: T => Validated[String,T] = {v:T => v.valid})(implicit member: UniformSelect[T, ?] |= R): Eff[R, T] =
    send[UniformSelect[T, ?], R, T](UniformSelectOne(List(key), options, validation))

  def uaskNOf[R, T](
    key: List[String],
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
