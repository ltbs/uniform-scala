package ltbs.uniform

import scala.language.higherKinds
import izumi.reflect.{Tag, TagK}
import validation.Rule
import scala.collection.immutable.{Map => IMap}

trait Uniform[-R <: Needs[_], +A, -T] {
  def map[F[_], B](f: A => B): Uniform[R, B, T] = Uniform.Map(this, f)
  def flatMap[R1 <: R, B, T1 <: T](f: A => Uniform[R1, B, T1]): Uniform[R1, B, T1] = Uniform.FlatMap(this, f)

  /** Returns None unless the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") when user.isEmployed
    * }}}
    */
  def when(predicate: => Boolean): Uniform[R, Option[A], T] = this.map{ v =>
    if (predicate) Some(v) else None
  }

  /** Returns None unless the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") when ask[Boolean]("employed")
    * }}}
    */
  def when[R1 <: R, T1 <: T](wmb: Uniform[R1, Boolean, T1]): Uniform[R1, Option[A], T1] = for {
    opt <- wmb
    ret <- if (opt) this map (x => Some(x)) else Uniform.Pure(None: Option[A])
  } yield ret

  /** Returns None when the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") unless user.isExempt
    * }}}
    */
  def unless(predicate: => Boolean): Uniform[R, Option[A], T] = when(!predicate)

  /** Returns None when the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") unless ask[Boolean]("is-exempt")
    * }}}
    */
  def unless[R1 <: R, T1 <: T](wmb: Uniform[R1, Boolean, T1]): Uniform[R1, Option[A], T1] =
    when[R1, T1](wmb.map(x => !x))

  /** Returns monoid empty unless the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") emptyUnless user.isEmployed
    * }}}
    */
  def emptyUnless[B >: A](predicate: => Boolean)(implicit mon: cats.Monoid[B]): Uniform[R, B, T] = this.map{ v =>
    if (predicate) v else mon.empty
  }

  /** Returns monoid empty unless the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") emptyUnless ask[Boolean]("employed")
    * }}}
    */
  def emptyUnless[B >: A, R1 <: R, T1 <: T](wmb: Uniform[R1, Boolean, T1])(implicit mon: cats.Monoid[B]): Uniform[R1, B, T1] = for {
    opt <- wmb
    ret <- if (opt) this else Uniform.Pure(mon.empty)
  } yield ret

  /** Returns monoid empty when the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") emptyWhen user.isExempt
    * }}}
    */
  def emptyWhen[B >: A](predicate: => Boolean)(implicit mon: cats.Monoid[B]): Uniform[R, B, T] =
    emptyUnless[B](!predicate)

  /** Returns monoid empty when the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") emptyWhen ask[Boolean]("is-exempt")
    * }}}
    */
  def emptyWhen[B >: A, R1 <: R, T1 <: T](wmb: Uniform[R1, Boolean, T1])(implicit mon: cats.Monoid[B]): Uniform[R1, B, T1] = emptyUnless[B, R1, T1](wmb map (x => !x))

}

object Uniform {
  case class Map[-R <: Needs[_], A, +B, T](base: Uniform[R, A, T], f: A => B) extends Uniform[R, B, T]

  case class FlatMap[R <: Needs[_], -R2 <: R, A, +B, T](base: Uniform[R, A, T], f: A => Uniform[R2, B, T]) extends Uniform[R2, B, T]
  case class Interact[A, T](
    key: String,
    value: T,
    default: Option[A],
    validation: Rule[A],
    customContent: IMap[String,(String,List[Any])],
    askTag: Tag[A],
    tellTag: Tag[T]
  ) extends Uniform[Needs.Ask[A] with Needs.Tell[T], A, T] {
    def customContent(from: String, to: String, args: Any*): Interact[A, T] = this.copy (
      customContent = customContent + ((from,(to, args.toList)))
    )
  }

  case class Tell[A](
    key: String,
    value: A,
    customContent: IMap[String,(String,List[Any])],
    tag: Tag[A]
  ) extends Uniform[Needs.Tell[A], Unit, A] {
    def customContent(from: String, to: String, args: Any*): Tell[A] = this.copy (
      customContent = customContent + ((from,(to, args.toList)))
    )
  }

  case class Ask[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    customContent: IMap[String,(String,List[Any])],
    tag: Tag[A]
  ) extends Uniform[Needs.Ask[A], A, Unit] {
    def customContent(from: String, to: String, args: Any*): Ask[A] = this.copy (
      customContent = customContent + ((from,(to, args.toList)))
    )
  }

  case class EndTell[A](
    key: String,
    value: A,
    customContent: IMap[String,(String,List[Any])],
    tag: Tag[A]
  ) extends Uniform[Needs.Tell[A], Nothing, A] {
    def customContent(from: String, to: String, args: Any*): EndTell[A] = this.copy (
      customContent = customContent + ((from,(to, args.toList)))
    )
  }

  case class End[A](
    key: String,
    customContent: IMap[String,(String,List[Any])]
  ) extends Uniform[Needs[Any], Nothing, Unit] {
    def customContent(from: String, to: String, args: Any*): End[A] = this.copy (
      customContent = customContent + ((from,(to, args.toList)))
    )
  }

  case class Pure[A](value: A) extends Uniform[Needs[_], A, Any]

  case class Subjourney[-R <: Needs[_], A, T](
    path: List[String],
    base: Uniform[R, A, T]
  ) extends Uniform[R, A, T]

  case class ListOf[-R <: Needs[_], A](
    key: String,
    base: (Option[Int], List[A]) => Uniform[R, A, Unit],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: IMap[String,(String,List[Any])],
    tag: Tag[A]
  ) extends Uniform[R with Needs.AskList[A], List[A], Any]

  case class Convert[F[_], A](
    action: F[A],
    tag: TagK[F]
  ) extends Uniform[Needs.Convert[F[_]], A, Unit]

  implicit def uniformMonadInstance[R <: Needs[_], T]: cats.Monad[Uniform[R, ?, T]] =
    new cats.StackSafeMonad[Uniform[R, ?, T]] {
      def pure[A](x: A): Uniform[R,A,T] = Uniform.Pure(x)
      def flatMap[A, B](fa: Uniform[R,A,T])(f: A => Uniform[R,B,T]): Uniform[R,B,T] =
        Uniform.FlatMap(fa, f)
    }

}
