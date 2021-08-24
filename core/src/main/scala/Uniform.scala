package ltbs.uniform

import scala.language.higherKinds
import izumi.reflect.{Tag, TagK}
import validation.Rule
import scala.collection.immutable.{Map => IMap}

trait Uniform[-R <: Needs[_, _], -T, +A] {
  def map[F[_], B](f: A => B): Uniform[R, T, B] = Uniform.Map(this, f)
  def flatMap[R1 <: R, B, T1 <: T](f: A => Uniform[R1, T1, B]): Uniform[R1, T1, B] = Uniform.FlatMap(this, f)

  /** Returns None unless the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") when user.isEmployed
    * }}}
    */
  def when(predicate: => Boolean): Uniform[R, T, Option[A]] = 
    if (predicate) this.map(x => Some(x)) else Uniform.Pure[Option[A]](None: Option[A])

  /** Returns None unless the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") when ask[Boolean]("employed")
    * }}}
    */
  def when[R1 <: R, T1 <: T](wmb: Uniform[R1, T1, Boolean]): Uniform[R1, T1, Option[A]] = for {
    opt <- wmb
    ret <- if (opt) this map (x => Some(x)) else Uniform.Pure[Option[A]](None: Option[A])
  } yield ret

  /** Returns None when the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") unless user.isExempt
    * }}}
    */
  def unless(predicate: => Boolean): Uniform[R, T, Option[A]] = when(!predicate)

  /** Returns None when the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") unless ask[Boolean]("is-exempt")
    * }}}
    */
  def unless[R1 <: R, T1 <: T](wmb: Uniform[R1, T1, Boolean]): Uniform[R1, T1, Option[A]] =
    when[R1, T1](wmb.map(x => !x))

  /** Returns monoid empty unless the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") emptyUnless user.isEmployed
    * }}}
    */
  def emptyUnless[B >: A](predicate: => Boolean)(implicit mon: cats.Monoid[B]): Uniform[R, T, B] = this.map{ v =>
    if (predicate) v else mon.empty
  }

  /** Returns monoid empty unless the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") emptyUnless ask[Boolean]("employed")
    * }}}
    */
  def emptyUnless[B >: A, T1 <: T, R1 <: R](wmb: Uniform[R1, T1, Boolean])(implicit mon: cats.Monoid[B]): Uniform[R1, T1, B] = for {
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
  def emptyWhen[B >: A](predicate: => Boolean)(implicit mon: cats.Monoid[B]): Uniform[R, T, B] =
    emptyUnless[B](!predicate)

  /** Returns monoid empty when the predicate given is true, will short
    * circuit if possible.
    *
    * {{{
    * ask[Salary]("salary") emptyWhen ask[Boolean]("is-exempt")
    * }}}
    */
  def emptyWhen[B >: A, T1 <: T, R1 <: R](wmb: Uniform[R1, T1, Boolean])(implicit mon: cats.Monoid[B]): Uniform[R1, T1, B] = emptyUnless[B, T1, R1](wmb map (x => !x))

}

object Uniform {
  case class Map[-R <: Needs[_, _], T, A, +B](base: Uniform[R, T, A], f: A => B) extends Uniform[R, T, B]

  case class FlatMap[R <: Needs[_, _], -R2 <: R, T, A, +B](base: Uniform[R, T, A], f: A => Uniform[R2, T, B]) extends Uniform[R2, T, B]
  case class Interact[T, A](
    key: String,
    value: T,
    default: Option[A],
    validation: Rule[A],
    customContent: IMap[String,(String,List[Any])],
    tellTag: Tag[T],
    askTag: Tag[A]
  ) extends Uniform[Needs.Interact[T, A], T, A] {
    def customContent(from: String, to: String, args: Any*): Interact[T, A] = this.copy (
      customContent = customContent + ((from,(to, args.toList)))
    )
  }

  case class End[T](
    key: String,
    value: T,
    customContent: IMap[String,(String,List[Any])],
    tag: Tag[T]
  ) extends Uniform[Needs.Interact[T,Nothing], T, Nothing] {
    def customContent(from: String, to: String, args: Any*): End[T] = this.copy (
      customContent = customContent + ((from,(to, args.toList)))
    )
  }

  case class Pure[A](value: A) extends Uniform[Needs[_, _], Any, A]

  case class Subjourney[-R <: Needs[_, _], T, A](
    path: List[String],
    base: Uniform[R, T, A]
  ) extends Uniform[R, T, A]

  case class ListOf[-R <: Needs[_, _], A](
    key: String,
    base: (Option[Int], List[A]) => Uniform[R, Unit, A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: IMap[String,(String,List[Any])],
    tag: Tag[A]
  ) extends Uniform[R with Needs.AskList[A], Any, List[A]]

  object ListOf {

    sealed trait ListAction
    sealed trait ListActionRow extends ListAction
    sealed trait ListActionGeneral extends ListAction

    object ListAction {
      case class Delete(index: Int) extends ListActionRow
      case class Edit(index: Int) extends ListActionRow
      case object Continue extends ListActionGeneral
      case object Add extends ListActionGeneral
    }
  }

  case class Convert[F[_], A](
    key: String, 
    action: () => F[A],
    tagF: TagK[F],
    tagA: Tag[A]    
  ) extends Uniform[Needs.Convert[F, A], Unit, A]

  // R -> Uniform Type (asks, tells and converts)
  // A -> Ask type
  // T -> Tell type
  implicit def uniformMonadInstance[R <: Needs[_, _], T]: cats.Monad[Uniform[R, T, *]] =
    new cats.StackSafeMonad[Uniform[R, T, *]] {
      def pure[A](x: A): Uniform[R,T,A] = Uniform.Pure(x)
      def flatMap[A, B](fa: Uniform[R,T,A])(f: A => Uniform[R,T,B]): Uniform[R,T,B] =
        Uniform.FlatMap(fa, f)
    }

}
