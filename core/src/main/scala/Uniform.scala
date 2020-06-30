package ltbs.uniform

import scala.language.higherKinds
import izumi.reflect.{Tag, TagK}
import validation.Rule
import scala.collection.immutable.{Map => IMap}

trait Uniform[-R <: Needs[_], +A, -T] {
  def map[F[_], B](f: A => B): Uniform[R, B, T] = Uniform.Map(this, f)
  def flatMap[R1 <: R, B, T1 <: T](f: A => Uniform[R1, B, T1]): Uniform[R1, B, T1] = Uniform.FlatMap(this, f)

  def unless(predicate: => Boolean): Uniform[R, Option[A], T] = this.map{ v => 
    if (predicate) Some(v) else None
  }

  def emptyUnless[B >: A](predicate: => Boolean)(implicit mon: cats.Monoid[B]): Uniform[R, B, T] = this.map{ v => 
    if (predicate) v else mon.empty
  }
  
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

}
