package ltbs.uniform

import scala.language.higherKinds
import izumi.reflect.Tag
import validation.Rule

trait Uniform[-R <: Needs[_], +A, -T] {
  def map[F[_], B](f: A => B): Uniform[R, B, T] = Uniform.Map(this, f)
  def flatMap[R1 <: R, B, T1 <: T](f: A => Uniform[R1, B, T1]): Uniform[R1, B, T1] = Uniform.FlatMap(this, f)
}

object Uniform {
  case class Map[-R <: Needs[_], A, +B, T](base: Uniform[R, A, T], f: A => B) extends Uniform[R, B, T]
  case class FlatMap[R <: Needs[_], -R2 <: R, A, +B, T](base: Uniform[R, A, T], f: A => Uniform[R2, B, T]) extends Uniform[R2, B, T]
  case class Interact[A, T](key: String, value: T, default: Option[A], validation: Rule[A], askTag: Tag[A], tellTag: Tag[T]) extends Uniform[Needs.Ask[A] with Needs.Tell[T], A, T]
  case class Tell[A](key: String, value: A, tag: Tag[A]) extends Uniform[Needs.Tell[A], Unit, A]
  case class Ask[A](key: String, default: Option[A], validation: Rule[A], tag: Tag[A]) extends Uniform[Needs.Ask[A], A, Unit]
  case class EndTell[A](key: String, value: A, tag: Tag[A]) extends Uniform[Needs.Tell[A], Nothing, A]
  case class End[A](key: String) extends Uniform[Needs[Any], Nothing, Unit]
  case class Pure[A](value: A) extends Uniform[Needs[_], A, Any]
  case class Subjourney[-R <: Needs[_], A, T](path: List[String], base: Uniform[R, A, T]) extends Uniform[R, A, T]
}


