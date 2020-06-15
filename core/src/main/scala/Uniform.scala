package ltbs.uniform

import scala.language.higherKinds
import izumi.reflect.Tag

trait Uniform[-R <: Needs[_], +A] {
  def map[F[_], B](f: A => B): Uniform[R, B] = Uniform.Map(this, f)
  def flatMap[R1 <: R, B](f: A => Uniform[R1, B]): Uniform[R1, B] = Uniform.FlatMap(this, f)
}

object Uniform {
  case class Map[-R <: Needs[_], A, +B](base: Uniform[R, A], f: A => B) extends Uniform[R, B]
  case class FlatMap[R <: Needs[_], -R2 <: R, A, +B](base: Uniform[R, A], f: A => Uniform[R2, B]) extends Uniform[R2, B]
  case class Interact[A, T](key: String, value: T, askTag: Tag[A], tellTag: Tag[T]) extends Uniform[Needs.Ask[A] with Needs.Tell[T], A]
  case class Tell[A](key: String, value: A, tag: Tag[A]) extends Uniform[Needs.Tell[A], Unit]
  case class Ask[A](key: String, tag: Tag[A]) extends Uniform[Needs.Ask[A], A]
  case class EndTell[A](key: String, value: A, tag: Tag[A]) extends Uniform[Needs.Tell[A], Nothing]
  case class End[A](key: String) extends Uniform[Needs[Any], Nothing]
  case class Pure[A](value: A) extends Uniform[Needs[_], A]
}


