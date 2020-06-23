package ltbs.uniform

sealed trait Needs[+A]

object Needs {
  trait Tell[T] extends Needs[T]
  trait Ask[T] extends Needs[T]
  trait Convert[F] extends Needs[F]
}
