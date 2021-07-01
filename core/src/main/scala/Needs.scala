package ltbs.uniform

import scala.language.higherKinds

sealed trait Needs[T, +A]

object Needs {

  trait Interact[T, A] extends Needs[T, A]
  trait AskList[A] extends Needs[Unit, A]
  trait Convert[F[_], A] extends Needs[Unit, F[A]]
  
}
