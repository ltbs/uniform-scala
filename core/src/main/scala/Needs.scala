package ltbs.uniform

import scala.language.higherKinds

sealed trait Needs[+A]

object Needs {
  trait Tell[T] extends Needs[T]
  trait Ask[T] extends Needs[T]
  trait AskList[T] extends Needs[T]  
  trait Convert[F[_], A] extends Needs[F[A]]
}
