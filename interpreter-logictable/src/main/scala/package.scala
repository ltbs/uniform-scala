package ltbs.uniform
package interpreters

import cats.implicits._
import cats.data._
import com.github.ghik.silencer.silent

package object logictable {
  type Logic[A] = EitherT[WriterT[List, List[String], ?],ErrorTree,A]

  val r = implicitly[cats.Monad[Logic]]

  implicit val unitTell = new TellRenderer[Unit] {
    def apply(key: String, value: Unit): List[String] =
      Nil
  }

  implicit val unitSample = new SampleData[Unit] {
    def apply(key: String): List[Unit] = List(())
  }

  implicit def automaticTell[A](
    implicit @silent lp: shapeless.LowPriority
  ) = new TellRenderer[A] {
    def apply(key: String, value: A): List[String] =
      List(value.toString)
  }
 
}
