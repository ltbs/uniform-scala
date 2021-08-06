package ltbs.uniform
package interpreters

import cats.data._

package object logictable {
  type Logic[A] = EitherT[WriterT[List, List[String], ?],ErrorTree,A]

  implicit val unitTell = new TellRenderer[Unit] {
    def apply(key: String, value: Unit): List[String] =
      Nil
  }

  implicit val unitSample = new SampleData[Unit] {
    def apply(key: String): List[Unit] = List(())
  }

  implicit def automaticTell[A] = new TellRenderer[A] {
    def apply(key: String, value: A): List[String] =
      List(value.toString)
  }

}
