package ltbs.uniform

import cats.implicits._
import cats.Monad
import util._

trait ScalaVersionCompatibility {
  implicit def catsSyntaxEither = cats.implicits.catsSyntaxEither _

  implicit class RichEither[A,B](in: Either[A,B]) {
    def toOption: Option[B] = in match {
      case Right(b) => Some(b)
      case _        => None
    }
  }

  implicit class RichTry[A](in: Try[A]) {
    def toEither: Either[Throwable, A] = in match {
      case Failure(e) => Left(e)
      case Success(a) => Right(a)
    }
  }
}
