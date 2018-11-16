package ltbs.uniform

import scala.util._

package object interpreters {

  /** for compatibility with 2.12 */
  implicit class RichTry[A](in: Try[A]) {
    def toEither: Either[Throwable, A] = in match {
      case Success(s) => Right(s)
      case Failure(e) => Left(e)
    }
  }

}
