package ltbs.uniform

import com.github.ghik.silencer.silent
import util._

@silent("Unused import") trait ScalaVersionCompatibility {

  import cats.implicits._
  import cats.Monad

  implicit class RichTry[A](in: Try[A]) {
    def toEither: Either[Throwable, A] = in match {
      case Failure(e) => Left(e)
      case Success(a) => Right(a)
    }
  }
}
