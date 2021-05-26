package ltbs.uniform
package validation

import cats.NotNull
import cats.data.Validated
import scala.reflect.ClassTag

object Transformation {
  final class CatchOnlyPartiallyApplied[T >: Null <: Throwable](val errorMsg: String) extends AnyVal {
    def apply[A,B](f: A => B)(implicit CT: ClassTag[T], NT: NotNull[T]): Transformation[A, B] = {(a: A) => 
      Validated.catchOnly[T](f(a))
        .leftMap(_ => ErrorMsg(errorMsg).toTree)
    }
  }

  def catchOnly[T >: Null <: Throwable](errorMsg: String): CatchOnlyPartiallyApplied[T] =
    new CatchOnlyPartiallyApplied[T](errorMsg: String)
}
