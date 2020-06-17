package ltbs.uniform
package interpreters.logictable

import cats.implicits._
import ltbs.uniform.validation._

object LogicTableInterpreter extends MonadInterpreter[Either[ErrorTree, +?], SampleData, TellRenderer] {

  override def ask[A](key: String, default: Option[A], validation: Rule[A], asker: SampleData[A]): Either[ErrorTree, A] = {
    validation.either(asker(key).head)
  }

  override def tell[T](key: String, value: T, teller: TellRenderer[T]):Either[ErrorTree, Unit] =
    Right(())

  override def end(key: String): Either[ErrorTree, Nothing] =
    Left(ErrorMsg(s"$key (end)").toTree)

}
