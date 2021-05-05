package ltbs.uniform
package interpreters.logictable

import cats.implicits._
import ltbs.uniform.validation._

object LogicTableInterpreter extends MonadInterpreter[Either[ErrorTree, +?], SampleData, TellRenderer, SampleData] {

  def monadInstance = implicitly[cats.Monad[Either[ErrorTree, +?]]]

  override def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String, List[Any])],
    asker: SampleData[A]
  ): Either[ErrorTree, A] = validation.either(asker(key).head)

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => Either[ErrorTree,A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],asker: SampleData[A]
  ): Either[ErrorTree,List[A]] = validation.either(asker(key))

  override def tellImpl[T](
    key: String,
    value: T,
    customContent: Map[String,(String, List[Any])],
    teller: TellRenderer[T]
  ):Either[ErrorTree, Unit] = Right(())

  override def endImpl(
    key: String,
    customContent: Map[String,(String, List[Any])]
  ): Either[ErrorTree,Nothing] =
    Left(ErrorMsg(s"$key (end)").toTree)
}
