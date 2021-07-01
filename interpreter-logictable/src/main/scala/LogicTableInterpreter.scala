package ltbs.uniform
package interpreters.logictable

import cats.implicits._
import ltbs.uniform.validation._

object LogicTableInterpreter extends MonadInterpreter[Either[ErrorTree, +?], LTInteraction, SampleData] {

  def monadInstance = implicitly[cats.Monad[Either[ErrorTree, +?]]]

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => Either[ErrorTree,A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],asker: SampleData[A]
  ): Either[ErrorTree,List[A]] = validation.either(asker(key))

  def interactImpl[T, A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String, List[Any])],
    interaction: LTInteraction[T,A]
  ) = validation.either(interaction.askRenderer(key).head)
}
