package ltbs.uniform
package interpreters.logictable

import cats.implicits._, cats.data._
import ltbs.uniform.validation._

object LogicTableInterpreter extends MonadInterpreter[Logic, LTInteraction, SampleListQty] {

  def monadInstance  = implicitly[cats.Monad[Logic]]

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => Logic[A],
    deleteJourney: (Int, List[A]) => Logic[Boolean],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],
    qty: SampleListQty[A] 
  ): Logic[List[A]] = {
    val r: List[Logic[A]] = (1 to qty.value).toList.map(_ => askJourney(None, Nil))
    r.sequence
  }

  def interactImpl[T, A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String, List[Any])],
    interaction: LTInteraction[T,A]
  ): Logic[A] = EitherT {
    WriterT {
      interaction.askRenderer(key).map { sample =>
        (
          s"$key ask: ${sample.toString}" :: Nil,
          validation.either(sample)
        )
      }
    }
  }
}
