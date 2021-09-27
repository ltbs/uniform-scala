package ltbs.uniform

import scala.language.higherKinds
import cats.implicits._
import validation.Rule

trait RecoverableMonadInterpreter[F[_], INTERACTTC[_,_], ASKLISTTC[_]]
    extends MonadInterpreter[F, INTERACTTC, ASKLISTTC]
{

  protected def interactRecoverable[T,A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],
    interaction: INTERACTTC[T,A],
    error: ErrorTree
  ): F[A]

  protected def interactImpl[T,A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],
    interaction: INTERACTTC[T,A]
  ): F[A] = 
    monadInstance.tailRecM(ErrorTree.empty)(error =>
      interactRecoverable(
        key,
        tellValue,
        default,
        validation,
        customContent,
        interaction,
        error
      ).map(validation.apply(_).toEither)
    )
}
