package ltbs.uniform

import scala.language.higherKinds

import cats.implicits._
import validation.Rule

trait MonadErrorInterpreter[F[_], E, INTERACTTC[_,_], ASKLISTTC[_]]
    extends RecoverableMonadInterpreter[F, INTERACTTC, ASKLISTTC]
{

  def toErrorTree: PartialFunction[E, ErrorTree]

  implicit def monadErrorInstance: cats.MonadError[F, E]
  implicit def monadInstance: cats.Monad[F] = monadErrorInstance

  override protected def interactImpl[T,A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],
    interaction: INTERACTTC[T,A]
  ): F[A] = 
    monadInstance.tailRecM(ErrorTree.empty){error =>

      val ir: F[A] = interactRecoverable(
        key,
        tellValue,
        default,
        validation,
        customContent,
        interaction,
        error
      )

      ir.map(validation.apply(_).toEither)
        .recover(toErrorTree.andThen(_.asLeft[A]))
    }

}
